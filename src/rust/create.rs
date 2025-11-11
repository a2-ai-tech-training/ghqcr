use extendr_api::{deserializer::from_robj, prelude::*, Robj};
use ghqctoolkit::{
    create_labels_if_needed, Checklist, GitFileOps, GitHubWriter, GitRepository, QCIssue,
};
use octocrab::models::Milestone;
use serde::Deserialize;

use crate::{
    configuration::RChecklist,
    git_utils::RRepoUser,
    utils::{get_cached_git_info, get_disk_cache, get_rt},
};

extendr_module! {
    mod create;
    fn create_issues_impl;
}

#[derive(Debug, Deserialize)]
struct RFileData {
    name: String,
    assignees: Vec<RRepoUser>,
    checklist: RChecklist,
}

#[extendr]
fn create_issues_impl(
    milestone_name: &str,
    description: Nullable<String>,
    file_data_robj: Robj,
    milestones_robj: Robj,
    prepended_checklist_note: Nullable<String>,
    working_dir: &str,
) -> Result<String> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();
    let cache = get_disk_cache(git_info);

    let rt = get_rt();

    // Deserialize milestones
    let milestones: Vec<Milestone> = from_robj(&milestones_robj)?;

    // Deserialize file data
    let file_data_vec: Vec<RFileData> = from_robj(&file_data_robj)?;

    let prepended_checklist_note = prepended_checklist_note.into_option();

    let (milestone_id, milestone_url) = if let Some(m) =
        milestones.iter().find(|m| m.title == milestone_name)
    {
        (m.number as u64, m.html_url.to_string())
    } else {
        if let Err(e) = rt.block_on(create_labels_if_needed(
            cache.as_ref(),
            git_info.branch().ok().as_ref().map(|s| s.as_str()),
            git_info,
        )) {
            log::warn!("Failed to create labels as needed: {e}. Continuing...");
        }

        match rt.block_on(git_info.create_milestone(milestone_name, &description.into_option())) {
            Ok(m) => (m.number as u64, m.html_url.to_string()),
            Err(e) => {
                return Err(Error::Other(format!(
                    "Failed to create milestone '{milestone_name}': {e}"
                )))
            }
        }
    };

    let mut successes = Vec::new();
    let mut errors = Vec::new();
    let mut successful_qc_issues = Vec::new();

    // First pass: Create QC issues and collect errors/successes
    for (index, file_data) in file_data_vec.into_iter().enumerate() {
        let file_name = file_data.name.clone();
        match create_qc_issue(milestone_id, file_data, &prepended_checklist_note, git_info) {
            Ok(qc_issue) => {
                successful_qc_issues.push((index, file_name, qc_issue));
            }
            Err(e) => errors.push((
                index,
                format!("❌ **{file_name}**: Failed to create QC issue - {e}"),
            )),
        }
    }

    // Second pass: Post QC issues in batches of 2, retry failures sequentially
    // Try to go as fast as possible, without too much rate limiting
    if !successful_qc_issues.is_empty() {
        let mut retry_queue = Vec::new();

        // Process in batches of 2
        for batch in successful_qc_issues.chunks(2) {
            let batch_futures = batch.iter().map(|(_, _, issue)| git_info.post_issue(issue));
            let batch_results = rt.block_on(futures::future::join_all(batch_futures));

            // Process batch results
            for ((index, file_name, issue), result) in batch.iter().zip(batch_results) {
                match result {
                    Ok(url) => successes.push((*index, format!("✅ [**{file_name}**]({url})"))),
                    Err(e) => {
                        log::debug!(
                            "Initial post failed for {file_name}: {e}. Adding to retry queue."
                        );
                        retry_queue.push((*index, file_name.clone(), issue));
                    }
                }
            }

            // Small delay between batches
            if successful_qc_issues.len() > 2 {
                std::thread::sleep(std::time::Duration::from_millis(100));
            }
        }

        // Retry failed issues one at a time with longer delays
        if !retry_queue.is_empty() {
            log::debug!("Retrying {} failed issues sequentially", retry_queue.len());
            for (index, file_name, issue) in retry_queue {
                std::thread::sleep(std::time::Duration::from_millis(500)); // Longer delay for retries

                match rt.block_on(git_info.post_issue(issue)) {
                    Ok(url) => {
                        log::debug!("Retry successful for {file_name}");
                        successes.push((index, format!("✅ [**{file_name}**]({url})")));
                    }
                    Err(e) => {
                        log::warn!("Retry failed for {file_name}: {e}");
                        errors.push((
                            index,
                            format!("❌ **{file_name}**: Failed to post issue - {e}"),
                        ));
                    }
                }
            }
        }
    }

    // Sort results by original file order
    successes.sort_by_key(|(index, _)| *index);
    errors.sort_by_key(|(index, _)| *index);

    // Check if we have successes/errors before consuming them
    let has_successes = !successes.is_empty();
    let has_errors = !errors.is_empty();

    // Create a formatted result string for the modal
    let mut result_parts = vec![format!(
        "#### [Click here to view Milestone on GitHub]({milestone_url})"
    )];

    if has_successes {
        let success_messages: Vec<String> =
            successes.into_iter().map(|(_, message)| message).collect();
        result_parts.push(format!(
            "## Successfully Created ({}):\n{}",
            success_messages.len(),
            success_messages.join("\n\n")
        ));
    }

    if has_errors {
        let error_messages: Vec<String> = errors.into_iter().map(|(_, message)| message).collect();
        result_parts.push(format!(
            "## Failed ({}):\n{}",
            error_messages.len(),
            error_messages.join("\n\n")
        ));
    }

    let result_string = result_parts.join("\n\n\n");

    // Only return an error if ALL files failed
    if !has_successes && has_errors {
        return Err(Error::Other(format!(
            "All QC issue creation failed:\n{}",
            result_string
        )));
    }

    Ok(result_string)
}

fn create_qc_issue(
    milestone_id: u64,
    file_data: RFileData,
    prepended_checklist_note: &Option<String>,
    git_info: &(impl GitHubWriter + GitFileOps + GitRepository),
) -> Result<QCIssue> {
    let checklist = Checklist::new(
        file_data.checklist.name,
        prepended_checklist_note.clone(),
        file_data.checklist.content,
    );

    let assignees = file_data
        .assignees
        .into_iter()
        .map(|r| r.login)
        .collect::<Vec<_>>();

    QCIssue::new(
        &file_data.name,
        git_info,
        milestone_id,
        assignees,
        Vec::new(),
        checklist,
    )
    .map_err(|e| Error::Other(format!("{e}")))
}
