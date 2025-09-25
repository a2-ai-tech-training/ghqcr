use std::{
    borrow::Cow,
    future::{Future, IntoFuture},
    path::{Path, PathBuf},
};

use extendr_api::{deserializer::from_robj, prelude::*, serializer::to_robj, Robj};
use ghqctoolkit::{
    create_labels_if_needed, get_repo_users, Checklist, DiskCache, GitFileOps, GitHubReader,
    GitHubWriter, GitRepository, QCIssue, RelevantFile, RepoUser,
};
use gix::hashtable::hash_map::HashMap;
use octocrab::models::Milestone;
use serde::Deserialize;

use crate::{configuration::RChecklist, get_disk_cache, get_rt};

pub fn get_milestones_impl(git_info: &impl GitHubReader) -> Result<Vec<Robj>> {
    let rt = get_rt();
    let milestones = rt
        .block_on(git_info.get_milestones())
        .map_err(|e| Error::Other(format!("Failed to get milestones: {e}")))?;
    milestones.iter().map(to_robj).collect::<Result<Vec<_>>>()
}

pub fn get_milestone_issues_impl(
    git_info: &impl GitHubReader,
    milestone: &Robj,
) -> Result<Vec<Robj>> {
    let milestone = from_robj(milestone)?;
    let rt = get_rt();
    let issues = rt
        .block_on(git_info.get_milestone_issues(&milestone))
        .map_err(|e| {
            Error::Other(format!(
                "Failed to retrieve issues from milestone {}: {e}",
                milestone.number
            ))
        })?;

    issues.iter().map(to_robj).collect::<Result<Vec<_>>>()
}

#[derive(Debug, Clone, PartialEq, IntoDataFrameRow, Deserialize)]
struct RRepoUser {
    login: String,
    name: Option<String>,
}

pub fn get_users_impl(git_info: &(impl GitHubReader + GitRepository)) -> Robj {
    let rt = get_rt();
    let cache = get_disk_cache(git_info);
    let repo_users = match rt.block_on(get_repo_users(cache.as_ref(), git_info)) {
        Ok(repo_users) => repo_users,
        Err(e) => {
            log::warn!("Could not determine repo users: {e}. Defaulting to none...");
            Vec::new()
        }
    };

    let r_repo_users = repo_users
        .into_iter()
        .map(|r| RRepoUser {
            login: r.login,
            name: r.name,
        })
        .collect::<Vec<_>>();

    match r_repo_users.into_dataframe() {
        Ok(df) => df.into(),
        Err(e) => {
            log::warn!("Could not convert repo users to R dataframe: {e}. Defaulting to none...");
            Robj::default()
        }
    }
}

#[derive(Debug, Deserialize)]
struct RFileData {
    name: String,
    assignees: Vec<RRepoUser>,
    checklist: RChecklist,
}

pub fn create_issues_impl(
    milestone_name: &str,
    description: Nullable<String>,
    file_data_robj: Robj,
    milestones_robj: Robj,
    prepended_checklist_note: Nullable<String>,
    git_info: &(impl GitHubWriter + GitHubReader + GitFileOps + GitRepository),
    cache: Option<&DiskCache>,
) -> Result<String> {
    let rt = get_rt();

    // Deserialize milestones
    let milestones: Vec<Milestone> = from_robj(&milestones_robj)?;

    // Deserialize file data
    let file_data_vec: Vec<RFileData> = from_robj(&file_data_robj)?;

    let prepended_checklist_note = prepended_checklist_note.into_option();

    let (milestone_id, milestone_url) = if let Some(m) =
        milestones.iter().find(|m| m.title == milestone_name)
    {
        (m.number as u64, m.url.to_string())
    } else {
        if let Err(e) = rt.block_on(create_labels_if_needed(
            cache,
            git_info.branch().ok().as_ref().map(|s| s.as_str()),
            git_info,
        )) {
            log::warn!("Failed to create labels as needed: {e}. Continuing...");
        }

        match rt.block_on(git_info.create_milestone(milestone_name, &description.into_option())) {
            Ok(m) => (m.number as u64, m.url.to_string()),
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
    for file_data in file_data_vec {
        let file_name = file_data.name.clone();
        match create_qc_issue(milestone_id, file_data, &prepended_checklist_note, git_info) {
            Ok(qc_issue) => {
                successful_qc_issues.push((file_name, qc_issue));
            }
            Err(e) => errors.push(format!(
                "❌ **{}**: Failed to create QC issue - {}",
                file_name, e
            )),
        }
    }

    // Second pass: Post all successful QC issues in parallel
    if !successful_qc_issues.is_empty() {
        let post_futures = successful_qc_issues
            .iter()
            .map(|(_, issue)| git_info.post_issue(issue));
        let post_results = rt.block_on(futures::future::join_all(post_futures));

        // Process posting results
        for ((file_name, _), post_result) in successful_qc_issues.iter().zip(post_results) {
            match post_result {
                Ok(url) => successes.push(format!("✅ **{}**: {}", file_name, url)),
                Err(e) => errors.push(format!(
                    "❌ **{}**: Failed to post issue - {}",
                    file_name, e
                )),
            }
        }
    }

    // Create a formatted result string for the modal
    let mut result_parts = vec![format!(
        "#### [Click here to view Milestone on GitHub]({milestone_url})"
    )];

    if !successes.is_empty() {
        result_parts.push(format!(
            "## Successfully Created ({}):\n{}",
            successes.len(),
            successes.join("\n\n")
        ));
    }

    if !errors.is_empty() {
        result_parts.push(format!(
            "## Failed ({}):\n{}",
            errors.len(),
            errors.join("\n\n")
        ));
    }

    let result_string = result_parts.join("\n\n\n");

    // Only return an error if ALL files failed
    if successes.is_empty() && !errors.is_empty() {
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
    log::debug!("Assigning {} to issue.", assignees.join(", "));

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
