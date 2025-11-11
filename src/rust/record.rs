use std::{
    collections::HashMap,
    fmt,
    fs::File,
    io::Write,
    path::{absolute, PathBuf},
};

use extendr_api::{deserializer::from_robj, prelude::*, serializer::to_robj};
use ghqctoolkit::{
    fetch_milestone_issues, get_milestone_issue_information, record, render, Configuration,
    IssueInformation,
};
use octocrab::models::Milestone;

use crate::{
    utils::{get_cached_git_info, get_disk_cache, get_rt},
    ENV_PROVIDER,
};

extendr_module! {
    mod record;
    fn record_issue_modal_check_impl;
    fn get_milestone_issue_information_impl;
    fn generate_record_impl;
}

#[extendr]
fn get_milestone_issue_information_impl(milestones: Robj, working_dir: String) -> Result<Robj> {
    let milestones: Vec<Milestone> = from_robj(&milestones)?;
    let arc_git_info = get_cached_git_info(&working_dir)?;
    let git_info = arc_git_info.as_ref();
    let rt = get_rt();

    let milestone_issues = rt
        .block_on(fetch_milestone_issues(&milestones, git_info))
        .map_err(|e| {
            Error::Other(format!(
                "Failed to fetch issues for selected milestones: {e}"
            ))
        })?;

    let cache = get_disk_cache(git_info).as_ref();

    let milestone_issue_info = rt
        .block_on(get_milestone_issue_information(
            &milestone_issues,
            cache,
            git_info,
        ))
        .map_err(|e| {
            Error::Other(format!(
                "Failed to flatten issues to issue information: {e}"
            ))
        })?;

    let mut names = Vec::new();
    let mut values: Vec<Robj> = Vec::new();
    for (name, issues) in milestone_issue_info {
        names.push(name);
        let issue_robjs = issues.iter().map(to_robj).collect::<Result<Vec<Robj>>>()?;
        let issue_list = List::from_values(issue_robjs);
        values.push(issue_list.into());
    }

    Ok(List::from_names_and_values(names, values).into())
}

enum IssueProblems<'a> {
    NotApproved,
    OpenChecklists {
        summary: &'a str,
        checklist_name: &'a str,
    },
}

impl fmt::Display for IssueProblems<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotApproved => write!(f, "Not Approved"),
            Self::OpenChecklists {
                summary,
                checklist_name,
            } => {
                let mut chars = checklist_name.chars();
                let capitalized_name = match chars.next() {
                    None => "Checklists".to_string(),
                    Some(first_char) => {
                        first_char.to_uppercase().collect::<String>() + chars.as_str()
                    }
                };

                writeln!(f, "{capitalized_name} are not completed - {summary}",)
            }
        }
    }
}

#[extendr]
fn record_issue_modal_check_impl(milestone_issues: Robj, checklist_name: String) -> Result<String> {
    // Deserialize the Robj into HashMap<String, Vec<Issue>>
    let issues_map = deserialize_milestone_issues(milestone_issues)?;

    let mut res = Vec::new();
    for (milestone_title, issues) in &issues_map {
        let mut milestone_issue_strs = Vec::new();
        for issue in issues {
            let mut tags = Vec::new();
            if !issue.qc_status.contains("Approved") {
                tags.push(IssueProblems::NotApproved);
            }

            if !issue.checklist_summary.contains("100.0%") {
                tags.push(IssueProblems::OpenChecklists {
                    summary: &issue.checklist_summary,
                    checklist_name: &checklist_name,
                });
            }

            if !tags.is_empty() {
                milestone_issue_strs.push(format!(
                    "#### Issue #{}: {}\n- {}",
                    issue.number,
                    issue.title,
                    tags.iter()
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>()
                        .join("\n- ")
                ));
            }
        }

        if !milestone_issue_strs.is_empty() {
            milestone_issue_strs.sort();

            res.push(format!(
                "## {milestone_title} Issues\n{}",
                milestone_issue_strs.join("\n")
            ));
        }
    }

    Ok(res.join("\n\n"))
}

#[extendr]
fn generate_record_impl(
    milestones: Robj,
    milestone_issues: Robj,
    configuration: Robj,
    record_path: String,
    working_dir: String,
    just_tables: bool,
) -> Result<String> {
    let milestones: Vec<Milestone> = from_robj(&milestones)
        .map_err(|e| Error::Other(format!("Failed to deserialize milestones: {e}")))?;
    let milestone_issues = deserialize_milestone_issues(milestone_issues)
        .map_err(|e| Error::Other(format!("Failed to deserialize milestone issues: {e}")))?;
    let configuration = match from_robj(&configuration) {
        Ok(c) => c,
        Err(e) => {
            log::warn!("Failed to load configuration: {e}. Using default...");
            Configuration::default()
        }
    };

    let arc_git_info = get_cached_git_info(&working_dir)?;
    let git_info = arc_git_info.as_ref();

    let mut record_path = PathBuf::from(record_path);
    record_path.set_extension("pdf");
    let record_path = absolute(record_path)
        .map_err(|e| Error::Other(format!("Failed to make path absolute: {e}")))?;

    let record_str = record(
        &milestones,
        &milestone_issues,
        &configuration,
        git_info,
        &ENV_PROVIDER,
        just_tables,
    )
    .map_err(|e| Error::Other(format!("Failed to generate record markdown string: {e}")))?;

    // Write the markdown to a debug file in the working directory
    if let Err(e) = write_debug_markdown(&record_str, &working_dir) {
        log::warn!("Failed to write debug markdown file: {}", e);
    }

    render(&record_str, &record_path)
        .map_err(|e| Error::Other(format!("Failed to render record due to: {e}")))?;

    Ok(format!(
        "Successfully rendered record to {}",
        record_path.display()
    ))
}

/// Helper function to write debug markdown to working directory
fn write_debug_markdown(markdown_content: &str, working_dir: &str) -> std::io::Result<()> {
    let debug_file_path = PathBuf::from(working_dir).join("ghqc_record_debug.md");
    let mut file = File::create(&debug_file_path)?;
    file.write_all(markdown_content.as_bytes())?;

    println!("Debug markdown written to: {}", debug_file_path.display());
    Ok(())
}

/// Helper function to deserialize the milestone issues Robj into HashMap<String, Vec<Issue>>
fn deserialize_milestone_issues(
    milestone_issues: Robj,
) -> Result<HashMap<String, Vec<IssueInformation>>> {
    // First, convert the Robj to a List to access its named elements
    let list = List::try_from(milestone_issues)
        .map_err(|e| Error::Other(format!("Failed to convert Robj to List: {}", e)))?;

    let mut result = HashMap::new();

    // Iterate through the named list
    for (name, value) in list.iter() {
        let milestone_title = name.to_string();

        // Convert the value (which should be a list of Issue Robjs) to Vec<Issue>
        let issues_list = List::try_from(value.clone()).map_err(|e| {
            Error::Other(format!(
                "Failed to convert issues to List for milestone '{}': {}",
                milestone_title, e
            ))
        })?;

        let mut issues = Vec::new();
        for issue_robj in issues_list.iter() {
            let issue: IssueInformation = from_robj(&issue_robj.1).map_err(|e| {
                Error::Other(format!(
                    "Failed to deserialize issue in milestone '{}': {}",
                    milestone_title, e
                ))
            })?;
            issues.push(issue);
        }

        result.insert(milestone_title, issues);
    }

    Ok(result)
}
