use std::{collections::HashMap, path::PathBuf, str::FromStr};

use extendr_api::{deserializer::from_robj, prelude::*, serializer::to_robj, Robj};
use ghqctoolkit::{GitFileOps, GitHelpers, GitHubReader, GitHubWriter, QCComment};
use gix::ObjectId;
use octocrab::models::{issues::Issue, Milestone};
use serde::{Deserialize, Serialize};

use crate::utils::get_rt;

// R-safe version of QCComment with string hashes instead of ObjectId
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RQCComment {
    pub file: String,
    pub issue: Issue,
    pub current_commit: String,   // String instead of ObjectId
    pub previous_commit: Option<String>, // String instead of ObjectId
    pub note: Option<String>,
    pub no_diff: bool,
}

impl RQCComment {
    // Convert to actual QCComment for body generation
    pub fn to_qc_comment(&self) -> Result<QCComment> {
        let current_commit = ObjectId::from_str(&self.current_commit)
            .map_err(|e| Error::Other(format!("Invalid current commit: {}", e)))?;

        let previous_commit = if let Some(ref prev) = self.previous_commit {
            Some(ObjectId::from_str(prev)
                .map_err(|e| Error::Other(format!("Invalid previous commit: {}", e)))?)
        } else {
            None
        };

        Ok(QCComment {
            file: PathBuf::from(&self.file),
            issue: self.issue.clone(),
            current_commit,
            previous_commit,
            note: self.note.clone(),
            no_diff: self.no_diff,
        })
    }
}

pub fn get_multiple_milestone_issues_impl(
    git_info: &impl GitHubReader,
    milestones_robj: &Robj,
) -> Result<Robj> {
    let rt = get_rt();

    // Deserialize milestones from R
    let milestones: Vec<Milestone> = from_robj(milestones_robj)?;

    // Create futures for parallel execution
    let futures: Vec<_> = milestones
        .iter()
        .map(|milestone| {
            let milestone_title = milestone.title.clone();
            async move {
                let issues = git_info.get_milestone_issues(milestone).await;
                (milestone_title, issues)
            }
        })
        .collect();

    // Execute all futures in parallel
    let results = rt.block_on(futures::future::join_all(futures));

    // Build the result hashmap, only including successful results
    let mut milestone_issues = HashMap::new();

    for (milestone_title, issues_result) in results {
        match issues_result {
            Ok(issues) => {
                // Convert issues to R objects
                let r_issues: Result<Vec<Robj>> = issues.iter().map(to_robj).collect();
                match r_issues {
                    Ok(issues_vec) => {
                        milestone_issues.insert(milestone_title, issues_vec);
                    }
                    Err(e) => {
                        log::warn!("Failed to convert issues for milestone {}: {}", milestone_title, e);
                        // Don't insert entry for this milestone on conversion error
                    }
                }
            }
            Err(e) => {
                log::warn!("Failed to get issues for milestone {}: {}", milestone_title, e);
                // Don't insert entry for this milestone on API error
            }
        }
    }

    // Convert HashMap to R named list
    let mut names = Vec::new();
    let mut values: Vec<Robj> = Vec::new();

    for (name, issues) in milestone_issues {
        names.push(name);
        let issues_list = List::from_values(issues);
        values.push(issues_list.into());
    }

    let result_list = List::from_names_and_values(names, values)?;
    Ok(result_list.into())
}

pub fn create_qc_comment_impl(
    issue_robj: &Robj,
    filename: &str,
    from_commit: &str,
    to_commit: &str,
    message: Option<String>,
    show_diff: bool,
) -> Result<Robj> {
    // Deserialize the issue from R
    let issue: Issue = from_robj(issue_robj)
        .map_err(|e| Error::Other(format!("Failed to deserialize issue: {}", e)))?;

    // Validate commit hashes can be parsed to ObjectId (but don't store ObjectId)
    ObjectId::from_str(to_commit)
        .map_err(|e| Error::Other(format!("Invalid current commit hash '{}': {}", to_commit, e)))?;

    let previous_commit = if from_commit.is_empty() {
        None
    } else {
        ObjectId::from_str(from_commit).map_err(|e| {
            Error::Other(format!("Invalid previous commit hash '{}': {}", from_commit, e))
        })?;
        Some(from_commit.to_string())
    };

    // Create RQCComment struct with string hashes
    let r_qc_comment = RQCComment {
        file: filename.to_string(),
        issue,
        current_commit: to_commit.to_string(),
        previous_commit,
        note: message,
        no_diff: !show_diff, // no_diff is the inverse of show_diff
    };

    // Convert to Robj
    let robj = to_robj(&r_qc_comment)?;
    Ok(robj)
}

pub fn get_qc_comment_body_html_impl(
    r_qc_comment_robj: &Robj,
    git_info: &(impl GitHelpers + GitFileOps),
) -> Result<String> {
    // Deserialize RQCComment from Robj
    let r_qc_comment: RQCComment = from_robj(r_qc_comment_robj)?;

    // Convert RQCComment to QCComment for body generation
    let qc_comment = r_qc_comment.to_qc_comment()?;

    // Use std::panic::catch_unwind to handle panics gracefully
    let markdown_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        qc_comment.body(git_info)
    }));

    let markdown_body = match markdown_result {
        Ok(body) => body,
        Err(_) => {
            log::error!("Panic occurred in qc_comment.body(), using fallback");
            // Create a fallback body without diff
            format!(
                "# QC Notification\n\n## Metadata\n* current commit: {}\n* previous commit: {:?}\n\n**Note**: Could not generate file diff due to git repository access issues.",
                qc_comment.current_commit,
                qc_comment.previous_commit
            )
        }
    };

    // Convert markdown to HTML
    let html_body = markdown::to_html(&markdown_body);
    Ok(html_body)
}

pub fn post_qc_comment_impl(
    r_qc_comment_robj: &Robj,
    git_info: &(impl GitHelpers + GitFileOps + GitHubWriter),
) -> Result<String> {
    // Deserialize RQCComment from Robj
    let r_qc_comment: RQCComment = from_robj(r_qc_comment_robj)?;

    // Convert RQCComment to QCComment for posting
    let qc_comment = r_qc_comment.to_qc_comment()?;

    // Post the comment using the GitHubWriter trait
    let rt = get_rt();
    let result = rt.block_on(git_info.post_comment(&qc_comment))
        .map_err(|e| Error::Other(format!("Failed to post comment: {}", e)))?;

    Ok(result)
}