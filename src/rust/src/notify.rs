use std::{collections::HashMap, path::PathBuf, str::FromStr};

use extendr_api::{deserializer::from_robj, prelude::*, serializer::to_robj, Robj};
use ghqctoolkit::{GitHubReader, GitHubWriter, QCApprove, QCComment, QCUnapprove};
use gix::ObjectId;
use octocrab::models::{issues::Issue, Milestone};
use serde::{Deserialize, Serialize};

use crate::utils::{get_cached_git_info, get_rt};

extendr_module! {
    mod notify;
    fn get_multiple_milestone_issues_impl;
    fn create_qc_comment_impl;
    fn get_qc_comment_body_html_impl;
    fn post_qc_comment_impl;
    fn create_qc_approval_impl;
    fn get_qc_approval_body_html_impl;
    fn post_qc_approval_impl;
    fn create_qc_unapproval_impl;
    fn get_qc_unapproval_body_html_impl;
    fn post_qc_unapproval_impl;
}

// R-safe version of QCComment with string hashes instead of ObjectId
#[derive(Debug, Clone, Serialize, Deserialize)]
struct RQCComment {
    file: String,
    issue: Issue,
    current_commit: String,          // String instead of ObjectId
    previous_commit: Option<String>, // String instead of ObjectId
    note: Option<String>,
    no_diff: bool,
}

impl RQCComment {
    // Convert to actual QCComment for body generation
    fn to_qc_comment(&self) -> Result<QCComment> {
        let current_commit = ObjectId::from_str(&self.current_commit)
            .map_err(|e| Error::Other(format!("Invalid current commit: {}", e)))?;

        let previous_commit = if let Some(ref prev) = self.previous_commit {
            Some(
                ObjectId::from_str(prev)
                    .map_err(|e| Error::Other(format!("Invalid previous commit: {}", e)))?,
            )
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

#[extendr]
fn get_multiple_milestone_issues_impl(working_dir: &str, milestones_robj: Robj) -> Result<Robj> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();
    let rt = get_rt();

    // Deserialize milestones from R
    let milestones: Vec<Milestone> = from_robj(&milestones_robj)?;

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
                        log::warn!(
                            "Failed to convert issues for milestone {}: {}",
                            milestone_title,
                            e
                        );
                        // Don't insert entry for this milestone on conversion error
                    }
                }
            }
            Err(e) => {
                log::warn!(
                    "Failed to get issues for milestone {}: {}",
                    milestone_title,
                    e
                );
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

#[extendr]
fn create_qc_comment_impl(
    issue_robj: Robj,
    filename: &str,
    from_commit: &str,
    to_commit: &str,
    message: Nullable<String>,
    show_diff: bool,
) -> Result<Robj> {
    // Deserialize the issue from R
    let issue: Issue = from_robj(&issue_robj)
        .map_err(|e| Error::Other(format!("Failed to deserialize issue: {}", e)))?;

    // Validate commit hashes can be parsed to ObjectId (but don't store ObjectId)
    ObjectId::from_str(to_commit).map_err(|e| {
        Error::Other(format!(
            "Invalid current commit hash '{}': {}",
            to_commit, e
        ))
    })?;

    let previous_commit = if from_commit.is_empty() {
        None
    } else {
        ObjectId::from_str(from_commit).map_err(|e| {
            Error::Other(format!(
                "Invalid previous commit hash '{}': {}",
                from_commit, e
            ))
        })?;
        Some(from_commit.to_string())
    };

    // Create RQCComment struct with string hashes
    let r_qc_comment = RQCComment {
        file: filename.to_string(),
        issue,
        current_commit: to_commit.to_string(),
        previous_commit,
        note: message.into_option(),
        no_diff: !show_diff, // no_diff is the inverse of show_diff
    };

    // Convert to Robj
    let robj = to_robj(&r_qc_comment)?;
    Ok(robj)
}

#[extendr]
fn get_qc_comment_body_html_impl(r_qc_comment_robj: Robj, working_dir: &str) -> Result<String> {
    let git_info = get_cached_git_info(working_dir)?;

    // Deserialize RQCComment from Robj
    let r_qc_comment: RQCComment = from_robj(&r_qc_comment_robj)?;

    // Convert RQCComment to QCComment for body generation
    let qc_comment = r_qc_comment.to_qc_comment()?;

    let markdown_body = qc_comment.body(git_info.as_ref());

    // Convert markdown to HTML
    let html_body = markdown::to_html(&markdown_body);
    Ok(html_body)
}

#[extendr]
fn post_qc_comment_impl(r_qc_comment_robj: Robj, working_dir: &str) -> Result<String> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();

    // Deserialize RQCComment from Robj
    let r_qc_comment: RQCComment = from_robj(&r_qc_comment_robj)?;

    // Convert RQCComment to QCComment for posting
    let qc_comment = r_qc_comment.to_qc_comment()?;

    // Post the comment using the GitHubWriter trait
    let rt = get_rt();
    let result = rt
        .block_on(git_info.post_comment(&qc_comment))
        .map_err(|e| Error::Other(format!("Failed to post comment: {}", e)))?;

    Ok(result)
}

// R-safe version of QCApproval with string hashes instead of ObjectId
#[derive(Debug, Clone, Serialize, Deserialize)]
struct RQCApprove {
    pub file: String,
    pub commit: String, // String instead of ObjectId
    pub issue: Issue,
    pub note: Option<String>,
}

impl TryInto<QCApprove> for RQCApprove {
    type Error = extendr_api::Error;
    fn try_into(self) -> std::result::Result<QCApprove, Self::Error> {
        let commit = ObjectId::from_str(&self.commit)
            .map_err(|e| Error::Other(format!("Invalid approval commit: {e}")))?;

        Ok(QCApprove {
            file: PathBuf::from(self.file),
            commit,
            issue: self.issue,
            note: self.note,
        })
    }
}

#[extendr]
fn create_qc_approval_impl(
    issue_robj: Robj,
    filename: &str,
    approval_commit: &str,
    message: Nullable<String>,
) -> Result<Robj> {
    let issue: Issue = from_robj(&issue_robj)
        .map_err(|e| Error::Other(format!("Failed to deserialize issue: {}", e)))?;

    // Validate commit hashes can be parsed to ObjectId (but don't store ObjectId)
    ObjectId::from_str(approval_commit).map_err(|e| {
        Error::Other(format!(
            "Invalid approval commit hash '{}': {}",
            approval_commit, e
        ))
    })?;

    let r_qc_approval = RQCApprove {
        file: filename.to_string(),
        commit: approval_commit.to_string(),
        issue,
        note: message.into_option(),
    };

    to_robj(&r_qc_approval)
}

#[extendr]
fn get_qc_approval_body_html_impl(r_qc_approval_robj: Robj, working_dir: &str) -> Result<String> {
    let git_info = get_cached_git_info(working_dir)?;

    // Deserialize RQCApprove from Robj
    let r_qc_approve: RQCApprove = from_robj(&r_qc_approval_robj)?;

    // Convert RQCComment to QCComment for body generation
    let qc_approve: QCApprove = r_qc_approve.try_into()?;

    let markdown_body = qc_approve.body(git_info.as_ref());

    // Convert markdown to HTML
    let html_body = markdown::to_html(&markdown_body);
    Ok(html_body)
}

#[extendr]
fn post_qc_approval_impl(r_qc_approval_robj: Robj, working_dir: &str) -> Result<String> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();

    // Deserialize RQCComment from Robj
    let r_qc_approval: RQCApprove = from_robj(&r_qc_approval_robj)?;

    // Convert RQCComment to QCComment for posting
    let qc_approval: QCApprove = r_qc_approval.try_into()?;

    // Post the comment using the GitHubWriter trait
    let rt = get_rt();
    let result = rt
        .block_on(git_info.post_approval(&qc_approval))
        .map_err(|e| Error::Other(format!("Failed to post approval: {}", e)))?;

    Ok(result)
}

#[extendr]
fn create_qc_unapproval_impl(issue_robj: Robj, reason: String) -> Result<Robj> {
    let issue: Issue = from_robj(&issue_robj)
        .map_err(|e| Error::Other(format!("Failed to deserialize issue: {}", e)))?;

    let qc_unapprove = QCUnapprove { issue, reason };

    to_robj(&qc_unapprove)
}

#[extendr]
fn get_qc_unapproval_body_html_impl(r_qc_unapproval_robj: Robj) -> Result<String> {
    // Deserialize QCUnapprove from Robj
    let qc_unapprove: QCUnapprove = from_robj(&r_qc_unapproval_robj)?;

    let markdown_body = qc_unapprove.body();

    // Convert markdown to HTML
    let html_body = markdown::to_html(&markdown_body);
    Ok(html_body)
}

#[extendr]
fn post_qc_unapproval_impl(r_qc_unapproval_robj: Robj, working_dir: &str) -> Result<String> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();

    // Deserialize QCUnapprove from Robj
    let qc_unapprove: QCUnapprove = from_robj(&r_qc_unapproval_robj)?;

    // Post the comment using the GitHubWriter trait
    let rt = get_rt();
    let result = rt
        .block_on(git_info.post_unapproval(&qc_unapprove))
        .map_err(|e| Error::Other(format!("Failed to post unapproval: {}", e)))?;

    Ok(result)
}
