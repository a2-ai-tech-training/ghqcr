use std::path::Path;

use extendr_api::{deserializer::from_robj, prelude::*, serializer::to_robj, Robj};
use ghqctoolkit::{
    find_file_commits, get_repo_users, GitFileOps, GitHubReader, GitRepository, GitStatusOps,
    IssueCommit, IssueThread,
};
use octocrab::models::issues::Issue;
use serde::Deserialize;

use crate::utils::{get_cached_git_info, get_disk_cache, get_rt};

extendr_module! {
    mod git_utils;
    fn get_milestones_impl;
    fn get_milestone_issues_impl;
    fn get_users_impl;
    fn get_issue_commits_impl;
    fn file_git_status_impl;
    fn get_head_commit_impl;
    fn get_branch_impl;
    fn get_branch_commits;
}

#[extendr]
fn get_milestones_impl(working_dir: &str) -> Result<Vec<Robj>> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();

    let rt = get_rt();
    let milestones = rt
        .block_on(git_info.get_milestones())
        .map_err(|e| Error::Other(format!("Failed to get milestones: {e}")))?;
    milestones.iter().map(to_robj).collect::<Result<Vec<_>>>()
}

#[extendr]
fn get_milestone_issues_impl(working_dir: &str, milestone: Robj) -> Result<Vec<Robj>> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();
    let milestone = from_robj(&milestone)?;
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
pub struct RRepoUser {
    pub login: String,
    pub name: Option<String>,
}

#[extendr]
fn get_users_impl(working_dir: &str) -> Robj {
    let Ok(cached_git_info) = get_cached_git_info(working_dir) else {
        log::debug!("Could not initialize git repository for {working_dir} to determine repo users. Defaulting to none...");
        return Robj::default();
    };

    let git_info = cached_git_info.as_ref();
    let cache = get_disk_cache(git_info);

    let rt = get_rt();
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

#[derive(Debug, Clone, PartialEq, IntoDataFrameRow)]
struct RFileGitStatus {
    file_path: String,
    is_git_tracked: bool,
    has_commits: bool,
    git_status: Option<String>,
    commit_hash: Option<String>,
    error_message: Option<String>,
}

#[extendr]
fn file_git_status_impl(
    files: Vec<String>,
    working_dir: &str,
) -> Result<Dataframe<RFileGitStatus>> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();

    let mut results = Vec::new();

    // Get git status once for efficiency
    let git_status = match git_info.status() {
        Ok(status) => Some(status),
        Err(e) => {
            log::warn!("Failed to get git status: {e}");
            None
        }
    };

    // Get current branch for commit detection
    let current_branch = git_info.branch().ok();

    for file_path in files {
        let mut result = RFileGitStatus {
            file_path: file_path.clone(),
            is_git_tracked: false,
            has_commits: false,
            git_status: None,
            commit_hash: None,
            error_message: None,
        };

        // Check if file has commits (indicates it's git tracked)
        let commits_res = git_info.commits(&current_branch);
        let file_commits = match &commits_res {
            Ok(commits) => {
                let file_commits = find_file_commits(&file_path, &commits);
                if file_commits.is_empty() {
                    log::debug!("No commits for file {file_path}");
                    result.is_git_tracked = false;
                    Vec::new()
                } else {
                    result.is_git_tracked = true;
                    result.has_commits = !commits.is_empty();
                    if let Some(first_commit) = commits.first() {
                        result.commit_hash = Some(first_commit.commit.to_string());
                    }
                    commits.iter().map(|c| &c.commit).collect::<Vec<_>>()
                }
            }
            Err(e) => {
                // File might not be tracked or other git error
                log::debug!("Could not get commits for {}: {}", file_path, e);
                result.is_git_tracked = false;
                Vec::new()
            }
        };

        // Get git status for the file if available
        if let Some(ref git_status) = git_status {
            let formatted_status = git_status.format_for_file(Path::new(&file_path), &file_commits);
            result.git_status = Some(formatted_status);
        }

        results.push(result);
    }

    results
        .into_dataframe()
        .map_err(|e| Error::Other(format!("Failed to create dataframe: {}", e)))
}

#[derive(Debug, Clone, PartialEq, IntoDataFrameRow)]
struct RIssueCommit {
    hash: String,
    message: String,
    qc_class: String, // Will convert QCClass to String for R compatibility
    edits_file: bool,
    reviewed: bool,
}

impl From<IssueCommit> for RIssueCommit {
    fn from(commit: IssueCommit) -> Self {
        Self {
            hash: commit.hash.to_string(),
            message: commit.message,
            qc_class: commit.state.to_string(),
            edits_file: commit.file_changed,
            reviewed: commit.reviewed,
        }
    }
}

#[extendr]
fn get_issue_commits_impl(working_dir: &str, issue_robj: Robj) -> Result<Dataframe<RIssueCommit>> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();
    let cache = get_disk_cache(git_info);

    let rt = get_rt();

    // Deserialize the issue from R
    let issue: Issue = from_robj(&issue_robj)?;

    // Create IssueThread from the issue
    let issue_thread = rt
        .block_on(IssueThread::from_issue(&issue, cache.as_ref(), git_info))
        .map_err(|e| Error::Other(format!("Failed to create issue thread: {e}")))?;
    let r_commits = issue_thread
        .commits
        .into_iter()
        .map(RIssueCommit::from)
        .collect::<Vec<_>>();

    r_commits
        .into_dataframe()
        .map_err(|e| Error::Other(format!("Failed to create dataframe: {}", e)))
}

#[extendr]
fn get_head_commit_impl(working_dir: &str) -> Result<String> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();

    // Use the existing commit() trait function to get HEAD commit
    match git_info.commit() {
        Ok(commit) => Ok(commit.to_string()),
        Err(e) => Err(Error::Other(format!("Failed to get HEAD commit: {}", e))),
    }
}

#[extendr]
fn get_branch_impl(working_dir: &str) -> Result<String> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();

    git_info
        .branch()
        .map_err(|e| Error::Other(format!("Failed to determine current branch: {e}")))
}

#[derive(Debug, Clone, PartialEq, IntoDataFrameRow)]
struct RCommitRow {
    commit: String,
    message: String,
    file: String,
}

#[extendr]
fn get_branch_commits(
    working_dir: &str,
    #[default = "NULL"] branch: Nullable<String>,
) -> Result<Dataframe<RCommitRow>> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();

    let branch = branch.into_option();

    git_info
        .commits(&branch)
        .map_err(|e| {
            Error::Other(format!(
                "Failed to determine commits for branch: {branch:?}: {e}"
            ))
        })?
        .iter()
        .flat_map(|c| {
            c.files.iter().map(|f| RCommitRow {
                commit: c.commit.to_string(),
                message: c.message.to_string(),
                file: f.to_string_lossy().to_string(),
            })
        })
        .collect::<Vec<_>>()
        .into_dataframe()
}
