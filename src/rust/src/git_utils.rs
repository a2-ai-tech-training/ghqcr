use std::path::Path;

use extendr_api::{deserializer::from_robj, prelude::*, serializer::to_robj, Robj};
use ghqctoolkit::{
    find_file_commits, get_repo_users, DiskCache, GitCommitAnalysis, GitFileOps, GitHubReader,
    GitRepository, GitStatusOps, IssueCommit, IssueThread,
};
use octocrab::models::issues::Issue;
use serde::Deserialize;

use crate::utils::get_rt;

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
pub struct RRepoUser {
    pub login: String,
    pub name: Option<String>,
}

pub fn get_users_impl(
    git_info: &(impl GitHubReader + GitRepository),
    cache: Option<&DiskCache>,
) -> Robj {
    let rt = get_rt();
    let repo_users = match rt.block_on(get_repo_users(cache, git_info)) {
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
pub struct RFileGitStatus {
    pub file_path: String,
    pub is_git_tracked: bool,
    pub has_commits: bool,
    pub git_status: Option<String>,
    pub commit_hash: Option<String>,
    pub error_message: Option<String>,
}

pub fn file_git_status_impl(
    files: Vec<String>,
    git_info: &(impl GitFileOps + GitRepository + GitStatusOps),
) -> Result<Vec<RFileGitStatus>> {
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

    Ok(results)
}

#[derive(Debug, Clone, PartialEq, IntoDataFrameRow)]
pub struct RIssueCommit {
    pub hash: String,
    pub message: String,
    pub qc_class: String, // Will convert QCClass to String for R compatibility
    pub edits_file: bool,
}

impl From<IssueCommit> for RIssueCommit {
    fn from(commit: IssueCommit) -> Self {
        Self {
            hash: commit.hash.to_string(),
            message: commit.message,
            qc_class: commit.state.to_string(),
            edits_file: commit.file_changed,
        }
    }
}

pub fn get_issue_commits_impl(
    git_info: &(impl GitFileOps + GitRepository + GitHubReader + GitCommitAnalysis),
    cache: Option<&DiskCache>,
    issue_robj: &Robj,
) -> Result<Vec<RIssueCommit>> {
    let rt = get_rt();

    // Deserialize the issue from R
    let issue: Issue = from_robj(issue_robj)?;

    // Create IssueThread from the issue
    let issue_thread = rt
        .block_on(IssueThread::from_issue(&issue, cache, git_info))
        .map_err(|e| Error::Other(format!("Failed to create issue thread: {e}")))?;
    let r_commits = issue_thread
        .commits
        .into_iter()
        .map(RIssueCommit::from)
        .collect::<Vec<_>>();

    Ok(r_commits)
}
