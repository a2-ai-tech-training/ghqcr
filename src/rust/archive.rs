use extendr_api::{deserializer::from_robj, prelude::*, IntoRobj};
use ghqctoolkit::{parse_branch_from_body, IssueThread};
use octocrab::models::issues::Issue;

use crate::utils::{get_cached_git_info, get_disk_cache, get_rt};

extendr_module! {
    mod archive;
    fn get_issue_latest_commit_impl;
    fn get_issue_branch_impl;
}

#[extendr]
fn get_issue_branch_impl(issue_robj: Robj) -> Nullable<String> {
    let issue: Issue = match from_robj::<Issue>(&issue_robj) {
        Ok(i) => i,
        Err(e) => {
            log::warn!("Failed to convert Issue Robj to struct: {e}. Returning null...");
            return Nullable::Null;
        }
    };

    let Some(body) = &issue.body else {
        log::warn!(
            "Issue #{} - {} has no body to parse for branch. Returning null...",
            issue.number,
            issue.title
        );
        return Nullable::Null;
    };

    match parse_branch_from_body(body) {
        Some(b) => Nullable::NotNull(b),
        None => {
            log::warn!(
                "Failed to parse branch from issue #{} - {}",
                issue.number,
                issue.title
            );
            Nullable::Null
        }
    }
}

#[derive(Debug, Clone, IntoRobj)]
struct IssueLatestCommit {
    file: String,
    commit: String,
    message: String,
    state: String,
}

impl TryFrom<IssueThread> for IssueLatestCommit {
    type Error = extendr_api::Error;
    fn try_from(issue_thread: IssueThread) -> std::result::Result<Self, Self::Error> {
        let commit = match issue_thread.latest_commit() {
            Some(latest_commit) => issue_thread
                .commits
                .iter()
                .find(|c| &c.hash == latest_commit),
            None => issue_thread.commits.first(),
        };

        match commit {
            Some(c) => Ok(Self {
                file: issue_thread.file.to_string_lossy().to_string(),
                commit: c.hash.to_string(),
                message: c.message.to_string(),
                state: c.state.to_string(),
            }),
            None => Err(Error::Other(format!(
                "Failed to find a commit for {}",
                issue_thread.file.display()
            ))),
        }
    }
}

#[extendr]
fn get_issue_latest_commit_impl(issue_robj: Robj, working_dir: &str) -> Result<IssueLatestCommit> {
    let issue: Issue = from_robj(&issue_robj)?;
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();
    let cache = get_disk_cache(git_info);
    let rt = get_rt();

    match rt.block_on(IssueThread::from_issue(&issue, cache.as_ref(), git_info)) {
        Ok(t) => t.try_into(),
        Err(e) => Err(Error::Other(format!(
            "Failed to get issue thread for #{} - {}: {e}",
            issue.number, issue.title
        ))),
    }
}
