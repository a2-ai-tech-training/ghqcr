use extendr_api::{deserializer::from_robj, prelude::*};
use ghqctoolkit::IssueThread;
use octocrab::models::issues::Issue;

use crate::utils::{get_cached_git_info, get_disk_cache, get_rt};

extendr_module! {
    mod archive;
    fn get_issue_df_impl;
}

#[derive(Debug, Clone, IntoDataFrameRow)]
struct RIssueRow {
    file: String,
    milestone: String,
    commit: String,
    state: String,
}

impl RIssueRow {
    fn from_issue_thread(issue_thread: IssueThread, milestone: String) -> Result<Self> {
        let commit = match issue_thread.latest_commit() {
            Some(latest_commit) => issue_thread
                .commits
                .iter()
                .find(|c| &c.hash == latest_commit),
            None => issue_thread.commits.first(),
        };

        match commit {
            Some(c) => Ok(RIssueRow {
                file: issue_thread.file.to_string_lossy().to_string(),
                milestone,
                commit: c.hash.to_string(),
                state: c.state.to_string(),
            }),
            None => Err(Error::Other(format!(
                "Failed to find a commit for {} in {milestone}",
                issue_thread.file.display()
            ))),
        }
    }
}

#[extendr]
fn get_issue_df_impl(issues_robj: Robj, working_dir: &str) -> Result<Dataframe<RIssueRow>> {
    let issues: Vec<Issue> = from_robj(&issues_robj)?;
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();
    let cache = get_disk_cache(git_info);
    let issue_thread_futures = issues
        .iter()
        .map(|issue| async move { IssueThread::from_issue(&issue, cache.as_ref(), git_info).await })
        .collect::<Vec<_>>();

    let rt = get_rt();
    let thread_results = rt.block_on(futures::future::join_all(issue_thread_futures));
    let mut err = Vec::new();
    let mut df = Vec::new();
    for (issue, thread_result) in issues.iter().zip(thread_results) {
        let Some(milestone) = &issue.milestone else {
            err.push(format!(
                "Issue #{} - {} does not have an associated milestone",
                issue.number, issue.title
            ));
            continue;
        };
        match thread_result {
            Ok(issue_thread) => {
                match RIssueRow::from_issue_thread(issue_thread, milestone.title.to_string()) {
                    Ok(row) => df.push(row),
                    Err(e) => err.push(e.to_string()),
                }
            }
            Err(e) => {
                err.push(format!(
                    "Failed to get issue thread for {} in {}: {e}",
                    issue.title, milestone.title
                ));
            }
        }
    }

    if !err.is_empty() {
        return Err(Error::Other(format!(
            "Failed to get all required issue data due to: \n- {}",
            err.join("\n- ")
        )));
    }

    df.into_dataframe()
}
