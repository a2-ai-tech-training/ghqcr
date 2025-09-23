use std::path::Path;

use extendr_api::{prelude::*, serializer::to_robj, Robj};
use ghqctoolkit::{
    get_repo_users, Checklist, GitFileOps, GitHubReader, GitHubWriter, GitRepository, QCIssue,
    RelevantFile, RepoUser,
};
use octocrab::models::Milestone;

use crate::{get_disk_cache, get_rt};

pub fn get_milestones_impl(git_info: &impl GitHubReader) -> Result<Vec<Robj>> {
    let rt = get_rt();
    log::debug!("Runtime created!");
    let milestones = rt
        .block_on(git_info.get_milestones())
        .map_err(|e| Error::Other(format!("Failed to get milestones: {e}")))?;
    milestones.iter().map(to_robj).collect::<Result<Vec<_>>>()
}

#[derive(Debug, Clone, PartialEq, IntoDataFrameRow)]
struct RRepoUsers {
    login: String,
    name: Option<String>,
}

pub fn get_users_impl(git_info: &(impl GitHubReader + GitRepository)) -> Robj {
    let rt = get_rt();
    let cache = get_disk_cache(git_info);
    let repo_users = rt
        .block_on(get_repo_users(cache.as_ref(), git_info))
        .unwrap_or({
            log::warn!("Could not determine repo users. Defaulting to none...");
            Vec::new()
        });

    let r_repo_users = repo_users
        .into_iter()
        .map(|r| RRepoUsers {
            login: r.login,
            name: r.name,
        })
        .collect::<Vec<_>>();

    r_repo_users.into_dataframe().map(|d| d.into()).unwrap_or({
        log::warn!("Could not convert repo users to dataframe. Defaulting to none...");
        Robj::default()
    })
}

fn create_issue_impl(
    milestone: &Milestone,
    file: impl AsRef<Path>,
    checklist: Checklist,
    relevant_files: Vec<RelevantFile>,
    assignees: &[RepoUser],
    git_info: &(impl GitHubWriter + GitHubReader + GitFileOps + GitRepository),
) -> Result<String> {
    let file = file.as_ref();
    let rt = get_rt();

    let milestone_issues = rt
        .block_on(git_info.get_milestone_issues(&milestone))
        .map_err(|e| {
            format!(
                "Failed to get issues for milestone {}: {}",
                milestone.number, e
            )
        })?;
    if milestone_issues
        .iter()
        .any(|i| i.title == file.to_string_lossy().as_ref())
    {
        return Err(Error::Other(format!(
            "File already has a corresponding issue within the milestone"
        )));
    }

    // sending RepoUser and flattening to logins so we don't have to verify valid user
    let assignees = assignees
        .iter()
        .map(|a| a.login.to_string())
        .collect::<Vec<_>>();
    let milestone_id = milestone.number as u64;

    let issue = QCIssue::new(
        file,
        git_info,
        milestone_id,
        assignees,
        relevant_files,
        checklist,
    )
    .map_err(|e| Error::Other(format!("Failed to create QC Issue: {e}")))?;

    let url = rt
        .block_on(git_info.post_issue(&issue))
        .map_err(|e| Error::Other(format!("Failed to post QC Issue: {e}")))?;

    Ok(url)
}
