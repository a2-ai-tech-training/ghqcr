use extendr_api::{deserializer::from_robj, prelude::*, serializer::to_robj, Robj};
use ghqctoolkit::{get_repo_users, DiskCache, GitHubReader, GitRepository};
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
