use std::{path::PathBuf, str::FromStr};

use extendr_api::{deserializer::from_robj, prelude::*, IntoRobj};
use ghqctoolkit::{
    archive, parse_branch_from_body, ArchiveFile, ArchiveMetadata, ArchiveQC, IssueThread,
};
use gix::ObjectId;
use octocrab::models::issues::Issue;
use serde::Deserialize;

use crate::{
    utils::{get_cached_git_info, get_disk_cache, get_rt},
    ENV_PROVIDER,
};

extendr_module! {
    mod archive;
    fn get_issue_latest_commit_impl;
    fn get_issue_branch_impl;
    fn create_archive_impl;
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

#[derive(Debug, Clone, Deserialize)]
struct ArchiveFileRow {
    file: String,
    commit: String,
    milestone: Option<String>,
    state: Option<String>,
}

impl ArchiveFileRow {
    fn into_archive_file(&self, flatten: bool) -> Result<ArchiveFile> {
        let commit = ObjectId::from_str(&self.commit).map_err(|e| {
            format!(
                "Could not parse commit from {} for {}: {e}",
                self.commit, self.file
            )
        })?;

        let repository_file = PathBuf::from(&self.file);
        let archive_file = if flatten {
            repository_file
                .file_name()
                .map(PathBuf::from)
                .expect("File to have file name")
        } else {
            repository_file
                .strip_prefix("/")
                .unwrap_or(&repository_file)
                .to_path_buf()
        };

        let qc = match (&self.milestone, &self.state) {
            (Some(milestone), Some(state)) => Some(ArchiveQC {
                milestone: milestone.to_string(),
                approved: state == "approved",
            }),
            (None, None) => None,
            _ => {
                return Err(Error::Other(format!(
                    "Milestone and state must be both a value or both null"
                )));
            }
        };

        Ok(ArchiveFile {
            repository_file,
            archive_file,
            commit,
            qc,
        })
    }
}

#[extendr]
fn create_archive_impl(
    archive_files_df: Robj,
    flatten: bool,
    archive_path: &str,
    working_dir: &str,
) -> Result<String> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();

    let archive_files_row = from_robj::<Vec<ArchiveFileRow>>(&archive_files_df)?;
    let archive_files = archive_files_row
        .iter()
        .map(|row| row.into_archive_file(flatten))
        .collect::<Result<Vec<_>>>()?;
    log::debug!("Including {} files in archive", archive_files.len());

    let archive_metadata = ArchiveMetadata::new(archive_files, &ENV_PROVIDER)
        .map_err(|e| Error::Other(format!("{e}")))?;
    let path = PathBuf::from(working_dir).join(archive_path);

    log::debug!("Creating archive at {}", path.display());
    archive(archive_metadata, git_info, &path)
        .map_err(|e| format!("Failed to create archive: {e}"))?;

    Ok(path.to_string_lossy().to_string())
}
