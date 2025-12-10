use std::{path::PathBuf, str::FromStr};

use extendr_api::{deserializer::from_robj, prelude::*, IntoRobj};
use ghqctoolkit::{
    archive, parse_branch_from_body, ArchiveFile, ArchiveMetadata, ArchiveQC, CommitStatus,
    IssueThread,
};
use gix::ObjectId;
use octocrab::models::issues::Issue;
use serde::Deserialize;

use crate::{
    utils::{get_cached_git_info, get_disk_cache, get_rt, ResultExt},
    ENV_PROVIDER,
};

extendr_module! {
    mod archive;
    fn get_issue_latest_commit_impl;
    fn get_issue_branch_impl;
    fn create_archive_impl;
    fn get_file_content_impl;
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
    approved: bool,
}

impl From<IssueThread> for IssueLatestCommit {
    fn from(issue_thread: IssueThread) -> Self {
        let latest_commit = issue_thread.latest_commit();
        Self {
            file: issue_thread.file.to_string_lossy().to_string(),
            commit: latest_commit.hash.to_string(),
            message: latest_commit.message.to_string(),
            approved: latest_commit.statuses.contains(&CommitStatus::Approved),
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

    rt.block_on(IssueThread::from_issue(&issue, cache.as_ref(), git_info))
        .map(IssueLatestCommit::from)
        .map_to_extendr_err(&format!(
            "Failed to get issue thread for #{} - {}",
            issue.number, issue.title
        ))
}

#[derive(Debug, Clone, Deserialize)]
struct ArchiveFileRow {
    file: String,
    commit: String,
    milestone: Option<String>,
    approved: Option<bool>,
}

impl ArchiveFileRow {
    fn into_archive_file(&self, flatten: bool) -> Result<ArchiveFile> {
        let commit = ObjectId::from_str(&self.commit).map_to_extendr_err(&format!(
            "Could not parse commit from {} for {}",
            self.commit, self.file
        ))?;

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

        let qc = match (&self.milestone, self.approved) {
            (Some(milestone), Some(approved)) => Some(ArchiveQC {
                milestone: milestone.to_string(),
                approved,
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
        .map_to_extendr_err("Failed to create archive metadata")?;

    // Handle both relative and absolute archive paths correctly
    let path = {
        let archive_path_buf = PathBuf::from(archive_path);
        if archive_path_buf.is_absolute() {
            // If archive_path is absolute, use it as-is
            archive_path_buf
        } else {
            // If archive_path is relative, make it relative to working_dir
            PathBuf::from(working_dir).join(archive_path)
        }
    };

    log::debug!("Creating archive at {}", path.display());
    archive(archive_metadata, git_info, &path)
        .map_to_extendr_err(&format!("Failed to create archive"))?;

    Ok(path.to_string_lossy().to_string())
}

#[extendr]
fn get_file_content_impl(archive_file_robj: Robj, working_dir: &str) -> Result<String> {
    let cached_git_info = get_cached_git_info(working_dir)?;
    let git_info = cached_git_info.as_ref();
    let archive_file = from_robj::<ArchiveFileRow>(&archive_file_robj)?.into_archive_file(false)?;
    let content = archive_file
        .file_content(git_info)
        .map_to_extendr_err("Failed to get file content")?;

    String::from_utf8(content)
        .map_err(|_| Error::Other(format!("Failed to convert file's bytes to string")))
}
