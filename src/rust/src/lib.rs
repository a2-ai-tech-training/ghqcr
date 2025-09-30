mod configuration;
mod create;
mod git_utils;
mod notify;
mod utils;

use std::sync::Once;

use extendr_api::{prelude::*, Robj};
use ghqctoolkit::{utils::StdEnvProvider, Configuration};

use crate::{
    configuration::{
        configuration_status_impl, determine_config_dir_from_null, format_checklist_as_html_impl,
        get_checklists_impl, get_configuration_impl, setup_configuration_impl,
    },
    create::{create_issues_impl, file_git_status_impl},
    git_utils::{get_issue_commits_impl, get_milestone_issues_impl, get_milestones_impl, get_users_impl},
    notify::get_multiple_milestone_issues_impl,
    utils::{get_cached_git_info, get_disk_cache},
};

static ENV_PROVIDER: StdEnvProvider = StdEnvProvider;
static LOGGER_INIT: Once = Once::new();

#[extendr]
pub fn setup_configuration_extr(config_dir: Nullable<&str>, git: &str) -> Result<String> {
    let config_dir = determine_config_dir_from_null(config_dir)?;
    setup_configuration_impl(&config_dir, git).map_err(|e| {
        Error::Other(format!(
            "Failed to set-up configuration repository due to: {e}"
        ))
    })
}

#[extendr]
pub fn configuration_status_extr(config_dir: Nullable<&str>) -> Result<String> {
    let config_dir = determine_config_dir_from_null(config_dir)?;
    configuration_status_impl(&config_dir)
        .map_err(|e| Error::Other(format!("Failed to determine configuration status: {e}")))
}

#[extendr]
pub fn get_configuration_extr(config_dir: Nullable<&str>) -> ExternalPtr<Configuration> {
    let Ok(config_dir) = determine_config_dir_from_null(config_dir) else {
        log::debug!("Failed to determine configuration directory. Using default configuration...");
        return ExternalPtr::new(Configuration::default());
    };
    get_configuration_impl(config_dir)
}

#[extendr]
pub fn get_checklists_extr(configuration: &ExternalPtr<Configuration>) -> Result<Robj> {
    get_checklists_impl(&configuration)
}

#[extendr]
pub fn get_checklist_display_name_extr(configuration: &ExternalPtr<Configuration>) -> String {
    let disp_name = configuration.checklist_display_name().to_string();
    log::debug!("Checklist display name determined to be: {disp_name}");
    disp_name
}

#[extendr]
pub fn get_prepended_checklist_note_extr(
    configuration: &ExternalPtr<Configuration>,
) -> Nullable<String> {
    let note = configuration
        .prepended_checklist_note()
        .map(|s| s.to_string())
        .into();

    log::debug!("Prepended Checklist Note determined to be: {note:?}");

    note
}

#[extendr]
pub fn get_milestones_extr(working_dir: &str) -> Result<Vec<Robj>> {
    let git_info = get_cached_git_info(working_dir)?;

    get_milestones_impl(git_info.as_ref())
}

#[extendr]
pub fn get_milestone_issues_extr(working_dir: &str, milestone: Robj) -> Result<Vec<Robj>> {
    let git_info = get_cached_git_info(working_dir)?;

    get_milestone_issues_impl(git_info.as_ref(), &milestone)
}

#[extendr]
pub fn get_repo_users_extr(working_dir: &str) -> Result<Robj> {
    let git_info = get_cached_git_info(working_dir)?;
    let cache = get_disk_cache(git_info.as_ref());
    Ok(get_users_impl(git_info.as_ref(), cache.as_ref()))
}

#[extendr]
pub fn create_issues_extr(
    milestone_name: &str,
    description: Nullable<String>,
    file_data: Robj,
    milestones: Robj,
    prepended_checklist_note: Nullable<String>,
    working_dir: &str,
) -> Result<String> {
    let git_info = get_cached_git_info(working_dir)?;
    let cache = get_disk_cache(git_info.as_ref());

    create_issues_impl(
        milestone_name,
        description,
        file_data,
        milestones,
        prepended_checklist_note,
        git_info.as_ref(),
        cache.as_ref(),
    )
}

#[extendr]
pub fn init_logger_extr() {
    LOGGER_INIT.call_once(|| {
        env_logger::Builder::new()
            .filter_level(log::LevelFilter::Off)
            .filter_module("ghqctoolkit", log::LevelFilter::Debug)
            .filter_module("ghqcr", log::LevelFilter::Debug)
            .filter_module("octocrab", log::LevelFilter::Info)
            .parse_env("GHQC_LOG_VERBOSITY")
            .init();
    });
}

#[extendr]
pub fn log_message_extr(level: &str, msg: &str) {
    match level.to_uppercase().as_str() {
        "ERROR" => log::error!("{}", msg),
        "WARN" => log::warn!("{}", msg),
        "INFO" => log::info!("{}", msg),
        "DEBUG" => log::debug!("{}", msg),
        "TRACE" => log::trace!("{}", msg),
        _ => log::info!("{}", msg), // Default to info for unknown levels
    }
}

#[extendr]
pub fn format_checklist_as_html_extr(checklist_robj: Robj) -> Result<String> {
    format_checklist_as_html_impl(&checklist_robj)
}

#[extendr]
pub fn markdown_to_html_extr(content: &str) -> String {
    markdown::to_html(content)
}

#[extendr]
pub fn read_to_string_extr(path: &str) -> Nullable<String> {
    match std::fs::read_to_string(path) {
        Ok(c) => Nullable::NotNull(c),
        Err(e) => {
            log::debug!("File {path} cannot be read: {e}. Returning null...");
            Nullable::Null
        }
    }
}

#[extendr]
pub fn file_git_status_extr(files: Vec<String>, working_dir: &str) -> Result<Robj> {
    let git_info = get_cached_git_info(working_dir)?;
    let results = file_git_status_impl(files, git_info.as_ref())?;

    match results.into_dataframe() {
        Ok(df) => Ok(df.into()),
        Err(e) => Err(Error::Other(format!("Failed to create dataframe: {}", e))),
    }
}

#[extendr]
pub fn get_multiple_milestone_issues_extr(milestones: Robj, working_dir: &str) -> Result<Robj> {
    let git_info = get_cached_git_info(working_dir)?;
    get_multiple_milestone_issues_impl(git_info.as_ref(), &milestones)
}

#[extendr]
pub fn get_issue_commits_extr(issue: Robj, working_dir: &str) -> Result<Robj> {
    let git_info = get_cached_git_info(working_dir)?;
    let cache = get_disk_cache(git_info.as_ref());
    let results = get_issue_commits_impl(git_info.as_ref(), cache.as_ref(), &issue)?;

    match results.into_dataframe() {
        Ok(df) => Ok(df.into()),
        Err(e) => Err(Error::Other(format!("Failed to create dataframe: {}", e))),
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod ghqcr;
    fn setup_configuration_extr;
    fn configuration_status_extr;
    fn get_configuration_extr;
    fn get_checklist_display_name_extr;
    fn get_prepended_checklist_note_extr;
    fn get_checklists_extr;
    fn get_milestones_extr;
    fn get_milestone_issues_extr;
    fn get_repo_users_extr;
    fn create_issues_extr;
    fn file_git_status_extr;
    fn init_logger_extr;
    fn log_message_extr;
    fn format_checklist_as_html_extr;
    fn read_to_string_extr;
    fn markdown_to_html_extr;
    fn get_multiple_milestone_issues_extr;
    fn get_issue_commits_extr;
}
