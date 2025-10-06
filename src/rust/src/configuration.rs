use std::path::PathBuf;

use extendr_api::{deserializer::from_robj, prelude::*, serializer::to_robj, Robj};
use ghqctoolkit::{
    configuration_status, determine_config_dir, setup_configuration, Checklist, Configuration,
    ConfigurationOptions, GitCommand, GitInfo,
};
use serde::{Deserialize, Serialize};

use crate::{utils::get_rt, ENV_PROVIDER};

extendr_module! {
    mod configuration;
    fn setup_configuration_impl;
    fn configuration_status_impl;
    fn get_configuration_impl;
    fn get_checklists_impl;
    fn format_checklist_as_html_impl;
    fn get_checklist_display_name_impl;
    fn get_prepended_checklist_note_impl;
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
struct RConfigurationOptions {
    prepended_checklist_note: Option<String>,
    checklist_display_name: String,
    logo_path: String,
    checklist_directory: String,
}

impl From<ConfigurationOptions> for RConfigurationOptions {
    fn from(options: ConfigurationOptions) -> Self {
        Self {
            prepended_checklist_note: options.prepended_checklist_note,
            checklist_display_name: options.checklist_display_name,
            logo_path: options.logo_path.to_string_lossy().to_string(),
            checklist_directory: options.checklist_directory.to_string_lossy().to_string(),
        }
    }
}

#[derive(Clone, PartialEq, Debug, IntoDataFrameRow, Deserialize, Serialize)]
pub struct RChecklist {
    pub name: String,
    pub content: String,
}

impl From<Checklist> for RChecklist {
    fn from(checklist: Checklist) -> Self {
        Self {
            name: checklist.name,
            content: checklist.content,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
struct RConfiguration {
    path: String,
    checklists: Vec<RChecklist>,
    options: RConfigurationOptions,
}

impl From<Configuration> for RConfiguration {
    fn from(configuration: Configuration) -> Self {
        Self {
            path: configuration.path.to_string_lossy().to_string(),
            checklists: configuration
                .checklists
                .into_iter()
                .map(|(_, c)| c.into())
                .collect(),
            options: configuration.options.into(),
        }
    }
}

fn determine_config_dir_from_null(config_dir: Nullable<&str>) -> Result<PathBuf> {
    let c = config_dir.into_option().map(PathBuf::from);
    determine_config_dir(c, &ENV_PROVIDER)
        .map_err(|e| Error::Other(format!("Error determining config directory: {e}")))
}

#[extendr]
fn setup_configuration_impl(config_dir: Nullable<&str>, git: &str) -> Result<String> {
    let config_dir = determine_config_dir_from_null(config_dir)?;

    let git_cmd = GitCommand;

    // Validate and parse the git URL
    let git = gix::url::parse(git.into())
        .map_err(|e| Error::Other(format!("Invalid git URL '{git}': {e}")))?;

    // Create a tokio runtime and run the async setup_configuration function
    let rt = get_rt();

    rt.block_on(setup_configuration(&config_dir, git.clone(), git_cmd))
        .map_err(|e| Error::Other(format!("Config directory could not be cloned: {e}")))?;

    Ok(format!(
        "Configuration successfully setup at {}",
        config_dir.display()
    ))
}

#[extendr]
fn configuration_status_impl(config_dir: Nullable<&str>) -> Result<String> {
    let config_dir = determine_config_dir_from_null(config_dir)?;
    let mut configuration = Configuration::from_path(&config_dir);
    configuration.load_checklists();

    let git_info = GitInfo::from_path(&config_dir, &ENV_PROVIDER).ok();

    Ok(configuration_status(&configuration, &git_info))
}

#[extendr]
fn get_configuration_impl(config_dir: Nullable<&str>) -> Result<Robj> {
    let config_dir = determine_config_dir_from_null(config_dir)?;
    let mut configuration = Configuration::from_path(&config_dir);
    configuration.load_checklists();
    let r_configuration = RConfiguration::from(configuration);

    to_robj(&r_configuration)
}

#[extendr]
fn get_checklists_impl(configuration: Robj) -> Result<Robj> {
    let configuration: RConfiguration = from_robj(&configuration)?;

    let r_obj = configuration
        .checklists
        .into_dataframe()
        .map_err(|e| {
            Error::Other(format!(
                "Checklists could not be converted to R dataframe object: {e}"
            ))
        })?
        .as_robj()
        .clone();

    Ok(r_obj)
}

#[extendr]
fn format_checklist_as_html_impl(checklist: Robj) -> Result<String> {
    let checklist: RChecklist = from_robj(&checklist)?;
    let markdown_content = format!("# {}\n{}", checklist.name, checklist.content);

    // Convert markdown to HTML
    let html = markdown::to_html(&markdown_content);

    Ok(html)
}

#[extendr]
fn get_checklist_display_name_impl(configuration: Robj) -> String {
    match from_robj::<RConfiguration>(&configuration) {
        Ok(c) => c.options.checklist_display_name,
        Err(e) => {
            log::warn!("Could not de-serialize Configuration information: {e}. Defaulting checklist display name to 'Checklists'...");
            "Checklists".to_string()
        }
    }
}

#[extendr]
fn get_prepended_checklist_note_impl(configuration: Robj) -> Nullable<String> {
    match from_robj::<RConfiguration>(&configuration) {
        Ok(c) => c.options.prepended_checklist_note.into(),
        Err(e) => {
            log::warn!("Could not de-serialize Configuration information: {e}. Defaulting prepended checklist note to none...");
            Nullable::Null
        }
    }
}
