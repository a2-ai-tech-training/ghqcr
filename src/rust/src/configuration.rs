use std::path::{Path, PathBuf};

use extendr_api::{prelude::*, Robj};
use ghqctoolkit::{
    configuration_status, determine_config_dir, setup_configuration, Configuration, GitCommand,
    GitInfo,
};
use serde::Deserialize;

use crate::{get_rt, ENV_PROVIDER};

pub fn determine_config_dir_from_null(config_dir: Nullable<&str>) -> Result<PathBuf> {
    let c = config_dir.into_option().map(PathBuf::from);
    determine_config_dir(c, &ENV_PROVIDER)
        .map_err(|e| extendr_api::Error::Other(format!("Error determining config directory: {e}")))
}

pub fn setup_configuration_impl(config_dir: impl AsRef<Path>, git: &str) -> Result<String> {
    let git_cmd = GitCommand;

    // Validate and parse the git URL
    let git = gix::url::parse(git.into())
        .map_err(|e| Error::Other(format!("Invalid git URL '{git}': {e}")))?;

    // Create a tokio runtime and run the async setup_configuration function
    let rt = get_rt();

    rt.block_on(setup_configuration(
        config_dir.as_ref(),
        git.clone(),
        git_cmd,
    ))
    .map_err(|e| Error::Other(format!("Config directory could not be cloned: {e}")))?;

    Ok(format!(
        "Configuration successfully setup at {}",
        config_dir.as_ref().display()
    ))
}

pub fn configuration_status_impl(config_dir: impl AsRef<Path>) -> Result<String> {
    let mut configuration = Configuration::from_path(config_dir.as_ref());
    configuration.load_checklists();

    let git_info = GitInfo::from_path(config_dir.as_ref(), &ENV_PROVIDER).ok();

    Ok(configuration_status(&configuration, &git_info))
}

pub fn get_configuration_impl(config_dir: impl AsRef<Path>) -> ExternalPtr<Configuration> {
    let mut configuration = Configuration::from_path(config_dir);
    configuration.load_checklists();

    ExternalPtr::new(configuration)
}

#[derive(Clone, PartialEq, Debug, IntoDataFrameRow, Deserialize)]
pub struct RChecklist {
    pub name: String,
    pub content: String,
}

pub fn get_checklists_impl(configuration: &ExternalPtr<Configuration>) -> Result<Robj> {
    let checklists = configuration
        .checklists
        .values()
        .map(|c| RChecklist {
            name: c.name.to_string(),
            content: c.content.to_string(),
        })
        .collect::<Vec<_>>();

    let r_obj = checklists
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

pub fn format_checklist_as_html_impl(checklist_robj: &Robj) -> Result<String> {
    use extendr_api::deserializer::from_robj;

    let checklist: RChecklist = from_robj(checklist_robj)?;
    let markdown_content = format!("# {}\n{}", checklist.name, checklist.content);

    // Convert markdown to HTML
    let html = markdown::to_html(&markdown_content);

    Ok(html)
}
