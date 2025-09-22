use std::path::PathBuf;

use anyhow::{anyhow, Result};
use ghqctoolkit::{
    configuration_status, determine_config_dir, setup_configuration, utils::StdEnvProvider,
    Configuration, GitCommand, GitInfo,
};

use crate::get_rt;

pub fn setup_configuration_impl(config_dir: Option<PathBuf>, git: &str) -> Result<String> {
    let env = StdEnvProvider;
    let git_cmd = GitCommand;

    // Validate and parse the git URL
    let git = gix::url::parse(git.into()).map_err(|e| anyhow!("Invalid git URL '{git}': {e}"))?;

    let config_dir = determine_config_dir(config_dir, &env)
        .map_err(|e| anyhow!("Error determining config directory: {e}"))?;

    // Create a tokio runtime and run the async setup_configuration function
    let rt = get_rt();

    rt.block_on(setup_configuration(&config_dir, git.clone(), git_cmd))
        .map_err(|e| anyhow!("Config directory could not be cloned: {e}"))?;

    Ok(format!(
        "Configuration successfully setup at {}",
        config_dir.display()
    ))
}

pub fn configuration_status_impl(config_dir: Option<PathBuf>) -> Result<String> {
    let env = StdEnvProvider;
    let config_dir = determine_config_dir(config_dir, &env)
        .map_err(|e| anyhow!("Error determining config directory: {e}"))?;
    let mut configuration = Configuration::from_path(&config_dir);
    configuration.load_checklists();

    // Create a tokio runtime for GitHub API operations (needed by GitInfo::from_path -> create_authenticated_client)
    let rt = get_rt();

    let git_info = rt.block_on(GitInfo::from_path(&config_dir, &env)).ok();

    Ok(configuration_status(&configuration, &git_info))
}
