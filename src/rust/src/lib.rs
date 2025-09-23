mod configuration;
mod create;

use std::{
    collections::HashMap,
    path::PathBuf,
    sync::OnceLock,
    sync::{Arc, Mutex},
};

use extendr_api::{prelude::*, Robj};
use ghqctoolkit::{utils::StdEnvProvider, DiskCache, GitInfo, GitRepository};
use log::{Level, Log, Metadata, Record};
use std::sync::Once;
use tokio::runtime::{Builder, Runtime};

use crate::{
    configuration::{
        configuration_status_impl, determine_config_dir_from_null, get_checklists_impl,
        setup_configuration_impl,
    },
    create::{get_milestones_impl, get_users_impl},
};

static TOKIO_RUNTIME: OnceLock<Runtime> = OnceLock::new();
static DISK_CACHE: OnceLock<Option<DiskCache>> = OnceLock::new();
static GIT_INFO_CACHE: OnceLock<Mutex<HashMap<String, Arc<GitInfo>>>> = OnceLock::new();
static ENV_PROVIDER: StdEnvProvider = StdEnvProvider;
static LOGGER_INIT: Once = Once::new();

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

// Helper function to get a tokio runtime
fn get_rt() -> &'static Runtime {
    TOKIO_RUNTIME.get_or_init(|| {
        Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("Failed to create tokio runtime")
    })
}

fn get_disk_cache(git_info: &impl GitRepository) -> &'static Option<DiskCache> {
    DISK_CACHE.get_or_init(|| DiskCache::from_git_info(git_info).ok())
}

fn get_cached_git_info(working_dir: &str) -> Result<Arc<GitInfo>> {
    let cache = GIT_INFO_CACHE.get_or_init(|| Mutex::new(HashMap::new()));
    let mut cache_guard = cache
        .lock()
        .map_err(|_| Error::Other("Failed to acquire GitInfo cache lock".to_string()))?;

    // Canonicalize path for consistent cache keys
    let key = std::fs::canonicalize(working_dir)
        .unwrap_or_else(|_| PathBuf::from(working_dir))
        .to_string_lossy()
        .to_string();

    if let Some(git_info) = cache_guard.get(&key) {
        log::debug!("Found cached GitInfo for: {}", working_dir);
        return Ok(Arc::clone(git_info));
    }

    log::debug!("Creating new GitInfo for: {}", working_dir);
    let git_info = GitInfo::from_path(&PathBuf::from(working_dir), &ENV_PROVIDER)
        .map_err(|e| Error::Other(format!("Failed to create GitInfo: {e}")))?;

    let arc_git_info = Arc::new(git_info);
    cache_guard.insert(key, Arc::clone(&arc_git_info));

    Ok(arc_git_info)
}

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
pub fn get_checklists_extr(config_dir: Nullable<&str>) -> Result<Robj> {
    let config_dir = determine_config_dir_from_null(config_dir)?;
    get_checklists_impl(&config_dir)
        .map_err(|e| Error::Other(format!("Failed to load checklists due to: {e}")))
}

#[extendr]
pub fn get_milestones_extr(working_dir: &str) -> Result<Vec<Robj>> {
    let git_info = get_cached_git_info(working_dir)?;

    get_milestones_impl(git_info.as_ref())
}

#[extendr]
pub fn get_repo_users_extr(working_dir: &str) -> Result<Robj> {
    let git_info = get_cached_git_info(working_dir)?;
    Ok(get_users_impl(git_info.as_ref()))
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

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod ghqcr;
    fn setup_configuration_extr;
    fn configuration_status_extr;
    fn get_checklists_extr;
    fn get_milestones_extr;
    fn get_repo_users_extr;
    fn init_logger_extr;
    fn log_message_extr;
}
