use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{Arc, Mutex, OnceLock},
};

use extendr_api::prelude::*;
use ghqctoolkit::{DiskCache, GitInfo, GitRepository};
use tokio::runtime::{Builder, Runtime};

use crate::ENV_PROVIDER;

static TOKIO_RUNTIME: OnceLock<Runtime> = OnceLock::new();
static DISK_CACHE: OnceLock<Option<DiskCache>> = OnceLock::new();
static GIT_INFO_CACHE: OnceLock<Mutex<HashMap<String, Arc<GitInfo>>>> = OnceLock::new();

// Helper function to get a tokio runtime
pub fn get_rt() -> &'static Runtime {
    TOKIO_RUNTIME.get_or_init(|| {
        Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("Failed to create tokio runtime")
    })
}

pub fn get_disk_cache(git_info: &impl GitRepository) -> &'static Option<DiskCache> {
    DISK_CACHE.get_or_init(|| DiskCache::from_git_info(git_info).ok())
}

pub fn get_cached_git_info(working_dir: &str) -> Result<Arc<GitInfo>> {
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
        .map_to_extendr_err("FAiled to create GitInfo")?;

    let arc_git_info = Arc::new(git_info);
    cache_guard.insert(key, Arc::clone(&arc_git_info));

    Ok(arc_git_info)
}

pub trait ResultExt<T> {
    fn map_to_extendr_err(self, message: &str) -> Result<T>;
}

impl<T, E: std::fmt::Debug> ResultExt<T> for std::result::Result<T, E> {
    fn map_to_extendr_err(self, message: &str) -> extendr_api::Result<T> {
        self.map_err(|x| extendr_api::Error::Other(format!("{}: {x:?}", message)))
    }
}
