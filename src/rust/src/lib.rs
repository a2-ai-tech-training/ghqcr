mod configuration;

use std::{path::PathBuf, sync::OnceLock};

use extendr_api::prelude::*;
use tokio::runtime::{Builder, Runtime};

use crate::configuration::{configuration_status_impl, setup_configuration_impl};

static TOKIO_RUNTIME: OnceLock<Runtime> = OnceLock::new();

// Helper function to get a tokio runtime
fn get_rt() -> &'static Runtime {
    TOKIO_RUNTIME.get_or_init(|| {
        Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("Failed to create tokio runtime")
    })
}

#[extendr]
pub fn setup_configuration_extr(config_dir: Nullable<&str>, git: &str) -> Result<String> {
    let config_dir = config_dir.into_option().map(PathBuf::from);
    setup_configuration_impl(config_dir, git).map_err(|e| {
        Error::Other(format!(
            "Failed to set-up configuration repository due to: {e}"
        ))
    })
}

#[extendr]
pub fn configuration_status_extr(config_dir: Nullable<&str>) -> Result<String> {
    let config_dir = config_dir.into_option().map(PathBuf::from);
    configuration_status_impl(config_dir)
        .map_err(|e| Error::Other(format!("Failed to determine configuration status: {e}")))
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod ghqcr;
    fn setup_configuration_extr;
    fn configuration_status_extr;
}
