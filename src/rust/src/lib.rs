mod configuration;
mod create;
mod git_utils;
mod notify;
mod utils;

use std::sync::Once;

use extendr_api::prelude::*;
use ghqctoolkit::utils::StdEnvProvider;

static ENV_PROVIDER: StdEnvProvider = StdEnvProvider;
static LOGGER_INIT: Once = Once::new();

#[extendr]
fn init_logger_impl() {
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
fn log_message_impl(level: &str, msg: &str) {
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
fn markdown_to_html_impl(content: &str) -> String {
    markdown::to_html(content)
}

#[extendr]
fn read_to_string_impl(path: &str) -> Nullable<String> {
    match std::fs::read_to_string(path) {
        Ok(c) => Nullable::NotNull(c),
        Err(e) => {
            log::debug!("File {path} cannot be read: {e}. Returning null...");
            Nullable::Null
        }
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod ghqcr;
    use notify;
    use git_utils;
    use create;
    use configuration;
    fn init_logger_impl;
    fn log_message_impl;
    fn read_to_string_impl;
    fn markdown_to_html_impl;
}
