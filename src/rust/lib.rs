mod configuration;
mod create;
mod git_utils;
mod notify;
mod record;
mod utils;

use std::sync::Mutex;

use extendr_api::prelude::*;
use ghqctoolkit::utils::StdEnvProvider;

static ENV_PROVIDER: StdEnvProvider = StdEnvProvider;
static LOGGER_INIT: Mutex<bool> = Mutex::new(false);

#[extendr]
fn init_logger_impl() {
    let mut initialized = LOGGER_INIT.lock().unwrap();

    if *initialized {
        // Logger already initialized, skip
        return;
    }

    let mut builder = env_logger::Builder::new();

    // All other crates should be OFF
    builder.filter_level(log::LevelFilter::Off);

    // Get the log level for ghqcr and ghqctoolkit (default to INFO if not set)
    let ghqc_level = match std::env::var("GHQC_LOG_LEVEL") {
        Ok(level_str) => {
            match level_str.to_uppercase().as_str() {
                "ERROR" => log::LevelFilter::Error,
                "WARN" => log::LevelFilter::Warn,
                "INFO" => log::LevelFilter::Info,
                "DEBUG" => log::LevelFilter::Debug,
                "TRACE" => log::LevelFilter::Trace,
                _ => log::LevelFilter::Info, // Default to INFO for unknown levels
            }
        }
        Err(_) => log::LevelFilter::Info, // Default to INFO when not set
    };

    // Set levels for our modules
    builder
        .filter_module("ghqctoolkit", ghqc_level)
        .filter_module("ghqcr", ghqc_level);

    // octocrab should never go below INFO, but can be more verbose if requested
    let octocrab_level = if ghqc_level < log::LevelFilter::Info {
        log::LevelFilter::Info
    } else {
        ghqc_level
    };
    builder.filter_module("octocrab", octocrab_level);

    let level_display = match std::env::var("GHQC_LOG_LEVEL") {
        Ok(level) => level,
        Err(_) => "INFO (default)".to_string(),
    };

    match builder.try_init() {
        Ok(_) => {
            *initialized = true;
            eprintln!("Logger initialized with GHQC_LOG_LEVEL: {}", level_display);
            log::info!("GHQC Logger initialized successfully");
        }
        Err(_) => {
            // Logger was already initialized by another thread or call
            // This is fine, just mark as initialized
            *initialized = true;
            eprintln!(
                "Logger already initialized, GHQC_LOG_LEVEL: {}",
                level_display
            );
        }
    }
}

#[extendr]
fn reset_logger_impl() {
    let mut initialized = LOGGER_INIT.lock().unwrap();
    *initialized = false;
    eprintln!("Logger reset - next call to init_logger_impl will reinitialize");
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
    use record;
    fn init_logger_impl;
    fn reset_logger_impl;
    fn log_message_impl;
    fn read_to_string_impl;
    fn markdown_to_html_impl;
}
