ghqc_config_setup <- function(config_url = NULL, config_path = NULL) {
  config_url <- if (is.null(config_url)) {
    ghqc_config_url <- Sys.getenv("GHQC_CONFIG_URL")
    if (ghqc_config_url == "") {
      rlang::abort(
        "Env. variable `GHQC_CONFIG_URL` must be set if config_url is not provided",
        "ghqc_configuration_setup_error",
        parent = NA
      )
    }
    ghqc_config_url
  } else {
    config_url
  }

  val <- .catch(setup_configuration_impl(config_path, config_url))

  cli::cli_alert_success(val)
}

ghqc_config_status <- function(config_path = NULL) {
  val <- .catch(configuration_status_impl(config_path))
  message(val)
}
