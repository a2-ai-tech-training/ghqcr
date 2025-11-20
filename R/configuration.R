#' @export
#' @description
#' Set-up the configuration repository. If `config_url` is provided, the repository will be cloned to `$XDG_DATA_HOME/ghqc/config`
#' If not AND `GHQC_CONFIG_URL` is provided, the repository will be cloned to `$XDG_DATA_HOME/ghqc/<repo name>`
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

#' @export
#' @description
#' Provides the status of the configuration repository found at `config_path` (default according to rules in `ghqc_config_setup()`)
#'
ghqc_config_status <- function(config_path = NULL) {
  val <- .catch(configuration_status_impl(config_path))
  message(val)
}
