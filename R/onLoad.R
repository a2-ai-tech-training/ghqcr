.onLoad <- function(...) {
  # Setup unified logging system
  tryCatch(
    {
      init_logging()
      init_logger_impl()
    },
    error = function(e) {
      cat("Failed to initialize logging:", conditionMessage(e), "\n")
    }
  )

  shiny::addResourcePath("ghqc", system.file(".", package = "ghqc"))
}

init_logging <- function() {
  # Create .le environment if it doesn't exist
  if (!exists(".le", envir = .GlobalEnv)) {
    assign(".le", new.env(), envir = .GlobalEnv)
  }

  # Create simple logging functions that use Rust backend with glue interpolation
  .le$trace <- function(msg) {
    log_message_impl(
      "TRACE",
      as.character(glue::glue(msg, .envir = parent.frame()))
    )
  }
  .le$debug <- function(msg) {
    log_message_impl(
      "DEBUG",
      as.character(glue::glue(msg, .envir = parent.frame()))
    )
  }
  .le$info <- function(msg) {
    log_message_impl(
      "INFO",
      as.character(glue::glue(msg, .envir = parent.frame()))
    )
  }
  .le$warn <- function(msg) {
    log_message_impl(
      "WARN",
      as.character(glue::glue(msg, .envir = parent.frame()))
    )
  }
  .le$error <- function(msg) {
    log_message_impl(
      "ERROR",
      as.character(glue::glue(msg, .envir = parent.frame()))
    )
  }
}

.onUnload <- function(...) {
  shiny::removeResourcePath("ghqc")
}
