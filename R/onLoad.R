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

  shiny::addResourcePath("ghqcr", system.file(".", package = "ghqcr"))
}

init_logging <- function() {
  # Create .le environment if it doesn't exist
  if (!exists(".le", envir = .GlobalEnv)) {
    assign(".le", new.env(), envir = .GlobalEnv)
  }

  # Create simple logging functions that use Rust backend
  .le$trace <- function(msg) log_message_impl("TRACE", as.character(msg))
  .le$debug <- function(msg) log_message_impl("DEBUG", as.character(msg))
  .le$info <- function(msg) log_message_impl("INFO", as.character(msg))
  .le$warn <- function(msg) log_message_impl("WARN", as.character(msg))
  .le$error <- function(msg) log_message_impl("ERROR", as.character(msg))
}

.onUnload <- function(...) {
  shiny::removeResourcePath("ghqcr")
}
