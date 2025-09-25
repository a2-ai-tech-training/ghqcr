.onLoad <- function(...) {
  # Setup unified logging system
  tryCatch(
    {
      init_logging()
      init_logger_extr()
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
  .le$trace <- function(msg) log_message_extr("TRACE", as.character(msg))
  .le$debug <- function(msg) log_message_extr("DEBUG", as.character(msg))
  .le$info <- function(msg) log_message_extr("INFO", as.character(msg))
  .le$warn <- function(msg) log_message_extr("WARN", as.character(msg))
  .le$error <- function(msg) log_message_extr("ERROR", as.character(msg))
}

.onUnload <- function(...) {
  shiny::removeResourcePath("ghqcr")
}
