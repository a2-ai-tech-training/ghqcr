.onLoad <- function(...) {
  # Setup unified logging system
  tryCatch({
    init_logging()
    cat("R logging initialized successfully\n")
  }, error = function(e) {
    cat("Failed to initialize R logging:", conditionMessage(e), "\n")
  })

  # Initialize the Rust logger when the package is loaded
  tryCatch({
    init_logger_extr()
    cat("Rust logging initialized successfully\n")
  }, error = function(e) {
    cat("Failed to initialize Rust logging:", conditionMessage(e), "\n")
  })

  shiny::addResourcePath("ghqcr", system.file(".", package = "ghqcr"))
}

init_logging <- function() {
  # Create .le environment if it doesn't exist
  if (!exists(".le", envir = .GlobalEnv)) {
    assign(".le", new.env(), envir = .GlobalEnv)
  }

  # Create simple logging functions that use Rust backend
  .le$debug <- function(msg) log_message_extr("DEBUG", as.character(msg))
  .le$info <- function(msg) log_message_extr("INFO", as.character(msg))
  .le$warn <- function(msg) log_message_extr("WARN", as.character(msg))
  .le$error <- function(msg) log_message_extr("ERROR", as.character(msg))
}

.onUnload <- function(...) {
  shiny::removeResourcePath("ghqcr")
}
