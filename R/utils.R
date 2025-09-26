.catch <- function(cnd) {
  rlang::catch_cnd(
    {
      if (rlang::is_condition(cnd)) {
        cnd[["message"]] <- cnd[["value"]]
        rlang::cnd_signal(cnd)
      }
      cnd
    },
    "extendr_err"
  )
  cnd
}

capitalize <- function(word) {
  first_letter <- toupper(substring(word, 1, 1))
  rest_of_word <- substring(word, 2)
  glue::glue("{first_letter}{rest_of_word}")
}

generate_input_id <- function(prefix = NULL, name) {
  clean_name <- gsub("[^a-zA-Z0-9_.-]", "_", name)
  if (is.null(prefix)) {
    return(name)
  } else {
    return(paste0(prefix, "_", clean_name))
  }
}

#' Get milestones with ID fix applied
#'
#' Wrapper around get_milestones_extr that automatically fixes floating
#' point ID serialization issues by converting IDs to character strings.
#'
#' @param working_dir Working directory path
#' @return List of milestones with fixed IDs
get_milestones <- function(working_dir) {
  milestones <- .catch(get_milestones_extr(working_dir))

  # Convert all floating point values to integers to prevent serialization issues
  lapply(milestones, function(milestone) {
    milestone$id <- as.character(as.integer(milestone$id))
    milestone$creator$id <- as.character(as.integer(milestone$creator$id))
    milestone
  })
}
