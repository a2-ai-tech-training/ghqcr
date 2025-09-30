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

  # Convert all floating point values to character strings to prevent serialization issues
  lapply(milestones, function(milestone) {
    milestone$id <- format(milestone$id, scientific = FALSE, digits = 15)
    milestone$creator$id <- format(milestone$creator$id, scientific = FALSE, digits = 15)
    milestone
  })
}

#' Get multiple milestone issues with ID fix applied
#'
#' Wrapper around get_multiple_milestone_issues_extr that automatically fixes floating
#' point ID serialization issues by converting IDs to character strings.
#'
#' @param milestones List of milestone objects
#' @param working_dir Working directory path
#' @return Named list of milestone issues with fixed IDs
get_multiple_milestone_issues <- function(milestones, working_dir) {
  multiple_milestone_issues <- .catch(get_multiple_milestone_issues_extr(milestones, working_dir))

  # Fix floating point ID serialization issues for all issues
  lapply(multiple_milestone_issues, function(milestone_issues) {
    lapply(milestone_issues, function(issue) {
      # Fix issue ID
      issue$id <- format(issue$id, scientific = FALSE, digits = 15)

      # Fix user IDs in various fields
      if (!is.null(issue$user) && !is.null(issue$user$id)) {
        issue$user$id <- format(issue$user$id, scientific = FALSE, digits = 15)
      }

      if (!is.null(issue$assignee) && !is.null(issue$assignee$id)) {
        issue$assignee$id <- format(issue$assignee$id, scientific = FALSE, digits = 15)
      }

      # Fix assignees (array of users)
      if (!is.null(issue$assignees) && length(issue$assignees) > 0) {
        issue$assignees <- lapply(issue$assignees, function(assignee) {
          if (!is.null(assignee$id)) {
            assignee$id <- format(assignee$id, scientific = FALSE, digits = 15)
          }
          assignee
        })
      }

      # Fix milestone ID if present
      if (!is.null(issue$milestone) && !is.null(issue$milestone$id)) {
        issue$milestone$id <- format(issue$milestone$id, scientific = FALSE, digits = 15)
        if (!is.null(issue$milestone$creator) && !is.null(issue$milestone$creator$id)) {
          issue$milestone$creator$id <- format(issue$milestone$creator$id, scientific = FALSE, digits = 15)
        }
      }

      # Fix label IDs
      if (!is.null(issue$labels) && length(issue$labels) > 0) {
        issue$labels <- lapply(issue$labels, function(label) {
          if (!is.null(label$id)) {
            label$id <- format(label$id, scientific = FALSE, digits = 15)
          }
          label
        })
      }

      issue
    })
  })
}
