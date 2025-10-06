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

# Helper function to safely convert numeric IDs to character strings
safe_id_to_char <- function(id) {
  if (is.null(id) || is.na(id)) {
    return(id)
  }
  id_char <- as.character(id)
  gsub(".0", "", id_char)
}

#' Get milestones with ID fix applied
#'
#' Wrapper around get_milestones_impl that automatically fixes floating
#' point ID serialization issues by converting IDs to character strings.
#'
#' @param working_dir Working directory path
#' @return List of milestones with fixed IDs
get_milestones <- function(working_dir) {
  milestones <- .catch(get_milestones_impl(working_dir))

  # Convert all floating point values to character strings to prevent serialization issues
  lapply(milestones, function(milestone) {
    milestone$id <- safe_id_to_char(milestone$id)
    milestone$creator$id <- safe_id_to_char(milestone$creator$id)
    milestone
  })
}

#' Fix ID serialization issues for a single Issue object
#'
#' Converts floating point IDs to character strings to prevent serialization issues
#' when passing Issue objects to Rust functions.
#'
#' @param issue Single issue object with potential floating point ID fields
#' @return Issue object with IDs converted to character strings
fix_issue_ids <- function(issue) {
  # Fix issue ID
  if (!is.null(issue$id)) {
    issue$id <- safe_id_to_char(issue$id)
  }

  # Fix user IDs in various fields
  if (!is.null(issue$user) && !is.null(issue$user$id)) {
    issue$user$id <- safe_id_to_char(issue$user$id)
  }

  if (!is.null(issue$assignee) && !is.null(issue$assignee$id)) {
    issue$assignee$id <- safe_id_to_char(issue$assignee$id)
  }

  # Fix assignees (array of users)
  if (!is.null(issue$assignees) && length(issue$assignees) > 0) {
    issue$assignees <- lapply(issue$assignees, function(assignee) {
      if (!is.null(assignee$id)) {
        assignee$id <- safe_id_to_char(assignee$id)
      }
      assignee
    })
  }

  # Fix milestone ID if present
  if (!is.null(issue$milestone) && !is.null(issue$milestone$id)) {
    issue$milestone$id <- safe_id_to_char(issue$milestone$id)
    if (
      !is.null(issue$milestone$creator) &&
        !is.null(issue$milestone$creator$id)
    ) {
      issue$milestone$creator$id <- safe_id_to_char(issue$milestone$creator$id)
    }
  }

  # Fix label IDs
  if (!is.null(issue$labels) && length(issue$labels) > 0) {
    issue$labels <- lapply(issue$labels, function(label) {
      if (!is.null(label$id)) {
        label$id <- safe_id_to_char(label$id)
      }
      label
    })
  }

  issue
}

#' Get multiple milestone issues with ID fix applied
#'
#' Wrapper around get_multiple_milestone_issues_impl that automatically fixes floating
#' point ID serialization issues by converting IDs to character strings.
#'
#' @param milestones List of milestone objects
#' @param working_dir Working directory path
#' @return Named list of milestone issues with fixed IDs
get_multiple_milestone_issues <- function(milestones, working_dir) {
  multiple_milestone_issues <- .catch(get_multiple_milestone_issues_impl(
    working_dir,
    milestones
  ))

  # Fix floating point ID serialization issues for all issues
  lapply(multiple_milestone_issues, function(milestone_issues) {
    lapply(milestone_issues, fix_issue_ids)
  })
}

git_issue_modal_check <- function(
  git_statuses,
  duplicate_files = character(0)
) {
  # Check for duplicate issues
  has_duplicates <- length(duplicate_files) > 0

  # Check git status states
  has_errors <- any(!is.na(git_statuses$error_message))
  has_untracked <- any(!git_statuses$is_git_tracked, na.rm = TRUE)
  has_dirty <- any(git_statuses$git_status %in% "Local changes", na.rm = TRUE)
  has_blocking <- any(
    git_statuses$git_status %in%
      c("Ahead", "Behind", "Diverged", "Local commits", "Remote changes"),
    na.rm = TRUE
  )

  # If clean (up to date) and no duplicates, return NULL (no modal needed)
  if (
    !has_duplicates &&
      !has_errors &&
      !has_untracked &&
      !has_dirty &&
      !has_blocking
  ) {
    return(list(message = NULL, state = NULL))
  }

  # Build message with markdown headers
  messages <- c()

  # Add duplicate files section first (most critical)
  if (has_duplicates) {
    messages <- c(
      messages,
      "## ❌ Duplicate Issues",
      paste(
        "- **",
        duplicate_files,
        "**: Already exists as an issue in this milestone",
        sep = ""
      )
    )
  }

  if (has_errors) {
    error_files <- git_statuses[!is.na(git_statuses$error_message), ]
    messages <- c(
      messages,
      "## ❌ Git Errors",
      paste(
        "- **",
        error_files$file_path,
        "**: ",
        error_files$error_message,
        sep = ""
      )
    )
  }

  if (has_blocking) {
    blocking_files <- git_statuses[
      git_statuses$git_status %in%
        c("Ahead", "Behind", "Diverged", "Local commits", "Remote changes"),
    ]
    messages <- c(
      messages,
      "## 🚫 Sync Issues",
      paste(
        "- **",
        blocking_files$file_path,
        "**: ",
        blocking_files$git_status,
        sep = ""
      )
    )
  }

  if (has_dirty) {
    dirty_files <- git_statuses[git_statuses$git_status %in% "Local changes", ]
    messages <- c(
      messages,
      "## ⚠️ Local Changes",
      paste("- **", dirty_files$file_path, "**: Uncommitted changes", sep = "")
    )
  }

  if (has_untracked) {
    untracked_files <- git_statuses[!git_statuses$is_git_tracked, ]
    messages <- c(
      messages,
      "## 📝 Untracked Files",
      paste(
        "- **",
        untracked_files$file_path,
        "**: Not tracked by Git",
        sep = ""
      )
    )
  }

  # Determine state: error (blocking) or warning (can proceed)
  # Duplicates are always blocking errors
  state <- if (has_duplicates || has_errors || has_blocking || has_untracked) {
    "error"
  } else {
    "warning"
  }

  # Convert to HTML
  message_html <- markdown_to_html_impl(paste(messages, collapse = "\n\n"))

  return(list(message = message_html, state = state))
}
