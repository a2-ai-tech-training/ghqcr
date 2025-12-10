# Helper functions for multi-status commit system
parse_commit_statuses <- function(statuses_str) {
  if (is.na(statuses_str) || statuses_str == "") {
    return(character(0))
  }
  trimws(strsplit(statuses_str, ",")[[1]])
}

has_status <- function(statuses_str, status) {
  status %in% parse_commit_statuses(statuses_str)
}

is_reviewed <- function(statuses_str) {
  has_status(statuses_str, "reviewed")
}

get_status_tooltip <- function(statuses) {
  if (length(statuses) == 0) return("No status")
  paste("Statuses:", paste(statuses, collapse = ", "))
}

#' Commit Range Slider Module
#'
#' A custom dual-handle slider for selecting commit ranges
#'
#' @param id Module ID
#' @importFrom jsonlite toJSON
commit_slider_ui <- function(id) {
  ns <- shiny::NS(id)

  # Just the slider container - centered for visual balance
  shiny::div(
    style = "display: flex; flex-direction: column; align-items: center; width: 100%;",
    shiny::uiOutput(ns("slider_ui"))
  )
}

#' Commit Range Slider Server
#'
#' @param id Module ID
#' @param commits Reactive data frame with columns: hash, message, statuses, edits_file
#' @param single_select Reactive logical, if TRUE shows single handle for single commit selection
#' @param tab_type Reactive character, the current tab type for styling
#' @param default_commit Reactive character, optional specific commit to use as default (short hash)
commit_slider_server <- function(
  id,
  commits,
  single_select = reactive(FALSE),
  tab_type = reactive("Notify Changes"),
  default_commit = reactive(NULL)
) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive slider UI - commits are already filtered by main app
    output$slider_ui <- shiny::renderUI({
      commits_df <- commits()

      if (is_empty(commits_df)) {
        return(
          shiny::div(
            shiny::p(
              "No commits available for timeline",
              style = "color: #999; font-style: italic; text-align: center; padding: 20px;"
            )
          )
        )
      }

      # Reverse commit order so most recent is on the right (chronological order)
      commits_df <- commits_df[nrow(commits_df):1, ]

      # Create commit labels (short hash only)
      commit_labels <- substr(commits_df$hash, 1, 7)

      # Calculate dynamic minimum width based on number of commits
      # Reduced spacing to fit more commits: 7 chars * 6px per char + 8px spacing between commits
      chars_per_commit <- 7
      px_per_char <- 6 # Reduced from 8 to 6
      spacing_between_commits <- 8 # Reduced from 20 to 8
      commit_width <- chars_per_commit * px_per_char
      total_commits_width <- length(commit_labels) * commit_width
      total_spacing_width <- max(
        0,
        (length(commit_labels) - 1) * spacing_between_commits
      )
      calculated_min_width <- total_commits_width + total_spacing_width + 20 # +20 for reduced padding

      # Cap the minimum width at 800px to accommodate more commits with tighter spacing
      min_width <- min(calculated_min_width, 800)

      # Set defaults based on sophisticated logic
      # Find latest file-changing commit
      file_changing_commits <- which(commits_df$edits_file == TRUE)
      latest_file_changing <- if (length(file_changing_commits) > 0) {
        max(file_changing_commits)
      } else {
        NULL
      }

      # Find latest commit with any status
      commits_with_status <- which(sapply(commits_df$statuses, function(s) length(parse_commit_statuses(s)) > 0))
      latest_with_status <- if (length(commits_with_status) > 0) {
        max(commits_with_status)
      } else {
        NULL
      }

      # Implement default selection logic
      if (single_select()) {
        # Check if a specific default commit is provided and valid
        if (!is.null(default_commit()) && default_commit() %in% commit_labels) {
          default_to <- default_commit()
          default_from <- default_commit()
        } else {
          # Single select: just pick the latest relevant commit
          if (!is.null(latest_with_status)) {
            default_to <- commit_labels[latest_with_status]
          } else if (!is.null(latest_file_changing)) {
            default_to <- commit_labels[latest_file_changing]
          } else {
            default_to <- commit_labels[length(commit_labels)]
          }
          default_from <- default_to # Same commit for both (only "to" will be shown)
        }
      } else {
        # Range selection logic (existing)
        if (!is.null(latest_file_changing) && !is.null(latest_with_status)) {
          if (latest_file_changing > latest_with_status) {
            # Case 1: File-changing commit occurs after latest commit with status
            default_from <- commit_labels[latest_with_status]
            default_to <- commit_labels[latest_file_changing]
          } else {
            # One selector on commit with status, other tries to be different file-changing
            default_to <- commit_labels[latest_with_status]

            # Try to find a different file-changing commit
            other_file_changing <- file_changing_commits[
              file_changing_commits != latest_with_status
            ]
            if (length(other_file_changing) > 0) {
              default_from <- commit_labels[max(other_file_changing)]
            } else {
              # Fall back to previous commit if available
              default_from <- if (latest_with_status > 1) {
                commit_labels[latest_with_status - 1]
              } else {
                commit_labels[min(2, length(commit_labels))]
              }
            }
          }
        } else if (!is.null(latest_with_status)) {
          # Only commits with status available
          default_to <- commit_labels[latest_with_status]
          default_from <- if (latest_with_status > 1) {
            commit_labels[latest_with_status - 1]
          } else {
            commit_labels[min(2, length(commit_labels))]
          }
        } else {
          # Fallback to simple last two commits
          default_from <- if (length(commit_labels) > 1) {
            commit_labels[length(commit_labels) - 1]
          } else {
            commit_labels[1]
          }
          default_to <- commit_labels[length(commit_labels)]
        }
      }

      shiny::div(
        # Custom dual-handle commit slider
        shiny::div(
          id = session$ns("commit_slider_container"),
          style = glue::glue(
            "margin: 20px auto; position: relative; width: calc(100vw - 20px); max-width: calc(100% - 20px); min-width: {min_width}px; padding: 0 10px;"
          ),

          # Hidden inputs to store values for Shiny
          shiny::tags$input(
            type = "text",
            id = session$ns("from_commit_select"),
            value = default_from,
            style = "display: none;",
            class = "shiny-input-text"
          ),
          shiny::tags$input(
            type = "text",
            id = session$ns("to_commit_select"),
            value = default_to,
            style = "display: none;",
            class = "shiny-input-text"
          ),

          # Slider container
          shiny::div(
            style = "position: relative; height: 60px; margin: 20px 0; padding: 0; width: 100%; max-width: 100%;",
            # Slider track - full width
            shiny::div(
              style = "position: absolute; top: 25px; left: 0; right: 0; height: 4px; background: #ddd; border-radius: 2px;"
            ),
            # Commit markers and labels
            lapply(1:nrow(commits_df), function(i) {
              if (nrow(commits_df) == 1) {
                left_pos <- 50
              } else {
                left_pos <- (i - 1) / (nrow(commits_df) - 1) * 100
              }

              # Style based on edits_file and multi-status system
              edits_file <- commits_df$edits_file[i]
              statuses <- parse_commit_statuses(commits_df$statuses[i])
              reviewed <- is_reviewed(commits_df$statuses[i])

              # Always render exactly two dots in the same structure
              # This ensures consistent spacing regardless of visibility

              # Primary status dot color based on priority
              primary_color <- if (length(statuses) == 0) {
                "transparent"
              } else if ("approved" %in% statuses) {
                "#28a745" # Green for approved
              } else if ("notification" %in% statuses) {
                "#ffc107" # Yellow for notification
              } else if ("initial" %in% statuses) {
                "#007bff" # Blue for initial
              } else if ("reviewed" %in% statuses) {
                "#6c757d" # Gray for reviewed only
              } else {
                "#007bff" # Default fallback
              }

              # Review dot color
              review_color <- if (reviewed) "#fd7e14" else "transparent"

              # Always create exactly the same structure: container with two dots
              # Handle "falling" logic: if only review (no primary QC), review falls to bottom
              has_primary <- length(statuses) > 0 && any(statuses %in% c("approved", "notification", "initial"))
              has_review <- reviewed

              if (has_primary && has_review) {
                # Both dots: review on top, primary on bottom
                qc_indicators <- shiny::div(
                  style = "margin: -8px auto 4px auto;",
                  # Review dot (top)
                  shiny::div(
                    style = glue::glue(
                      "width: 8px; height: 8px; background: {review_color}; border-radius: 50%; margin: 0 auto 2px auto;"
                    ),
                    title = "Reviewed"
                  ),
                  # Primary QC dot (bottom)
                  shiny::div(
                    style = glue::glue(
                      "width: 8px; height: 8px; background: {primary_color}; border-radius: 50%; margin: 0 auto 0px auto;"
                    ),
                    title = get_status_tooltip(statuses)
                  )
                )
              } else if (has_review) {
                # Only review: falls to bottom position
                qc_indicators <- shiny::div(
                  style = "margin: -8px auto 4px auto;",
                  # Empty top slot
                  shiny::div(
                    style = "width: 8px; height: 8px; background: transparent; border-radius: 50%; margin: 0 auto 2px auto;",
                    title = ""
                  ),
                  # Review dot (bottom)
                  shiny::div(
                    style = glue::glue(
                      "width: 8px; height: 8px; background: {review_color}; border-radius: 50%; margin: 0 auto 0px auto;"
                    ),
                    title = "Reviewed"
                  )
                )
              } else {
                # Only primary or neither: primary in bottom position, top transparent
                qc_indicators <- shiny::div(
                  style = "margin: -8px auto 4px auto;",
                  # Empty top slot
                  shiny::div(
                    style = "width: 8px; height: 8px; background: transparent; border-radius: 50%; margin: 0 auto 2px auto;",
                    title = ""
                  ),
                  # Primary QC dot (bottom) or transparent
                  shiny::div(
                    style = glue::glue(
                      "width: 8px; height: 8px; background: {primary_color}; border-radius: 50%; margin: 0 auto 0px auto;"
                    ),
                    title = get_status_tooltip(statuses)
                  )
                )
              }

              # Dim colors if file not edited
              if (!edits_file) {
                tick_color <- "#ccc"
                label_color <- "#999"
              } else {
                tick_color <- "#666"
                label_color <- "black"
              }

              shiny::div(
                style = glue::glue(
                  "position: absolute; left: {left_pos}%; top: 6px; transform: translateX(-50%);"
                ),
                # QC class indicator (same structure as original)
                qc_indicators,
                # Tick mark
                shiny::div(
                  style = glue::glue(
                    "width: 2px; height: 14px; background: {tick_color}; margin: 0px auto 0px auto;"
                  )
                ),
                # Label
                shiny::div(
                  style = glue::glue(
                    "text-align: center; font-size: 9px; margin-top: 5px; white-space: nowrap; color: {label_color};"
                  ),
                  commit_labels[i]
                )
              )
            }),
            # Handle 1 (only show in range mode)
            if (!single_select()) {
              shiny::div(
                id = session$ns("handle1"),
                class = "commit-slider-handle",
                style = glue::glue(
                  "position: absolute; top: 18px; width: 18px; height: 18px; background: #007bff; border: 2px solid white; border-radius: 50%; cursor: pointer; box-shadow: 0 1px 3px rgba(0,0,0,0.3); left: {if(length(commit_labels) == 1) 50 else (match(default_from, commit_labels)-1) / (length(commit_labels)-1) * 100}%; transform: translateX(-50%);"
                ),
                `data-commit` = default_from
              )
            },
            # Handle 2 (always show, represents selected commit in single mode)
            shiny::div(
              id = session$ns("handle2"),
              class = "commit-slider-handle",
              style = glue::glue(
                "position: absolute; top: 18px; width: 18px; height: 18px; background: {
                    if(single_select()) {
                      if(tab_type() == 'Review') '#fd7e14' else '#28a745'
                    } else '#007bff'
                  }; border: 2px solid white; border-radius: 50%; cursor: pointer; box-shadow: 0 1px 3px rgba(0,0,0,0.3); left: {if(length(commit_labels) == 1) 50 else (match(default_to, commit_labels)-1) / (length(commit_labels)-1) * 100}%; transform: translateX(-50%);"
              ),
              `data-commit` = default_to
            )
          )
        ),

        # JavaScript for slider functionality
        shiny::tags$script(shiny::HTML(glue::glue(
          "
          $(document).ready(function() {{
            const commits = {jsonlite::toJSON(commit_labels)};
            const containerId = '{session$ns('commit_slider_container')}';
            const handle1Id = '{session$ns('handle1')}';
            const handle2Id = '{session$ns('handle2')}';
            const fromInputId = '{session$ns('from_commit_select')}';
            const toInputId = '{session$ns('to_commit_select')}';
            const singleSelect = {jsonlite::toJSON(single_select())};
            const tabType = {jsonlite::toJSON(tab_type())}[0]; // Get first element from array

            function getCommitPosition(commit) {{
              const index = commits.indexOf(commit);
              if (commits.length === 1) return 50;
              return (index / (commits.length - 1)) * 100;
            }}

            function getClosestCommit(percentage) {{
              if (commits.length === 1) return commits[0];
              const index = Math.round((percentage / 100) * (commits.length - 1));
              return commits[Math.max(0, Math.min(commits.length - 1, index))];
            }}

            function updateHandle(handleId, commit) {{
              const pos = getCommitPosition(commit);
              $('#' + handleId).css('left', pos + '%');
              $('#' + handleId).attr('data-commit', commit);
            }}

            function updateFromToInputs() {{
              // Get both handle commits and their positions
              const handle1Commit = $('#' + handle1Id).attr('data-commit');
              const handle2Commit = $('#' + handle2Id).attr('data-commit');
              const handle1Index = commits.indexOf(handle1Commit);
              const handle2Index = commits.indexOf(handle2Commit);

              // Special handling for Review tab in single select mode
              if (singleSelect && tabType === 'Review') {{
                // For Review tab, the selected commit (handle2) should be the from commit
                console.log('Review tab: setting from_commit to', handle2Commit);
                $('#' + fromInputId).val(handle2Commit).trigger('change');
                $('#' + toInputId).val('').trigger('change'); // Clear to_commit for Review
              }} else {{
                // Normal logic: assign from/to based on commit order (earlier commit = from)
                if (handle1Index <= handle2Index) {{
                  $('#' + fromInputId).val(handle1Commit).trigger('change');
                  $('#' + toInputId).val(handle2Commit).trigger('change');
                }} else {{
                  $('#' + fromInputId).val(handle2Commit).trigger('change');
                  $('#' + toInputId).val(handle1Commit).trigger('change');
                }}
              }}
            }}

            $('.commit-slider-handle').on('mousedown', function(e) {{
              e.preventDefault();
              const handle = $(this);
              const container = $('#' + containerId + ' > div').first();
              const containerOffset = container.offset().left;
              const containerWidth = container.width();

              $(document).on('mousemove.slider', function(e) {{
                const x = e.pageX - containerOffset;
                const percentage = Math.max(0, Math.min(100, (x / containerWidth) * 100));
                const commit = getClosestCommit(percentage);

                updateHandle(handle.attr('id'), commit);
                updateFromToInputs();
              }});

              $(document).on('mouseup.slider', function() {{
                $(document).off('.slider');
              }});
            }});

            // Initialize from/to on load
            console.log('Initializing commit slider, tabType:', tabType, 'singleSelect:', singleSelect);
            updateFromToInputs();
          }});
        "
        ))),

        # Legend for QC classes
        shiny::div(
          style = "margin-top: 15px; text-align: center; font-size: 12px;",
          shiny::div(
            style = "display: inline-flex; gap: 15px; align-items: center;",
            shiny::div(
              style = "display: flex; align-items: center; gap: 5px;",
              shiny::div(
                style = "width: 8px; height: 8px; background: #007bff; border-radius: 50%;"
              ),
              shiny::span("Initial")
            ),
            shiny::div(
              style = "display: flex; align-items: center; gap: 5px;",
              shiny::div(
                style = "width: 8px; height: 8px; background: #ffc107; border-radius: 50%;"
              ),
              shiny::span("Notification")
            ),
            shiny::div(
              style = "display: flex; align-items: center; gap: 5px;",
              shiny::div(
                style = "width: 8px; height: 8px; background: #28a745; border-radius: 50%;"
              ),
              shiny::span("Approval")
            ),
            shiny::div(
              style = "display: flex; align-items: center; gap: 5px;",
              shiny::div(
                style = "width: 8px; height: 8px; background: #fd7e14; border-radius: 50%;"
              ),
              shiny::span("Review")
            )
          )
        )
      )
    })

    # Return reactive values for the selected commits
    return(
      list(
        from_commit = shiny::reactive({
          input$from_commit_select
        }),
        to_commit = shiny::reactive({
          input$to_commit_select
        })
      )
    )
  })
}
