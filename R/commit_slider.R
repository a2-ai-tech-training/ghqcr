#' Commit Range Slider Module
#'
#' A custom dual-handle slider for selecting commit ranges
#'
#' @param id Module ID
#' @param commits Data frame with columns: hash, message, qc_class, edits_file
#' @param default_from Default "from" commit (short SHA)
#' @param default_to Default "to" commit (short SHA)
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
#' @param commits Reactive data frame with columns: hash, message, qc_class, edits_file
commit_slider_server <- function(id, commits) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive slider UI - commits are already filtered by main app
    output$slider_ui <- shiny::renderUI({
      commits_df <- commits()

      if (is.null(commits_df) || nrow(commits_df) == 0) {
        return(
          shiny::div(
            shiny::p(
              "No commits available for timeline",
              style = "color: #999; font-style: italic; text-align: center; padding: 20px;"
            )
          )
        )
      }

      # Create commit labels (short hash only)
      commit_labels <- substr(commits_df$hash, 1, 7)

      # Calculate dynamic minimum width based on number of commits
      # Estimate: 7 chars * 6px per char + 20px spacing between commits
      chars_per_commit <- 7
      px_per_char <- 8
      spacing_between_commits <- 20
      commit_width <- chars_per_commit * px_per_char
      total_commits_width <- length(commit_labels) * commit_width
      total_spacing_width <- max(
        0,
        (length(commit_labels) - 1) * spacing_between_commits
      )
      calculated_min_width <- total_commits_width + total_spacing_width + 40 # +40 for padding

      # Cap the minimum width at 700px to accept overlap at high commit counts
      min_width <- min(calculated_min_width, 700)

      # Set defaults to last two commits if available
      default_from <- if (length(commit_labels) > 1) {
        commit_labels[length(commit_labels) - 1]
      } else {
        commit_labels[1]
      }
      default_to <- commit_labels[length(commit_labels)]

      shiny::div(
        # Custom dual-handle commit slider
        shiny::div(
          id = session$ns("commit_slider_container"),
          style = glue::glue(
            "margin: 20px auto; position: relative; width: calc(100vw - 40px); max-width: calc(100% - 40px); min-width: {min_width}px; padding: 0 20px;"
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

              # Grey out commits that don't edit the file (even when included)
              edits_file <- commits_df$edits_file[i]
              tick_color <- if (edits_file) "#666" else "#ccc"
              label_color <- if (edits_file) "black" else "#999"

              shiny::div(
                style = glue::glue(
                  "position: absolute; left: {left_pos}%; top: 20px; transform: translateX(-50%);"
                ),
                # Tick mark
                shiny::div(
                  style = glue::glue(
                    "width: 2px; height: 14px; background: {tick_color}; margin: 0 auto;"
                  )
                ),
                # Label
                shiny::div(
                  style = glue::glue(
                    "text-align: center; font-size: 10px; margin-top: 5px; white-space: nowrap; color: {label_color};"
                  ),
                  commit_labels[i]
                )
              )
            }),
            # Handle 1
            shiny::div(
              id = session$ns("handle1"),
              class = "commit-slider-handle",
              style = glue::glue(
                "position: absolute; top: 18px; width: 18px; height: 18px; background: #007bff; border: 2px solid white; border-radius: 50%; cursor: pointer; box-shadow: 0 1px 3px rgba(0,0,0,0.3); left: {if(length(commit_labels) == 1) 50 else (match(default_from, commit_labels)-1) / (length(commit_labels)-1) * 100}%; transform: translateX(-50%);"
              ),
              `data-commit` = default_from
            ),
            # Handle 2
            shiny::div(
              id = session$ns("handle2"),
              class = "commit-slider-handle",
              style = glue::glue(
                "position: absolute; top: 18px; width: 18px; height: 18px; background: #007bff; border: 2px solid white; border-radius: 50%; cursor: pointer; box-shadow: 0 1px 3px rgba(0,0,0,0.3); left: {if(length(commit_labels) == 1) 50 else (match(default_to, commit_labels)-1) / (length(commit_labels)-1) * 100}%; transform: translateX(-50%);"
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

              // Assign from/to based on commit order (earlier commit = from)
              if (handle1Index <= handle2Index) {{
                $('#' + fromInputId).val(handle1Commit).trigger('change');
                $('#' + toInputId).val(handle2Commit).trigger('change');
              }} else {{
                $('#' + fromInputId).val(handle2Commit).trigger('change');
                $('#' + toInputId).val(handle1Commit).trigger('change');
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
            updateFromToInputs();
          }});
        "
        ))),

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
