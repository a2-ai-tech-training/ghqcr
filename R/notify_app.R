ghqc_notify_app <- function(working_dir = here::here()) {
  app <- shiny::shinyApp(
    ui = ghqc_notify_ui(id = "ghqc_notify_app"),
    server = function(input, output, session) {
      ghqc_notify_server(
        id = "ghqc_notify_app",
        working_dir = working_dir
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454))
  shiny::runApp(app, port = port)
}

ghqc_notify_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- miniUI::miniPage(
    waiter::use_waiter(),
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "ghqcr/css/styles.css"
      ),
      shiny::tags$style(
        HTML(
          "
          /* Add gap between tabs and milestone filter */
          #ghqc_notify_app-type_tab {
            margin-bottom: 20px;
          }
          /* Reduce spacing between dropdowns and their associated checkboxes */
          #ghqc_notify_app-select_milestone {
            margin-bottom: 5px;
          }
          .shiny-input-container:has(#ghqc_notify_app-include_closed_milestones) {
            margin-top: 0px;
            margin-bottom: 15px;
          }
          #ghqc_notify_app-select_issue {
            margin-bottom: 5px;
          }
          .shiny-input-container:has(#ghqc_notify_app-include_more_issues) {
            margin-top: 0px;
            margin-bottom: 15px;
          }
        "
        )
      )
    ),
    waiter::waiter_show_on_load(
      html = shiny::tagList(
        waiter::spin_1(),
        shiny::h4("Loading in...", style = "color: white;")
      ),
      color = "darkgrey"
    ),
    shiny::div(
      id = ns("main_container"),
      miniUI::gadgetTitleBar(
        title = shiny::div(
          style = "display: inline-flex",
          shiny::div(
            style = "position: relative; flex-shrink: 0; width: 50px; height: 50px;",
            shiny::tags$img(
              src = "ghqcr/ghqc_hex.png",
              class = "logo-img",
              style = "height: 46px; !important;"
            ) # this is important to ensure style priority so logo is the correct size
          ),
          shiny::div("Post QC notification(s)", style = "white-space: nowrap;")
        ),
        left = shiny::actionButton(ns("close"), "Close", class = "btn-sm"),
        right = shiny::actionButton(ns("reset"), "Reset", class = "btn-sm")
      ),
      miniUI::miniContentPanel(
        shiny::div(
          id = ns("center_content"),
          shiny::tabsetPanel(
            id = ns("type_tab"),
            shiny::tabPanel(title = "Notify Changes"),
            shiny::tabPanel(title = "Approve"),
            shiny::tabPanel(title = "Unapprove")
          ),
          shiny::selectInput(
            ns("select_milestone"),
            "Filter Issues by Milestone",
            choices = "",
            multiple = FALSE
          ),
          shiny::checkboxInput(
            ns("include_closed_milestones"),
            "Include Closed Milestones",
            value = FALSE
          ),
          shiny::selectInput(
            ns("select_issue"),
            "Select Issue",
            choices = "",
            multiple = FALSE
          ),
          shiny::checkboxInput(
            ns("include_more_issues"),
            "Include Closed Issues",
            value = FALSE
          ),
          shiny::textAreaInput(
            ns("message"),
            "Message",
            "",
            placeholder = "(Optional)"
          ),
          shiny::div(
            id = ns("show_diff_wrap"),
            shiny::checkboxInput(ns("show_diff"), "Show file difference", TRUE)
          ),
          shiny::h4(
            id = ns("commits_header"),
            "Select commits to compare:",
          ),
          shiny::radioButtons(
            ns("include_non_editing"),
            "Commit filter:",
            choices = list(
              "Relevant commits only" = FALSE,
              "All commits" = TRUE
            ),
            selected = FALSE,
            inline = TRUE
          ),
          shiny::uiOutput(ns("commit_range_slider")),
          shiny::div(
            id = ns("from_commit_display"),
            style = "text-align: left; margin-bottom: 5px; margin-top: 20px;",
            shiny::span(
              "From: ",
              style = "font-weight: bold; margin-right: 5px;"
            ),
            shiny::textOutput(ns("from_commit_text"), inline = TRUE)
          ),
          shiny::div(
            id = ns("to_commit_display"),
            style = "text-align: left; margin-bottom: 10px;",
            shiny::span(
              "To: ",
              style = "font-weight: bold; margin-right: 5px;"
            ),
            shiny::textOutput(ns("to_commit_text"), inline = TRUE)
          )
        )
      ),
      shiny::div(
        class = "button_block",
        miniUI::miniButtonBlock(
          shiny::actionButton(ns("preview"), "Preview")
        )
      )
    )
  )
  return(ui)
}

ghqc_notify_server <- function(id, working_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    .le$debug("Fetching Milestones...")

    milestones <- get_milestones(working_dir)

    milestone_df <- purrr::map_dfr(milestones, function(x) {
      tibble::tibble(
        name = x$title,
        number = x$number,
        open = identical(x$state, "open")
      )
    })

    .le$debug(
      glue::glue(
        "Found {nrow(milestone_df)} milestones ({milestone_df |> dplyr::filter(open) |> nrow()} open)"
      )
    )

    # Smart loading system: track what we've loaded
    loaded_milestone_issues <- shiny::reactiveVal(list()) # Store all loaded milestone issues
    loaded_milestone_names <- shiny::reactiveVal(character(0)) # Track which milestones we've loaded

    # Initially load only open milestones and their issues
    open_milestones <- milestones[milestone_df$open]
    .le$debug("Loading issues for open milestones...")
    initial_milestone_issues <- get_multiple_milestone_issues(
      open_milestones,
      working_dir
    )
    initial_milestone_issues_count <- purrr::map(
      initial_milestone_issues,
      ~ length(.x)
    ) |>
      unlist() |>
      sum()
    .le$debug("Successfully loaded {initial_milestone_issues_count} issues")

    # Store the initial loaded data
    loaded_milestone_issues(initial_milestone_issues)
    loaded_milestone_names(names(initial_milestone_issues))

    reset_triggered <- shiny::reactiveVal(FALSE)
    session$onSessionEnded(function() {
      if (!isTRUE(shiny::isolate(reset_triggered()))) {
        stopApp()
      }
    })

    post_trigger <- shiny::reactiveVal(FALSE)

    # Reactive value to store current issue commits
    current_issue_commits <- shiny::reactiveVal(NULL)

    # Reactive value to track when we're ready to preview
    ready_to_preview <- shiny::reactiveVal(FALSE)

    # Reactive value to store commit slider result
    commit_slider_result <- shiny::reactiveVal(NULL)

    # Reactive value to store the current qc_comment for posting
    current_qc_comment <- shiny::reactiveVal(NULL)

    # Initialize validator for form validation
    iv <- shinyvalidate::InputValidator$new()

    # Track if message validation rule is active
    message_rule_active <- shiny::reactiveVal(FALSE)

    # Reactive to determine if Preview button should be enabled
    preview_enabled <- shiny::reactive({
      # Always check if an issue is selected
      if (is.null(input$select_issue) || input$select_issue == "") {
        return(FALSE)
      }

      # For Unapprove tab, check validation state
      if (input$type_tab == "Unapprove") {
        return(iv$is_valid())
      }

      # For other tabs, just need an issue selected
      return(TRUE)
    })

    # Update Preview button state based on validation
    shiny::observe({
      shinyjs::toggleState("preview", preview_enabled())
    })

    shiny::observe(waiter::waiter_hide())

    # Initialize the commit range slider with placeholder
    output$commit_range_slider <- shiny::renderUI({
      shiny::div(
        shiny::p(
          "Select an issue above to view its commit timeline",
          style = "color: #999; font-style: italic; text-align: center; padding: 20px;"
        )
      )
    })

    # Smart loading function
    load_missing_milestones <- function(needed_milestone_names) {
      current_loaded <- loaded_milestone_names()
      missing_names <- setdiff(needed_milestone_names, current_loaded)

      if (length(missing_names) > 0) {
        # Find the milestone objects for the missing ones
        missing_milestones <- milestones[sapply(milestones, function(m) {
          m$title %in% missing_names
        })]

        .le$debug(
          glue::glue(
            "Missing issues for the milestone(s): {paste0(missing_milestones, collapse = \", \")}"
          )
        )

        if (length(missing_milestones) > 0) {
          # Load issues for missing milestones
          new_milestone_issues <- get_multiple_milestone_issues(
            missing_milestones,
            working_dir
          )

          # Merge with existing loaded data
          current_issues <- loaded_milestone_issues()
          updated_issues <- c(current_issues, new_milestone_issues)
          loaded_milestone_issues(updated_issues)
          loaded_milestone_names(names(updated_issues))
        }
      }
    }

    # Reactive for filtered milestone names based on checkbox
    filtered_milestone_names <- shiny::reactive({
      shiny::req(milestone_df)

      if (nrow(milestone_df) == 0) {
        return(c())
      }

      if (input$include_closed_milestones) {
        .le$trace("Including all milestones...")
        # Include all milestones, sorted by number (reverse)
        milestone_df |>
          dplyr::arrange(dplyr::desc(number)) |>
          dplyr::pull(name)
      } else {
        .le$trace("Including closed milestones...")
        # Only open milestones, sorted by number (reverse)
        milestone_df |>
          dplyr::filter(open) |>
          dplyr::arrange(dplyr::desc(number)) |>
          dplyr::pull(name)
      }
    })

    # Reactive for current milestone issues with smart loading
    current_milestone_issues <- shiny::reactive({
      # Only load additional milestones if the checkbox input is available and checked
      if (
        !is.null(input$include_closed_milestones) &&
          isTRUE(input$include_closed_milestones)
      ) {
        needed_milestones <- milestone_df |> dplyr::pull(name)
        load_missing_milestones(needed_milestones)
      }

      # Return the currently loaded issues
      loaded_milestone_issues()
    })

    shiny::observeEvent(input$type_tab, {
      # For Unapprove tab: default to closed issues, checkbox allows including open
      # For other tabs: default to open issues, checkbox allows including closed
      if (input$type_tab == "Unapprove") {
        label <- "Include Open Issues"
        default_value <- FALSE  # Default to closed issues only

        # Hide commit-related UI for Unapprove tab
        shinyjs::hide("show_diff_wrap")
        shinyjs::hide("commits_header")
        shinyjs::hide("include_non_editing")
        shinyjs::hide("commit_range_slider")
        shinyjs::hide("from_commit_display")
        shinyjs::hide("to_commit_display")

        # Update message field to be required
        shiny::updateTextAreaInput(
          session,
          "message",
          label = "Message",
          placeholder = "Reason for Unapproval"
        )

        # Add validation rule for message field if not already active
        if (!message_rule_active()) {
          iv$add_rule("message", shinyvalidate::sv_required())
          message_rule_active(TRUE)
        }
        iv$enable()
      } else {
        label <- "Include Closed Issues"
        default_value <- FALSE  # Default to open issues only

        # Show commit-related UI for other tabs
        shinyjs::show("show_diff_wrap")
        shinyjs::show("commits_header")
        shinyjs::show("include_non_editing")
        shinyjs::show("commit_range_slider")
        shinyjs::show("from_commit_display")
        shinyjs::show("to_commit_display")

        # Update message field to be optional
        shiny::updateTextAreaInput(
          session,
          "message",
          label = "Message",
          placeholder = "(Optional)"
        )

        # Disable validator for other tabs
        iv$disable()
      }

      shiny::updateCheckboxInput(
        session,
        inputId = "include_more_issues",
        label = label,
        value = default_value
      )
    })

    # Update milestone choices when checkbox changes
    shiny::observe({
      milestone_names <- filtered_milestone_names()
      current_selection <- input$select_milestone

      # Preserve current selection if it's still valid
      new_choices <- c("All Issues", milestone_names)
      selected_value <- if (
        !is.null(current_selection) && current_selection %in% new_choices
      ) {
        current_selection
      } else {
        NULL # Let Shiny handle default selection
      }

      shiny::updateSelectizeInput(
        session,
        "select_milestone",
        choices = new_choices,
        selected = selected_value
      )
    })

    # Consolidated reactive for available issue choices
    # This combines milestone selection and include more issues filtering
    available_issue_choices <- shiny::reactive({
      # Require basic inputs
      shiny::req(input$select_milestone)

      # Get current milestone issues
      current_issues <- current_milestone_issues()
      milestone_issue_df <- flatten_multiple_milestone_issues(current_issues)

      # Filter by selected milestone
      filtered_issues <- if (input$select_milestone == "All Issues") {
        milestone_issue_df
      } else {
        milestone_issue_df |>
          dplyr::filter(milestone == input$select_milestone)
      }

      # Apply open/closed filtering based on tab and checkbox
      include_more <- isTRUE(input$include_more_issues)

      if (!include_more) {
        # Default behavior based on tab
        if (input$type_tab == "Unapprove") {
          # Unapprove tab: default to closed issues only
          filtered_issues <- filtered_issues |>
            dplyr::filter(open == FALSE)
        } else {
          # Other tabs: default to open issues only
          filtered_issues <- filtered_issues |>
            dplyr::filter(open == TRUE)
        }
      }
      # If include_more is TRUE, show all issues (no additional filtering)

      filtered_issues
    })

    # Consolidated observer for updating issue choices
    # Triggers on changes to milestone selection, include_more_issues checkbox, or type_tab
    shiny::observe({
      # Only update if we have a milestone selected
      shiny::req(input$select_milestone)

      # If "All Issues" is selected and we need closed milestones, load them
      if (input$select_milestone == "All Issues" && isTRUE(input$include_closed_milestones)) {
        all_milestone_names <- milestone_df |> dplyr::pull(name)
        load_missing_milestones(all_milestone_names)
      }

      # Get filtered issue choices using our consolidated reactive
      filtered_issues <- available_issue_choices()

      # Prepare issue choices
      current_issue_selection <- input$select_issue
      new_issue_choices <- filtered_issues |> format_issues()

      # Check if there are any issues available
      if (length(new_issue_choices) == 0) {
        # No issues available - show helpful message
        placeholder_msg <- if (input$type_tab == "Unapprove") {
          "No closed issues available"
        } else {
          "No open issues available"
        }

        # Create a single disabled choice that shows the message
        new_issue_choices <- setNames("", placeholder_msg)
        selected_issue_value <- NULL
      } else {
        # Issues are available - preserve selection if valid
        selected_issue_value <- if (
          !is.null(current_issue_selection) &&
            current_issue_selection %in% new_issue_choices
        ) {
          current_issue_selection
        } else {
          NULL # Let Shiny handle default selection
        }
      }

      shiny::updateSelectizeInput(
        session,
        "select_issue",
        choices = new_issue_choices,
        selected = selected_issue_value
      )
    })

    # Handle milestone loading when selection changes
    shiny::observeEvent(input$select_milestone, {
      shiny::req(input$select_milestone)

      # If a specific milestone is selected, ensure we have its issues loaded
      if (input$select_milestone != "All Issues") {
        load_missing_milestones(input$select_milestone)
      }
    })

    # Get commits when an issue is selected
    shiny::observeEvent(input$select_issue, {
      shiny::req(input$select_issue)

      .le$debug("Finding issue information for {input$select_issue}...")
      # Get current milestone issues and flatten for searching
      current_issues <- current_milestone_issues()
      milestone_issue_df <- flatten_multiple_milestone_issues(current_issues)

      # Find the selected issue from the flattened data
      selected_issue_info <- milestone_issue_df |>
        dplyr::filter(
          glue::glue("Issue {number}: {name}") == input$select_issue |
            glue::glue("Issue {number}: {name} (closed)") == input$select_issue
        ) |>
        dplyr::slice(1)

      if (nrow(selected_issue_info) == 0) {
        return()
      }

      # Find the actual issue object from loaded milestone issues
      milestone_name <- selected_issue_info$milestone
      issue_number <- selected_issue_info$number

      # Ensure we have the milestone loaded
      load_missing_milestones(milestone_name)
      current_issues <- loaded_milestone_issues()

      selected_issue <- NULL
      if (milestone_name %in% names(current_issues)) {
        milestone_issues <- current_issues[[milestone_name]]
        for (issue in milestone_issues) {
          if (issue$number == issue_number) {
            selected_issue <- issue
            break
          }
        }
      }

      if (is.null(selected_issue)) {
        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "Error",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
          ),
          "Could not find the selected issue. Please try again.",
          footer = NULL,
          easyClose = TRUE
        ))
        return()
      }

      # Get commits for this issue
      issue_commits <- tryCatch(
        {
          .le$debug("Getting commits for issue {input$select_issue}")
          .catch(get_issue_commits_impl(working_dir, selected_issue))
        },
        error = function(e) {
          .le$debug("Error getting issue commits: {e$message}")
          shiny::showModal(shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::modalButton("Return"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "Unable to Load Commits",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
            ),
            shiny::div(
              shiny::p("Could not retrieve commit history for this issue."),
              shiny::p("This may happen if:"),
              shiny::tags$ul(
                shiny::tags$li("The issue has no associated commits"),
                shiny::tags$li("There are network connectivity issues"),
                shiny::tags$li("The repository history is unavailable")
              ),
              shiny::p("The timeline will be disabled for this issue.")
            ),
            footer = NULL,
            easyClose = TRUE
          ))
          NULL
        }
      )

      # Update the commit range slider based on results
      if (is.null(issue_commits) || nrow(issue_commits) == 0) {
        .le$warn("There are no commits available for {input$select_issue}")
        # No commits available - disable timeline
        current_issue_commits(NULL)
        output$commit_range_slider <- shiny::renderUI({
          shiny::div(
            shiny::p(
              "No commit timeline available for this issue",
              style = "color: #999; font-style: italic; text-align: center; padding: 20px;"
            )
          )
        })
      } else {
        # Commits available - create timeline using module
        current_issue_commits(issue_commits)

        # Set defaults
        default_from <- substr(
          issue_commits$hash[max(1, nrow(issue_commits) - 1)],
          1,
          7
        )
        default_to <- substr(issue_commits$hash[nrow(issue_commits)], 1, 7)

        output$commit_range_slider <- shiny::renderUI({
          commit_slider_ui(session$ns("commit_slider"))
        })

        # Initialize commit slider server with checkbox input
        slider_result <- commit_slider_server(
          "commit_slider",
          reactive({
            if (is.null(current_issue_commits())) {
              return(NULL)
            }

            # Filter based on checkbox
            if (input$include_non_editing) {
              .le$debug(
                "Displaying all {length(current_issue_commits())} commits"
              )
              current_issue_commits()
            } else {
              # Show commits that either edit files OR have QC significance
              rel_commits <- current_issue_commits()[
                current_issue_commits()$edits_file == TRUE |
                  current_issue_commits()$qc_class != "no_comment",
              ]

              .le$debug(
                "Displaying {length(rel_commits)} relevant commits ({length(current_issue_commits())} available commits)"
              )

              rel_commits
            }
          })
        )

        # Store in reactive value for access elsewhere
        commit_slider_result(slider_result)

        # Connect module outputs to main app displays
        output$from_commit_text <- shiny::renderText({
          slider_result <- commit_slider_result()
          if (
            !is.null(slider_result) &&
              !is.null(slider_result$from_commit()) &&
              !is.null(current_issue_commits())
          ) {
            selected_sha <- slider_result$from_commit()
            .le$debug("From commit: {selected_sha}")
            commits_df <- current_issue_commits()
            matching_commit <- commits_df[
              substr(commits_df$hash, 1, 7) == selected_sha,
            ]
            if (nrow(matching_commit) > 0) {
              glue::glue("{selected_sha}: {matching_commit$message[1]}")
            } else {
              selected_sha
            }
          } else {
            .le$debug("No from commit selected")
            "No selection"
          }
        })

        output$to_commit_text <- shiny::renderText({
          slider_result <- commit_slider_result()
          if (
            !is.null(slider_result) &&
              !is.null(slider_result$to_commit()) &&
              !is.null(current_issue_commits())
          ) {
            selected_sha <- slider_result$to_commit()
            .le$debug("To commit: {selected_sha}")
            commits_df <- current_issue_commits()
            matching_commit <- commits_df[
              substr(commits_df$hash, 1, 7) == selected_sha,
            ]
            if (nrow(matching_commit) > 0) {
              glue::glue("{selected_sha}: {matching_commit$message[1]}")
            } else {
              selected_sha
            }
          } else {
            .le$debug("No to commit selected")
            "No selection"
          }
        })
      }
    })

    # Preview and post comment flow
    shiny::observeEvent(input$preview, {
      shiny::req(input$select_issue)
      .le$trace("Preview button clicked")

      # Extract file name from selected issue title
      # Issue titles should be in format like "QC [filename]" or similar
      selected_issue_text <- input$select_issue

      # Get current milestone issues and flatten for searching
      current_issues <- current_milestone_issues()
      milestone_issue_df <- flatten_multiple_milestone_issues(current_issues)

      # Find the selected issue object to get its title
      selected_issue_info <- milestone_issue_df |>
        dplyr::filter(
          glue::glue("Issue {number}: {name}") == selected_issue_text |
            glue::glue("Issue {number}: {name} (closed)") == selected_issue_text
        ) |>
        dplyr::slice(1)

      if (nrow(selected_issue_info) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "Error",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
          ),
          "Could not find the selected issue. Please try again.",
          footer = NULL,
          easyClose = TRUE
        ))
        return()
      }

      # Extract filename from issue title
      filename <- selected_issue_info$name

      # Get git status for the file
      git_status_result <- tryCatch(
        {
          .le$debug("Determining git status for {filename}")
          .catch(file_git_status_impl(c(filename), working_dir))
        },
        error = function(e) {
          .le$debug("Error getting git status: {e$message}")
          shiny::showModal(shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::modalButton("Return"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "Git Status Error",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
            ),
            glue::glue("Could not check git status for file: {filename}"),
            footer = NULL,
            easyClose = TRUE
          ))
          return(NULL)
        }
      )

      if (is.null(git_status_result)) {
        return()
      }

      # Check for git issues using the modal check function
      modal_result <- git_issue_modal_check(git_status_result, character(0))

      if (!is.null(modal_result$message)) {
        # Show modal with git status warnings/errors
        modal_title_text <- if (modal_result$state == "error") {
          "Git Issues Found - Cannot Proceed"
        } else {
          "Git Status Warning"
        }

        # Create title with appropriate buttons
        modal_title <- if (modal_result$state == "error") {
          # Error state: only Return button on left
          shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              modal_title_text,
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
          )
        } else {
          # Warning state: Return on left, Proceed Anyway on right
          shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              modal_title_text,
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(
              shiny::actionButton(
                "proceed_anyway",
                "Proceed Anyway",
                class = "btn-warning"
              ),
              style = "flex: 0 0 auto;"
            )
          )
        }

        shiny::showModal(shiny::modalDialog(
          title = modal_title,
          shiny::HTML(modal_result$message),
          footer = NULL,
          easyClose = modal_result$state != "error"
        ))

        # If it's just a warning, we could proceed, but for now just stop
        return()
      }

      # If we get here, git status is clean - proceed to next step (QC comment creation)
      ready_to_preview(TRUE)
    })

    # Handle proceed anyway button for warnings
    shiny::observeEvent(input$proceed_anyway, {
      shiny::removeModal()
      ready_to_preview(TRUE)
    })

    # Create QCComment and show preview when ready
    shiny::observeEvent(ready_to_preview(), {
      shiny::req(ready_to_preview() == TRUE)
      shiny::req(input$select_issue)

      .le$debug("Creating preview for {input$select_issue}...")

      # Reset the reactive val
      ready_to_preview(FALSE)

      # Get the current state for QCComment creation
      selected_issue_text <- input$select_issue

      # Get current milestone issues and flatten for searching
      current_issues <- current_milestone_issues()
      milestone_issue_df <- flatten_multiple_milestone_issues(current_issues)

      # Find the selected issue object to get its title and the actual issue
      selected_issue_info <- milestone_issue_df |>
        dplyr::filter(
          glue::glue("Issue {number}: {name}") == selected_issue_text |
            glue::glue("Issue {number}: {name} (closed)") == selected_issue_text
        ) |>
        dplyr::slice(1)

      if (nrow(selected_issue_info) == 0) {
        return()
      }

      # Find the actual issue object
      milestone_name <- selected_issue_info$milestone
      issue_number <- selected_issue_info$number

      # Ensure we have the milestone loaded
      load_missing_milestones(milestone_name)
      current_issues <- loaded_milestone_issues()

      selected_issue <- NULL
      if (milestone_name %in% names(current_issues)) {
        milestone_issues <- current_issues[[milestone_name]]
        for (issue in milestone_issues) {
          if (issue$number == issue_number) {
            selected_issue <- issue
            break
          }
        }
      }

      if (is.null(selected_issue)) {
        .le$debug("Did not find an associated issue")
        return()
      }

      # Check if commit slider is available
      slider_result <- commit_slider_result()
      if (is.null(slider_result) || is.null(current_issue_commits())) {
        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "No Commits Available",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
          ),
          "Please select an issue with available commits to preview.",
          footer = NULL,
          easyClose = TRUE
        ))
        return()
      }

      # Get commit selection (short hashes)
      from_commit_short <- slider_result$from_commit()
      to_commit_short <- slider_result$to_commit()
      message <- input$message
      show_diff <- input$show_diff

      if (is.null(from_commit_short) || is.null(to_commit_short)) {
        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "Commit Selection Required",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
          ),
          "Please select both 'from' and 'to' commits using the timeline slider.",
          footer = NULL,
          easyClose = TRUE
        ))
        return()
      }

      # Look up full commit hashes from commits_df
      commits_df <- current_issue_commits()
      from_commit_match <- commits_df[
        substr(commits_df$hash, 1, 7) == from_commit_short,
      ]
      to_commit_match <- commits_df[
        substr(commits_df$hash, 1, 7) == to_commit_short,
      ]

      if (nrow(from_commit_match) == 0 || nrow(to_commit_match) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "Commit Not Found",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
          ),
          "Could not find the selected commits in the timeline.",
          footer = NULL,
          easyClose = TRUE
        ))
        return()
      }

      # Get full hashes
      from_commit <- from_commit_match$hash[1]
      to_commit <- to_commit_match$hash[1]

      # Extract filename from issue title
      filename <- selected_issue_info$name

      # Validate filename
      if (is.null(filename) || is.na(filename) || nchar(filename) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "Invalid Filename",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
          ),
          "The selected issue does not have a valid filename.",
          footer = NULL,
          easyClose = TRUE
        ))
        return()
      }

      # Create QCComment with additional safety checks
      qc_comment <- tryCatch(
        {
          # Additional validation
          if (nchar(from_commit) != 40 || nchar(to_commit) != 40) {
            stop(glue::glue(
              "Invalid commit hash lengths - from: {nchar(from_commit)}, to: {nchar(to_commit)}"
            ))
          }

          qc_comment <- .catch(create_qc_comment_impl(
            fix_issue_ids(selected_issue),
            filename,
            from_commit,
            to_commit,
            if (nchar(message) > 0) message else NULL,
            show_diff
          ))
          qc_comment$issue <- fix_issue_ids(qc_comment$issue)

          # Store the qc_comment for later use in posting
          current_qc_comment(qc_comment)

          qc_comment
        },
        error = function(e) {
          .le$debug("Error creating QC comment: {e$message}")
          shiny::showModal(shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::modalButton("Return"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "QC Comment Error",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
            ),
            glue::glue("Could not create QC comment: {e$message}"),
            footer = NULL,
            easyClose = TRUE
          ))
          return(NULL)
        }
      )

      if (is.null(qc_comment)) {
        return()
      }

      # Get HTML body from QC comment
      comment_html <- tryCatch(
        {
          .catch(get_qc_comment_body_html_impl(qc_comment, working_dir))
        },
        error = function(e) {
          .le$debug("Error getting QC comment body: {e$message}")
          shiny::showModal(shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::modalButton("Return"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "Preview Error",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
            ),
            glue::glue("Could not generate comment preview: {e$message}"),
            footer = NULL,
            easyClose = TRUE
          ))
          return(NULL)
        }
      )

      if (is.null(comment_html)) {
        .le$warn("Comment preview is null. Not showing preview modal...")
        return()
      }

      # Show preview modal with HTML body
      shiny::showModal(shiny::modalDialog(
        title = shiny::tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
          shiny::tags$div(
            shiny::modalButton("Return"),
            style = "flex: 0 0 auto;"
          ),
          shiny::tags$div(
            "Preview QC Comment",
            style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
          ),
          shiny::tags$div(
            shiny::actionButton(
              session$ns("post_comment"),
              "Post Comment",
              class = "btn-primary"
            ),
            style = "flex: 0 0 auto;"
          )
        ),
        size = "l",
        shiny::div(
          style = "max-height: 500px; overflow-y: auto; border: 1px solid #ddd; padding: 15px; background-color: #f8f9fa;",
          shiny::HTML(comment_html)
        ),
        footer = NULL,
        easyClose = FALSE
      ))
    })

    # Handle posting the comment when Post Comment button is clicked
    shiny::observeEvent(input$post_comment, {
      .le$trace("Post Comment button clicked")
      shiny::removeModal() # Close the preview modal

      # Get the stored qc_comment from the reactive value
      qc_comment <- current_qc_comment()

      if (is.null(qc_comment)) {
        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "Post Error",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;")
          ),
          "No QC comment available for posting. Please try previewing first.",
          footer = NULL,
          easyClose = TRUE
        ))
        return()
      }

      # Post the comment
      post_result <- tryCatch(
        {
          .le$info("Posting QC comment...")
          .catch(post_qc_comment_impl(qc_comment, working_dir))
        },
        error = function(e) {
          .le$debug("Error posting QC comment: {e$message}")
          shiny::showModal(shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::modalButton("Return"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "Post Error",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;")
            ),
            glue::glue("Could not post QC comment: {e$message}"),
            footer = NULL,
            easyClose = TRUE
          ))
          return(NULL)
        }
      )
      .le$debug("Successfully posted QC comment!")

      if (!is.null(post_result)) {
        # Success! Show success modal
        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "Success",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;")
          ),
          shiny::div(
            shiny::p("QC comment posted successfully!"),
            shiny::HTML(markdown_to_html_impl(glue::glue(
              "[Click here to view comment on GitHub]({post_result})"
            )))
          ),
          footer = NULL,
          easyClose = TRUE
        ))
      }
    })

    # Handle close button
    shiny::observeEvent(input$close, {
      shiny::stopApp()
    })

    # Handle reset button
    shiny::observeEvent(input$reset, {
      reset_triggered(TRUE)
      session$reload()
    })
  })
}

# takes in a list of milestones, with sublist issues and
# returns a df with columns Milestone, Number, Name, Open, MilestoneNumber
flatten_multiple_milestone_issues <- function(multiple_milestone_issues) {
  purrr::imap_dfr(
    multiple_milestone_issues,
    function(milestone_issues, milestone_name) {
      purrr::map_dfr(milestone_issues, function(issue) {
        # Extract milestone number from the issue's milestone object
        milestone_number <- if (
          !is.null(issue$milestone) && !is.null(issue$milestone$number)
        ) {
          issue$milestone$number
        } else {
          # Fallback: extract number from milestone name if it follows a pattern
          # This handles cases where milestone name is like "v1.0", "Release 2.1", etc.
          milestone_name_pattern <- stringr::str_implact(
            milestone_name,
            "\\d+(\\.\\d+)*"
          )
          if (!is.na(milestone_name_pattern)) {
            as.numeric(stringr::str_replace_all(
              milestone_name_pattern,
              "\\.",
              ""
            ))
          } else {
            0 # Default fallback
          }
        }

        tibble::tibble(
          milestone = milestone_name,
          milestone_number = milestone_number,
          number = issue$number,
          name = issue$title,
          open = identical(issue$state, "open")
        )
      })
    }
  )
}

format_issues <- function(milestone_issue_df) {
  milestone_issue_df |>
    dplyr::arrange(dplyr::desc(milestone_number), dplyr::desc(number)) |>
    dplyr::transmute(
      open_disp = dplyr::if_else(open, "", " (closed)"),
      disp = glue::glue(
        "Issue {number}: {name}{open_disp}"
      )
    ) |>
    dplyr::pull(disp)
}
