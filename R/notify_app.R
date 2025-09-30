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
          shiny::selectInput(
            ns("select_milestone"),
            "Filter Issues by Milestone",
            choices = "",
            multiple = FALSE
          ),
          shiny::selectInput(
            ns("select_issue"),
            "Select Issue",
            choices = "",
            multiple = FALSE
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
            "Select commits to compare:",
          ),
          shiny::radioButtons(
            ns("include_non_editing"),
            "Commit filter:",
            choices = list(
              "File-editing commits only" = FALSE,
              "All commits" = TRUE
            ),
            selected = FALSE,
            inline = TRUE
          ),
          shiny::uiOutput(ns("commit_range_slider")),
          shiny::div(
            id = ns("from_commit_display"),
            style = "text-align: left; margin-bottom: 5px;",
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
    .le$debug("Getting Milestones...")

    milestones <- get_milestones(working_dir)

    milestone_df <- purrr::map_dfr(milestones, function(x) {
      tibble::tibble(
        name = x$title,
        open = identical(x$state, "open")
      )
    })

    open_milestones <- milestones[milestone_df$open]
    multiple_milestone_issues <- get_multiple_milestone_issues(
      open_milestones,
      working_dir
    )
    milestone_issue_df <- flatten_multiple_milestone_issues(
      multiple_milestone_issues
    )
    .le$debug("Successfully fetched Milestones and Issues for open Milestones")

    reset_triggered <- shiny::reactiveVal(FALSE)
    session$onSessionEnded(function() {
      if (!isTRUE(shiny::isolate(reset_triggered()))) {
        stopApp()
      }
    })

    post_trigger <- shiny::reactiveVal(FALSE)

    # Reactive value to store current issue commits
    current_issue_commits <- shiny::reactiveVal(NULL)

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

    shiny::observe({
      shiny::req(milestone_df)

      open_milestone_names <- if (nrow(milestone_df) == 0) {
        c()
      } else {
        milestone_df |>
          dplyr::filter(open) |>
          dplyr::pull(name)
      }

      shiny::updateSelectizeInput(
        session,
        "select_milestone",
        choices = c("All Issues", open_milestone_names)
      )
    })

    shiny::observeEvent(input$select_milestone, {
      issue_choices <- if (input$select_milestone == "All Issues") {
        milestone_issue_df
      } else {
        milestone_issue_df |>
          dplyr::filter(milestone == input$select_milestone)
      }

      shiny::updateSelectizeInput(
        session,
        "select_issue",
        choices = issue_choices |> format_issues()
      )
    })

    # Get commits when an issue is selected
    shiny::observeEvent(input$select_issue, {
      shiny::req(input$select_issue)

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

      # Find the actual issue object from multiple_milestone_issues
      milestone_name <- selected_issue_info$milestone
      issue_number <- selected_issue_info$number

      selected_issue <- NULL
      if (milestone_name %in% names(multiple_milestone_issues)) {
        milestone_issues <- multiple_milestone_issues[[milestone_name]]
        for (issue in milestone_issues) {
          if (issue$number == issue_number) {
            selected_issue <- issue
            break
          }
        }
      }

      if (is.null(selected_issue)) {
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          "Could not find the selected issue. Please try again.",
          footer = shiny::modalButton("OK"),
          easyClose = TRUE
        ))
        return()
      }

      # Get commits for this issue
      issue_commits <- tryCatch(
        {
          .catch(get_issue_commits_extr(selected_issue, working_dir))
        },
        error = function(e) {
          .le$debug(glue::glue("Error getting issue commits: {e$message}"))
          shiny::showModal(shiny::modalDialog(
            title = "Unable to Load Commits",
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
            footer = shiny::modalButton("OK"),
            easyClose = TRUE
          ))
          NULL
        }
      )

      # Update the commit range slider based on results
      if (is.null(issue_commits) || nrow(issue_commits) == 0) {
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
        commit_slider_result <- commit_slider_server(
          "commit_slider",
          reactive({
            if (is.null(current_issue_commits())) {
              return(NULL)
            }

            # Filter based on checkbox
            if (input$include_non_editing) {
              current_issue_commits()
            } else {
              current_issue_commits()[
                current_issue_commits()$edits_file == TRUE,
              ]
            }
          })
        )

        # Connect module outputs to main app displays
        output$from_commit_text <- shiny::renderText({
          if (
            !is.null(commit_slider_result$from_commit()) &&
              !is.null(current_issue_commits())
          ) {
            selected_sha <- commit_slider_result$from_commit()
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
            "No selection"
          }
        })

        output$to_commit_text <- shiny::renderText({
          if (
            !is.null(commit_slider_result$to_commit()) &&
              !is.null(current_issue_commits())
          ) {
            selected_sha <- commit_slider_result$to_commit()
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
            "No selection"
          }
        })
      }
    })
  })
}

# takes in a list of milestones, with sublist issues and
# returns a df with columns Milestone, Number, Name, Open
flatten_multiple_milestone_issues <- function(multiple_milestone_issues) {
  purrr::imap_dfr(
    multiple_milestone_issues,
    function(milestone_issues, milestone_name) {
      purrr::map_dfr(milestone_issues, function(issue) {
        tibble::tibble(
          milestone = milestone_name,
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
    dplyr::transmute(
      open_disp = dplyr::if_else(open, "", " (closed)"),
      disp = glue::glue(
        "Issue {number}: {name}{open_disp}"
      )
    ) |>
    dplyr::pull(disp)
}
