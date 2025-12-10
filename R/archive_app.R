#' Launch GHQC Archive App
#'
#' Opens a Shiny application for creating archives of QC files with their
#' associated milestones and commit information.
#'
#' @param working_dir Character. Path to the working directory containing the
#'   git repository. Defaults to the current working directory via here::here().
#'
#' @return Launches a Shiny app (no return value).
#'
#' @export
#' @examples
#' \dontrun{
#' # Launch archive app in current directory
#' ghqc_archive_app()
#'
#' # Launch archive app in specific directory
#' ghqc_archive_app("/path/to/repo")
#' }
ghqc_archive_app <- function(
  working_dir = here::here()
) {
  app <- shiny::shinyApp(
    ui = ghqc_archive_ui(id = "ghqc_archive_app"),
    server = function(input, output, session) {
      ghqc_archive_server(
        id = "ghqc_archive_app",
        working_dir = working_dir
      )
    }
  )

  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5254))
  shiny::runApp(app, port = port)
}

ghqc_archive_ui <- function(id) {
  ns <- shiny::NS(id)
  miniUI::miniPage(
    waiter::use_waiter(),
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "ghqc/css/styles.css"
      ),
      shiny::tags$script(type = "module", src = "ghqc/js/adjust_grid.js"),
      shiny::tags$script(
        type = "module",
        src = "ghqc/js/toggle_sidebar.js"
      ),
      shiny::tags$style(shiny::HTML(
        "
    ::placeholder {
      color: #8e8e8e; /* match colors of placeholders */
    }

    /* Fix transparent overlay issue */
    .mini-content {
      background: transparent !important;
      position: relative !important;
    }

    .mini-content::before,
    .mini-content::after {
      display: none !important;
    }
  "
      ))
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
          style = "display: inline-flex; align-items: center; justify-content: center; width: 100%; height: 100%;",
          shiny::div(
            style = "position: relative; flex-shrink: 0; width: 50px; height: 50px;",
            shiny::tags$img(
              src = "ghqc/ghqc_hex.png",
              class = "logo-img",
              style = "height: 46px; !important;"
            ) # this is important to ensure style priority so logo is the correct size
          ),
          shiny::div("Archive QC file(s)", style = "white-space: nowrap;")
        ),
        left = shiny::actionButton(ns("close"), "Close", class = "btn-sm"),
        right = shiny::actionButton(ns("reset"), "Reset", class = "btn-sm")
      ),
      shiny::div(
        id = ns("content"),
        shiny::uiOutput(ns("sidebar")),
        shiny::div(
          id = ns("divider"),
          shiny::actionButton(
            ns("toggle_sidebar"),
            "",
            icon = shiny::icon("angle-double-left"),
            class = "toggle-sidebar-btn"
          )
        ),
        miniUI::miniContentPanel(shiny::tagList(
          shiny::uiOutput(ns("main_panel_static")), # Static button
          shiny::uiOutput(ns("main_panel_dynamic")) # Reactive content
        ))
      ),
      shiny::div(
        class = "button_block",
        miniUI::miniButtonBlock(
          shiny::actionButton(ns("create_archive"), "Archive file(s)")
        )
      )
    )
  )
}

ghqc_archive_server <- function(
  id,
  working_dir
) {
  .le$info("Fetching Milestones...")
  milestones <- get_milestones(working_dir)
  milestone_df <- create_safe_milestone_df(milestones)
  .le$debug(
    glue::glue(
      "Found {nrow(milestone_df)} milestones ({milestone_df |> dplyr::filter(!open) |> nrow()} closed)"
    )
  )

  validator <- shinyvalidate::InputValidator$new()

  working_dir_rv <- shiny::reactive(working_dir)
  shiny::observe({
    shiny::req(working_dir)
    # Ensure waiter is hidden - try multiple approaches for reliability
    try(waiter::waiter_hide(), silent = TRUE)
    # Also try hiding any specific waiter on main_container
    try(waiter::waiter_hide(id = "main_container"), silent = TRUE)
  })
  selected_files <- treeNavigatorServer(
    id,
    rootFolder = working_dir_rv,
    search = FALSE,
    all.files = FALSE
  )

  repo_name <- normalizePath(working_dir, mustWork = FALSE) |>
    basename() |>
    as.character()
  if (is.na(repo_name) || !nzchar(repo_name)) {
    repo_name <- "repo"
  }

  branch <- .catch(get_branch_impl(working_dir))

  local_commits <- .catch(get_branch_commits(working_dir))
  .le$debug(
    "Found {local_commits$commit |> unique() |> length()} commits on branch '{branch}'"
  )

  # In-memory cache for issue data to avoid re-fetching
  cached_issues <- shiny::reactiveValues()

  shiny::moduleServer(id, function(input, output, session) {
    # Additional waiter hide attempt at module initialization
    shiny::onFlush(
      function() {
        try(waiter::waiter_hide(), silent = TRUE)
      },
      once = TRUE
    )

    reset_triggered <- shiny::reactiveVal(FALSE)
    session$onSessionEnded(function() {
      if (!isTRUE(shiny::isolate(reset_triggered))) {
        shiny::stopApp()
      }
    })

    output$sidebar <- shiny::renderUI({
      shiny::tagList(
        shiny::selectizeInput(
          session$ns("selected_milestones"),
          label = "Select Milestone(s)",
          choices = "",
          multiple = TRUE,
          width = "100%"
        ),
        shiny::checkboxInput(
          session$ns("include_open_milestones"),
          label = "Include Open Milestones",
          value = FALSE
        ),
        shiny::checkboxInput(
          session$ns("include_open_issues"),
          label = "Include Open Issues",
          value = FALSE
        ),
        shiny::textInput(
          session$ns("archive_name"),
          "Archive Name",
          value = get_archive_name(repo_name, character(0))
        ),
        shiny::checkboxInput(
          session$ns("flatten"),
          "Flatten Directory Structure",
          value = FALSE
        ),
        shiny::div(
          style = "font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif !important; font-weight: bold;",
          "Select Additional File(s) for Archive"
        ),
        treeNavigatorUI(session$ns("treeNavigator"))
      )
    })

    # Pre-load issues for closed milestones first
    .le$info(
      "Fetching issues for {milestone_df |> dplyr::filter(!open) |> nrow()} closed milestones..."
    )
    closed_milestone_issues <- get_multiple_milestone_issues(
      milestones[!milestone_df$open],
      working_dir
    )
    closed_milestone_issue_count <- closed_milestone_issues |>
      purrr::map(length) |>
      unlist() |>
      sum()
    .le$debug(
      "Found {closed_milestone_issue_count} issues in the closed milestones"
    )
    loaded_issues <- shiny::reactiveVal(closed_milestone_issues)

    # Load open milestones once only once include_open_milestones is selected
    open_milestone_issues_loaded <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$include_open_milestones, {
      shiny::req(input$include_open_milestones)
      shiny::req(!open_milestone_issues_loaded())
      waiter <- waiter::Waiter$new(
        id = session$ns("main_container"),
        html = shiny::tagList(
          waiter::spin_2(),
          shiny::h4(glue::glue(
            "Loading issues for {milestone_df |> dplyr::filter(open) |> nrow()} open milestones..."
          ))
        ),
        color = "darkgrey"
      )

      waiter$show()

      open_milestones <- milestones[milestone_df$open]
      .le$info(
        "Loading issues for {milestone_df |> dplyr::filter(open) |> nrow()} open milestones..."
      )
      previously_loaded <- loaded_issues()
      newly_loaded <- get_multiple_milestone_issues(
        open_milestones,
        working_dir
      )
      loaded_issues(c(previously_loaded, newly_loaded))
      open_milestone_issues_loaded(TRUE)
      waiter$hide()
    })

    shiny::observeEvent(input$include_open_milestones, {
      milestone_choices <- if (input$include_open_milestones) {
        milestone_df
      } else {
        milestone_df |> dplyr::filter(!open)
      }

      # Track selected milestones to persist after options update
      previously_selected <- shiny::isolate(input$selected_milestones)
      if (is.null(previously_selected)) {
        previously_selected <- ""
      }

      shiny::updateSelectizeInput(
        session,
        "selected_milestones",
        choices = milestone_choices |> dplyr::pull(name),
        selected = previously_selected
      )
    })

    validator$add_rule("archive_name", shinyvalidate::sv_required())
    validator$enable()
    custom_archive_name <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(
      input$archive_name,
      {
        # Safely extract selected milestones, handling case where input might be an environment
        safe_milestones <- if (is_empty(input$selected_milestones)) {
          character(0)
        } else {
          as.character(input$selected_milestones)
        }

        expected_name <- get_archive_name(
          repo_name,
          safe_milestones
        )

        if (!is.null(input$archive_name)) {
          if (
            nzchar(input$archive_name) && input$archive_name != expected_name
          ) {
            .le$debug("Determined archive path to be edited by user")
            custom_archive_name(TRUE)
          } else if (!nzchar(input$archive_name)) {
            # If user clears the field, reset to automatic naming
            .le$debug("Archive path is empty. Will reset to default...")
            custom_archive_name(FALSE)
          }
        }
      }
    )

    shiny::observeEvent(
      input$selected_milestones,
      {
        .le$trace(
          "User changed the selected milestones: {paste0(input$selected_milestones, collapse = ', ')}"
        )
        shiny::req(!custom_archive_name())

        # Safely extract selected milestones, handling case where input might be an environment
        safe_milestones <- if (is_empty(input$selected_milestones)) {
          character(0)
        } else {
          as.character(input$selected_milestones)
        }

        archive_name <- get_archive_name(
          repo_name,
          selected_milestones = safe_milestones
        )

        .le$debug("Updating archive path to {archive_name}")
        shiny::updateTextInput(
          session,
          "archive_name",
          value = archive_name
        )
      },
      ignoreNULL = FALSE
    )

    flattened_loaded_issues <- shiny::reactive(flatten_multiple_milestone_issues(loaded_issues()))

    selected_milestone_files <- shiny::reactive({
      if (is_empty(input$selected_milestones)) {
        return(tibble::tibble(
          name = character(0),
          milestone = character(0),
          branch = character(0)
        ))
      }
      selected_issues <- flattened_loaded_issues() |>
        dplyr::filter(milestone %in% input$selected_milestones) |>
        dplyr::arrange(factor(milestone, levels = input$selected_milestones))
      if (!input$include_open_issues) {
        selected_issues <- selected_issues |> dplyr::filter(!open)
      }
      selected_issues |>
        dplyr::select(name, milestone, branch) |>
        dplyr::mutate(from_milestone = TRUE)
    })

    # Check if current branch is missing from globally selected milestones
    branch_mismatch_warning_needed <- shiny::reactive({
      # Only show warning if there are globally selected milestones
      if (is_empty(input$selected_milestones)) {
        return(FALSE)
      }

      milestone_files <- selected_milestone_files()
      if (nrow(milestone_files) == 0) {
        return(FALSE)
      }

      # Get unique branches from selected milestones
      milestone_branches <- milestone_files$branch |> unique()

      # Check if current branch is NOT in the milestone branches
      !branch %in% milestone_branches
    })

    # Trigger validation when branch mismatch status changes
    shiny::observeEvent(branch_mismatch_warning_needed(), {
      # Re-validate all commit fields when branch mismatch status changes
      current_files <- rendered_files()
      if (!is_empty(current_files)) {
        for (file in current_files) {
          commit_id <- generate_input_id("commits", file)
          # Trigger validation by getting current value and re-setting it
          current_value <- input[[commit_id]]
          if (!is.null(current_value)) {
            # This will trigger the validation rules
            shiny::updateSelectizeInput(
              session,
              commit_id,
              selected = current_value
            )
          }
        }
      }
    })

    shiny::observeEvent(
      input$selected_milestones,
      {
        shiny::req(length(input$selected_milestones) > 1)
        .le$debug(
          "Checking for duplicate files in selected milestones: {paste0(input$selected_milestones, collapse = ', ')}"
        )
        file_names <- selected_milestone_files() |>
          dplyr::pull(name)
        duplicates <- unique(file_names[duplicated(file_names)])
        if (is_empty(duplicates)) {
          .le$trace("Found no duplicates between selected milestones")
          return()
        }

        milestones_to_deselect <- selected_milestone_files() |>
          dplyr::select(-branch) |>
          dplyr::filter(name %in% duplicates) |>
          dplyr::group_by(name) |>
          dplyr::mutate(row_id = dplyr::row_number()) |>
          dplyr::filter(row_id > 1) |>
          dplyr::pull(milestone) |>
          unique()

        shiny::showModal(
          shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::modalButton("Return"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "Duplicate Files Found",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;")
            ),
            shiny::HTML(
              glue::glue(
                "#### New milestones add files which exist in selected milestones
Deselecting the new milestones from the **Select Milestone(s)** selector
##### Added Milestones
- {paste0(milestones_to_deselect, collapse = '\n- ')}

##### Duplicate Files
- {paste0(duplicates, collapse = '\n- ')}"
              ) |>
                markdown_to_html_impl()
            ),
            easyClose = TRUE,
            footer = NULL
          )
        )

        shiny::updateSelectizeInput(
          session,
          "selected_milestones",
          selected = input$selected_milestones[
            !input$selected_milestones %in% milestones_to_deselect
          ]
        )
      },
      ignoreNULL = TRUE
    )
    shiny::observeEvent(input$include_open_issues, {
      shiny::req(input$include_open_issues)
      shiny::req(length(input$selected_milestones) > 1)

      .le$debug(
        "Checking for duplicate files caused by including open issues..."
      )
      file_names <- selected_milestone_files() |>
        dplyr::pull(name)
      duplicates <- unique(file_names[duplicated(file_names)])
      if (is_empty(duplicates)) {
        .le$trace("Found no duplicates caused by including open issues")
        return()
      }

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "Duplicate Files Found",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;")
          ),
          shiny::HTML(
            glue::glue(
              "#### Including open issues would introduce duplicate files.
Deselecting the **Include Open Issues** checkbox
##### Duplicate Files 
- {paste0(duplicates, collapse = '\n- ')}"
            ) |>
              markdown_to_html_impl()
          ),
          easyClose = TRUE,
          footer = NULL
        )
      )

      shiny::updateCheckboxInput(session, "include_open_issues", value = FALSE)
    })

    previously_warned_duplicates <- shiny::reactiveVal(c())
    shiny::observeEvent(
      c(selected_files(), selected_milestone_files()),
      {
        shiny::req(selected_milestone_files())
        milestone_file_names <- selected_milestone_files() |>
          dplyr::pull(name) |>
          unique()
        duplicates <- selected_files()[
          selected_files() %in% milestone_file_names
        ] |>
          unique()
        new_to_warn_duplicates <- duplicates[
          !duplicates %in% previously_warned_duplicates()
        ]

        prev_warned_and_selected <- previously_warned_duplicates()[
          previously_warned_duplicates() %in% selected_files()
        ]
        previously_warned_duplicates(c(
          prev_warned_and_selected,
          new_to_warn_duplicates
        ))

        if (is_empty(new_to_warn_duplicates)) {
          .le$trace(
            "Found no duplicates between selected milestones and additionally selected files"
          )
          return()
        }

        # Create bullet points for duplicate files with their milestones
        duplicate_details <- selected_milestone_files() |>
          dplyr::filter(name %in% duplicates) |>
          dplyr::group_by(name) |>
          dplyr::summarise(
            milestones = paste(unique(milestone), collapse = ", "),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            bullet_point = glue::glue("- **{name}**: {milestones}")
          ) |>
          dplyr::pull(bullet_point)

        duplicate_bullets <- paste(duplicate_details, collapse = "\n")

        shiny::showModal(
          shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::modalButton("Return"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "Duplicate Files Found",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;")
            ),
            shiny::HTML(
              glue::glue(
                "#### Selected additional files are apart of selected milestone(s)\n\n{duplicate_bullets}"
              ) |>
                markdown_to_html_impl()
            ),
            easyClose = TRUE,
            footer = NULL
          )
        )
      },
      ignoreNULL = TRUE
    )

    file_tree_selected_files <- shiny::eventReactive(
      c(selected_files(), input$include_open_milestones),
      {
        if (is_empty(selected_files())) {
          return(tibble::tibble(
            name = character(0),
            milestone = character(0),
            from_milestone = logical(0)
          ))
        }
        selected_files <- flattened_loaded_issues() |>
          dplyr::filter(
            name %in% selected_files(),
            !name %in% (selected_milestone_files() |> dplyr::pull(name))
          ) |>
          dplyr::select(name, milestone) |>
          dplyr::bind_rows(tibble::tibble(
            name = selected_files() |> unlist(),
            milestone = ""
          )) |>
          dplyr::mutate(from_milestone = FALSE)
      }
    )

    archive_files <- shiny::reactive(
      {
        avail_milestones <- (if (isTRUE(input$include_open_milestones)) {
          milestone_df
        } else {
          milestone_df |> dplyr::filter(!open)
        }) |>
          dplyr::pull(name) |>
          c("")

        dplyr::bind_rows(
          selected_milestone_files() |> dplyr::select(-branch),
          file_tree_selected_files()
        ) |>
          dplyr::filter(milestone %in% avail_milestones)
      }
    )

    # Create static container for dynamic file content (like assign_app)
    output$main_panel_dynamic <- renderUI({
      session$sendCustomMessage("adjust_grid", id)

      shiny::tagList(
        shiny::div(
          id = session$ns("grid_container"),
          class = "grid-container-depth-0",
          style = "display: grid; grid-template-columns: 1fr minmax(150px, 1fr) 1.5fr; gap: 10px 12px; align-items: start;",
          shiny::div(shiny::tags$strong("Files")),
          shiny::div(shiny::tags$strong("Milestones")),
          shiny::div(shiny::tags$strong("Commits"))
        )
      )
    })

    commits_to_update <- shiny::reactiveVal(tibble::tibble(
      file_name = character(0),
      milestone = character(0)
    ))
    rendered_files <- shiny::reactiveVal(c())

    # Track when to show missing commits modal
    show_missing_commits_modal <- shiny::reactiveVal(FALSE)

    # Track previously warned commit issues to avoid duplicate warnings
    previously_warned_issues <- shiny::reactiveVal(character(0))

    #Track issues which do not have commits selected
    unselected_commit_issues <- shiny::reactiveVal(c())

    # Keep track of milestone observers to avoid duplicates
    milestone_observers <- shiny::reactiveValues()

    # Leep track of commit observers to avoid duplicates
    commit_observers <- shiny::reactiveValues()

    # Keep track of file preview observers to avoid duplicates
    file_preview_observers <- shiny::reactiveValues()

    # Reactive value for file preview requests
    file_preview_request <- shiny::reactiveVal(NULL)

    # Handle file preview requests (separate from button observers to avoid modal issues)
    shiny::observeEvent(file_preview_request(), {
      shiny::req(file_preview_request())

      request <- file_preview_request()
      file_name <- request$file
      selected_commit <- request$commit

      .le$debug("Processing file preview request for: {file_name}")

      # Check if commit is selected
      if (is.null(selected_commit) || selected_commit == "") {
        .le$warn("No commit selected for file preview: {file_name}")
        shiny::showModal(
          shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::modalButton("Return"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "No commit selected",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;")
            ),
            shiny::h6("Select a commit to preview file content"),
            footer = NULL,
            easyClose = TRUE
          )
        )
      } else {
        .le$debug(
          "Showing file preview for {file_name} at commit {selected_commit}"
        )

        # Attempt to get file content using Rust function
        content_result <- tryCatch(
          {
            # Create the archive file data structure for the Rust function
            archive_file_data <- list(
              file = file_name,
              commit = selected_commit
            )

            file_content <- .catch(get_file_content_impl(
              archive_file_data,
              working_dir
            ))
            list(success = TRUE, content = file_content)
          },
          error = function(e) {
            .le$error("Failed to get file content for {file_name}: {e$message}")
            list(success = FALSE, error = e$message)
          }
        )

        if (content_result$success) {
          # Success - show file content modal
          shiny::showModal(
            shiny::modalDialog(
              title = shiny::tags$div(
                style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                shiny::tags$div(
                  shiny::modalButton("Close"),
                  style = "flex: 0 0 auto;"
                ),
                shiny::tags$div(
                  glue::glue("File Preview: {basename(file_name)}"),
                  style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 18px;"
                ),
                shiny::tags$div(style = "flex: 0 0 auto;")
              ),
              shiny::div(
                shiny::p(
                  style = "margin: 10px 0; color: #666;",
                  glue::glue("Commit: {substr(selected_commit, 1, 7)}")
                ),
                shiny::tags$pre(
                  style = "border: 1px solid #ccc; padding: 10px; max-height: 500px; overflow-y: auto; background-color: white; margin: 0; font-family: monospace; white-space: pre-wrap; word-wrap: break-word;",
                  content_result$content
                )
              ),
              size = "l",
              easyClose = TRUE,
              footer = NULL
            )
          )
        } else {
          # Error - show error modal
          shiny::showModal(
            shiny::modalDialog(
              title = shiny::tags$div(
                style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                shiny::tags$div(
                  shiny::modalButton("Close"),
                  style = "flex: 0 0 auto;"
                ),
                shiny::tags$div(
                  "Cannot Read File Content",
                  style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 18px; color: #dc3545;"
                ),
                shiny::tags$div(style = "flex: 0 0 auto;")
              ),
              shiny::div(
                style = "text-align: center; padding: 20px;",
                shiny::div(
                  shiny::icon(
                    "exclamation-triangle",
                    style = "font-size: 48px; color: #dc3545; margin-bottom: 15px;"
                  )
                ),
                shiny::h5(glue::glue(
                  "Unable to read content for: {file_name}"
                )),
                shiny::p(
                  style = "margin: 10px 0; color: #666;",
                  glue::glue("Commit: {substr(selected_commit, 1, 7)}")
                ),
                shiny::div(
                  style = "background-color: #f8d7da; padding: 15px; border-radius: 5px; margin: 15px 0; text-align: left; border-left: 4px solid #dc3545;",
                  shiny::strong("Error details:"),
                  shiny::br(),
                  shiny::code(
                    content_result$error,
                    style = "word-break: break-all; color: #721c24;"
                  )
                )
              ),
              size = "m",
              easyClose = TRUE,
              footer = NULL
            )
          )
        }
      }

      # Reset the reactive value
      file_preview_request(NULL)
    })

    shiny::observeEvent(
      archive_files(),
      {
        files_to_add <- setdiff(archive_files()$name, rendered_files()) |>
          unique()
        files_to_remove <- setdiff(rendered_files(), archive_files()$name)

        if (!is_empty(files_to_add)) {
          .le$info(
            "Rendering {length(files_to_add)} ui rows for: {paste(files_to_add, collapse = ', ')}"
          )
        }

        if (!is_empty(files_to_remove)) {
          .le$info(
            "Removing {length(files_to_remove)} ui rows for: {paste(files_to_remove, collapse = ', ')}"
          )

          # Clear previously warned issues for files that are being removed
          if (length(previously_warned_issues()) > 0) {
            # Get issue numbers for files being removed
            removed_file_issues <- character(0)
            for (file in files_to_remove) {
              # Check all milestones to find issues associated with this file
              file_issues <- flattened_loaded_issues() |>
                dplyr::filter(name == file) |>
                dplyr::pull(number) |>
                as.character()
              removed_file_issues <- c(removed_file_issues, file_issues)
            }

            # Remove these issues from previously warned list
            if (length(removed_file_issues) > 0) {
              current_warned <- previously_warned_issues()
              updated_warned <- setdiff(current_warned, removed_file_issues)
              previously_warned_issues(updated_warned)
              .le$debug(
                "Cleared previously warned issues for removed files: {paste(removed_file_issues, collapse = ', ')}"
              )
            }
          }
        }

        if (is_empty(files_to_add) && is_empty(files_to_remove)) {
          return()
        }

        update_commits_waiter$show()

        for (file in files_to_remove) {
          milestone_id <- generate_input_id("milestone", file)
          commit_id <- generate_input_id("commits", file)
          row_id <- generate_input_id("file_row", file) |> session$ns()
          attr_selector <- glue::glue("[id='{row_id}']")

          # Remove milestone observer for this file
          if (!is.null(milestone_observers[[file]])) {
            milestone_observers[[file]]$destroy()
            milestone_observers[[file]] <- NULL
            .le$debug("Removed milestone observer for file: {file}")
          }

          if (!is.null(commit_observers[[file]])) {
            unselected <- unselected_commit_issues()
            unselected_commit_issues(setdiff(unselected, file))

            commit_observers[[file]]$destroy()
            commit_observers[[file]] <- NULL
            .le$debug("Removed commit observer for file: {file}")
          }

          # Remove file preview observer for this file
          if (!is.null(file_preview_observers[[file]])) {
            file_preview_observers[[file]]$destroy()
            file_preview_observers[[file]] <- NULL
            .le$debug("Removed file preview observer for file: {file}")
          }

          # Note: shinyvalidate doesn't support removing individual rules
          # Rules will be cleaned up when the input elements are removed
          .le$debug(
            "Validation rules will be cleaned up with input removal for file: {file}"
          )

          shiny::updateSelectizeInput(
            session,
            inputId = milestone_id,
            choices = character(0),
            selected = NULL,
            server = TRUE
          )
          shiny::removeUI(selector = attr_selector)
        }

        for (file in files_to_add) {
          milestone_options <- archive_files() |>
            dplyr::filter(name == file)
          file_ui <- create_single_archive_file_ui(
            file,
            milestone_options,
            milestone_df,
            session$ns
          )
          shiny::insertUI(
            selector = paste0("#", session$ns("grid_container")),
            where = "beforeEnd",
            ui = file_ui
          )

          # Add comprehensive commit validation rule (branch mismatch + required)
          commit_id <- generate_input_id("commits", file)
          local({
            current_file <- file
            validator$add_rule(
              commit_id,
              function(value) {
                tryCatch(
                  {
                    # First check for branch mismatch - this overrides required validation
                    branch_warning <- check_branch_mismatch_warning(
                      current_file,
                      input,
                      branch_mismatch_warning_needed()
                    )

                    if (!is.null(branch_warning)) {
                      return(branch_warning)
                    }

                    # If no branch mismatch, check required validation
                    if (is.null(value) || value == "") {
                      return("Select a commit")
                    }

                    # All validations pass
                    return(NULL)
                  },
                  error = function(e) {
                    .le$warn(
                      "Error in commit validation for {current_file}: {e$message}"
                    )
                    return(NULL)
                  }
                )
              }
            )
          })
          .le$debug(
            "Added comprehensive commit validation rule for file: {file}"
          )

          # Add milestone validation rule for this file to show open issue warnings
          milestone_id <- generate_input_id("milestones", file)
          # Capture file name in local scope to avoid closure issues
          local({
            current_file <- file
            validator$add_rule(
              milestone_id,
              function(value) {
                tryCatch(
                  {
                    check_milestone_issue_status(
                      current_file,
                      value,
                      flattened_loaded_issues(),
                      cached_issues,
                      loaded_issues(),
                      working_dir
                    )
                  },
                  error = function(e) {
                    .le$warn(
                      "Error in milestone validation for {current_file}: {e$message}"
                    )
                    return(NULL)
                  }
                )
              }
            )
          })
          .le$debug("Added milestone validation rule for file: {file}")

          # Create milestone observer for this file after UI is inserted
          # Use local() to properly capture the file variable
          local({
            current_file_name <- file
            later::later(
              function() {
                milestone_input_id <- generate_input_id(
                  "milestones",
                  current_file_name
                )
                commit_input_id <- generate_input_id(
                  "commits",
                  current_file_name
                )

                # Create observer for this file's milestone selection
                observer <- shiny::observeEvent(
                  input[[milestone_input_id]],
                  {
                    milestone_value <- input[[milestone_input_id]]
                    .le$debug(
                      "Milestone for '{current_file_name}' changed: {milestone_value}"
                    )
                    backlog <- commits_to_update()
                    # Remove any existing entry for this file to avoid duplicates
                    backlog <- backlog[backlog$file_name != current_file_name, ]
                    commits_to_update(dplyr::bind_rows(
                      backlog,
                      tibble::tibble(
                        file_name = current_file_name,
                        milestone = milestone_value
                      )
                    ))
                  },
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE
                )

                milestone_observers[[current_file_name]] <- observer

                observer <- shiny::observeEvent(
                  input[[commit_input_id]],
                  {
                    unselected_sans_current <- setdiff(
                      unselected_commit_issues(),
                      current_file_name
                    )
                    if (
                      is_empty(input[[commit_input_id]]) ||
                        input[[commit_input_id]] == ""
                    ) {
                      unselected_commit_issues(c(
                        unselected_sans_current,
                        current_file_name
                      ))
                    } else {
                      unselected_commit_issues(unselected_sans_current)
                    }
                  },
                  ignoreNULL = FALSE,
                  ignoreInit = FALSE
                )

                commit_observers[[current_file_name]] <- observer

                # Create file preview observer for this file
                file_preview_input_id <- generate_input_id(
                  "file_preview",
                  current_file_name
                )

                preview_observer <- shiny::observeEvent(
                  input[[file_preview_input_id]],
                  {
                    .le$debug("File preview clicked for: {current_file_name}")

                    # Get the current commit for this file
                    commit_input_id <- generate_input_id(
                      "commits",
                      current_file_name
                    )
                    selected_commit <- input[[commit_input_id]]

                    # Set the reactive value with file preview request
                    file_preview_request(list(
                      file = current_file_name,
                      commit = selected_commit # Can be NULL, will be handled by modal observer
                    ))

                    commit_display <- if (
                      is.null(selected_commit) || selected_commit == ""
                    ) {
                      "NULL"
                    } else {
                      selected_commit
                    }
                    .le$debug(
                      "Set file preview request for: {current_file_name}, commit: {commit_display}"
                    )
                  },
                  ignoreNULL = TRUE,
                  ignoreInit = TRUE
                )

                file_preview_observers[[current_file_name]] <- preview_observer
                .le$debug(
                  "Created commit, milestones, and preview observers for file: {current_file_name}"
                )
              },
              delay = 0.1
            )
          })
        }
        rendered_files(archive_files()$name)
        update_commits_waiter$hide()
      },
      ignoreInit = TRUE
    )

    update_commits_waiter <- waiter::Waiter$new(
      id = session$ns("main_container"),
      html = shiny::tagList(
        waiter::spin_2(),
        shiny::h4(glue::glue(
          "Rendering file row(s)..."
        ))
      ),
      color = "darkgrey"
    )

    shiny::observeEvent(
      shiny::debounce(commits_to_update(), 50),
      {
        shiny::req(!is_empty(commits_to_update()))
        .le$trace(
          "Looking up commit for selected milestone files: {paste0(commits_to_update()$file_name, collapse = ', ')}"
        )
        update_commits_waiter$show()

        # Process all accumulated items (without failed_commits parameter)
        purrr::map2(
          commits_to_update()$file_name,
          commits_to_update()$milestone,
          function(file_name, milestone) {
            update_commit_options(
              session,
              file_name,
              milestone,
              local_commits,
              cached_issues,
              loaded_issues(),
              flattened_loaded_issues(),
              working_dir
            )
          }
        )

        # Clear everything and reset flag
        commits_to_update(tibble::tibble(
          file_name = character(0),
          milestone = character(0)
        ))
        update_commits_waiter$hide()

        # Trigger modal check after processing is complete
        show_missing_commits_modal(TRUE)
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Check all rendered files for missing commits modal system
    shiny::observeEvent(
      shiny::debounce(show_missing_commits_modal(), 500),
      {
        shiny::req(show_missing_commits_modal())

        # Reset the trigger
        show_missing_commits_modal(FALSE)

        # Check all currently rendered files for commit issues
        missing_commits_info <- tibble::tibble()

        for (file_name in rendered_files()) {
          # Get the milestone input for this file
          milestone_input_id <- generate_input_id("milestones", file_name)
          milestone_value <- input[[milestone_input_id]]

          # Skip if no milestone selected
          if (is.null(milestone_value) || milestone_value == "") {
            next
          }

          # Find the issue info for this file and milestone
          issue_info <- flattened_loaded_issues() |>
            dplyr::filter(name == file_name, milestone == milestone_value)

          if (nrow(issue_info) == 0) {
            next
          }

          issue_number <- issue_info$number[1]

          # Replicate the same logic as update_commit_options to check if commit is missing
          latest_commit_info <- tryCatch(
            {
              get_latest_commit_from_issue(
                issue_number,
                cached_issues,
                loaded_issues(),
                working_dir
              )
            },
            error = function(e) NULL
          )

          # Check if commit info is missing (same condition as update_commit_options)
          if (
            is.null(latest_commit_info) ||
              is.null(latest_commit_info$commit) ||
              is.null(latest_commit_info$message)
          ) {
            missing_commits_info <- dplyr::bind_rows(
              missing_commits_info,
              tibble::tibble(
                file_name = file_name,
                issue_number = as.character(issue_number),
                expected_branch = issue_info$branch[1],
                milestone = milestone_value
              )
            )
          }
        }

        # Show modal only if there are missing commits AND they're different from previously warned
        if (nrow(missing_commits_info) > 0) {
          current_issues <- sort(missing_commits_info$issue_number)
          previous_issues <- sort(previously_warned_issues())

          # Only show modal if the issues are different from what we've already warned about
          if (!identical(current_issues, previous_issues)) {
            .le$debug(
              "Showing missing commits modal for issues: {paste0(current_issues, collapse = ', ')}"
            )

            # Create modal content with recommendations at top
            modal_content <- shiny::div(
              shiny::h4(
                glue::glue(
                  "Could not find commits for {length(current_issues)} issues"
                )
              ),
              shiny::div(
                style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px; border-left: 4px solid #bee5eb; margin-bottom: 15px;",
                shiny::strong("To resolve:"),
                shiny::br(),
                "1. Run: ",
                shiny::code("git fetch origin"),
                shiny::br(),
                "2. For each expected branch below, checkout the branch:",
                shiny::br(),
                # List each unique branch that needs to be checked out
                purrr::map(
                  unique(missing_commits_info$expected_branch),
                  function(branch) {
                    shiny::div(
                      style = "margin-left: 20px;",
                      shiny::code(glue::glue("git checkout {branch}")),
                      shiny::br()
                    )
                  }
                ),
                "3. Return to your working branch: ",
                shiny::code(glue::glue("git checkout {branch}")),
                shiny::br(),
                "4. Refresh the milestone selection"
              ),
              shiny::div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;",
                shiny::strong("Current branch: "),
                shiny::code(branch)
              ),
              shiny::br(),
              # Issues list after recommendations
              purrr::map(1:nrow(missing_commits_info), function(i) {
                row <- missing_commits_info[i, ]
                shiny::div(
                  style = "border: 1px solid #dee2e6; padding: 10px; margin: 5px 0; border-radius: 3px;",
                  shiny::strong(glue::glue("Issue #{row$issue_number}")),
                  shiny::br(),
                  shiny::span("File: ", shiny::code(row$file_name)),
                  shiny::br(),
                  shiny::span("Milestone: ", shiny::code(row$milestone)),
                  shiny::br(),
                  shiny::span(
                    "Expected branch: ",
                    shiny::code(row$expected_branch)
                  )
                )
              })
            )

            # Show modal with Return button in title bar
            shiny::showModal(
              shiny::modalDialog(
                modal_content,
                title = shiny::tags$div(
                  style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                  shiny::tags$div(
                    shiny::modalButton("Return"),
                    style = "flex: 0 0 auto;"
                  ),
                  shiny::tags$div(
                    "Missing Commits",
                    style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
                  ),
                  shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
                ),
                size = "l",
                footer = NULL,
                easyClose = TRUE
              )
            )

            # Record the current issues as warned immediately when modal is shown
            previously_warned_issues(current_issues)
          } else {
            .le$debug(
              "Skipping modal - same issues already warned: {paste0(current_issues, collapse = ', ')}"
            )
          }
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Update milestone options for all rendered files when include_open_milestones changes
    shiny::observeEvent(
      c(input$include_open_milestones, input$include_open_issues),
      {
        # Get currently rendered files and update their milestone options
        current_files <- rendered_files()
        if (!is_empty(current_files)) {
          .le$debug(
            "Updating milestone options for {length(current_files)} files after include_open_milestones change"
          )
          for (file in current_files) {
            # Get the current milestone selection for this file
            milestone_input_id <- generate_input_id("milestones", file)
            current_milestone <- input[[milestone_input_id]]

            update_milestone_options(
              session,
              current_milestone,
              file,
              archive_files(),
              milestone_df
            )
          }
        }
      },
      ignoreInit = TRUE
    )

    # Function to check all archive button requirements
    check_archive_button_state <- function() {
      # Check 1: Must have files in archive
      if (length(rendered_files()) == 0) {
        .le$debug("Archive button disabled - no files in archive")
        shinyjs::removeClass("create_archive", "enable-btn")
        shinyjs::addClass("create_archive", "disable-btn")
        shinyjs::disable("create_archive")
        return()
      }

      # Check 2: Must have archive name
      archive_name <- input$archive_name
      if (is.null(archive_name) || trimws(archive_name) == "") {
        .le$debug("Archive button disabled - no archive name provided")
        shinyjs::removeClass("create_archive", "enable-btn")
        shinyjs::addClass("create_archive", "disable-btn")
        shinyjs::disable("create_archive")
        return()
      }

      # Check 3: All files must have commits selected
      if (length(unselected_commit_issues()) > 0) {
        .le$debug(
          "Archive button disabled - some files missing commit selection"
        )
        shinyjs::removeClass("create_archive", "enable-btn")
        shinyjs::addClass("create_archive", "disable-btn")
        shinyjs::disable("create_archive")
        return()
      }

      # All requirements met - enable button
      .le$debug("Archive button enabled - all requirements met")
      shinyjs::removeClass("create_archive", "disable-btn")
      shinyjs::addClass("create_archive", "enable-btn")
      shinyjs::enable("create_archive")
    }

    # Monitor all requirements for Archive button
    shiny::observeEvent(
      c(unselected_commit_issues(), rendered_files(), input$archive_name),
      {
        check_archive_button_state()
      }
    )

    # Handle create archive button with validation
    shiny::observeEvent(input$create_archive, {
      .le$debug("Create archive button clicked")

      # Custom validation that only blocks on required fields (not milestone warnings)
      validation_failed <- FALSE

      # Check archive name is provided
      if (is.null(input$archive_name) || trimws(input$archive_name) == "") {
        .le$debug("Validation failed - archive name required")
        validation_failed <- TRUE
      }

      # Check all files have commits selected (but allow milestone warnings)
      if (length(unselected_commit_issues()) > 0) {
        .le$debug("Validation failed - some files missing commit selection")
        validation_failed <- TRUE
      }

      if (validation_failed) {
        return()
      }

      .le$info("Validation passed - proceeding with archive creation")

      # Create tibble of all rendered files with their data
      archive_tibble <- tibble::tibble(
        file = character(0),
        commit = character(0),
        milestone = character(0),
        approved = logical(0)
      )

      for (file_name in (rendered_files() |> unique())) {
        # Get milestone and commit for this file
        milestone_input_id <- generate_input_id("milestones", file_name)
        commit_input_id <- generate_input_id("commits", file_name)

        milestone_value <- input[[milestone_input_id]]
        commit_value <- input[[commit_input_id]]

        # Determine approved status for this file
        if (!is.null(milestone_value) && milestone_value != "") {
          # Milestone has a value - approved must be a boolean
          approved_value <- FALSE # Default to FALSE if we can't determine

          # File has a milestone - get its state from issue data
          issue_info <- flattened_loaded_issues() |>
            dplyr::filter(name == file_name, milestone == milestone_value)

          if (nrow(issue_info) > 0) {
            issue_number <- issue_info$number[1]

            # Get the commit info that contains state
            latest_commit_info <- tryCatch(
              {
                get_latest_commit_from_issue(
                  issue_number,
                  cached_issues,
                  loaded_issues(),
                  working_dir
                )
              },
              error = function(e) NULL
            )

            # Extract approved status if available
            if (
              !is.null(latest_commit_info) &&
                !is.null(latest_commit_info$approved)
            ) {
              approved_value <- latest_commit_info$approved
            }
          }
        } else {
          # File has no milestone - set milestone and approved to NA
          milestone_value <- NA_character_
          approved_value <- NA
        }

        # Add row to tibble
        archive_tibble <- dplyr::bind_rows(
          archive_tibble,
          tibble::tibble(
            file = file_name,
            commit = if (is.null(commit_value) || commit_value == "") {
              NA_character_
            } else {
              commit_value
            },
            milestone = milestone_value,
            approved = approved_value
          )
        )
      }

      .le$debug("Created archive tibble with {nrow(archive_tibble)} files")

      # Attempt to create the archive and show result modal
      archive_result <- tryCatch(
        {
          archive_path <- .catch(create_archive_impl(
            archive_tibble |> purrr::transpose(),
            input$flatten,
            input$archive_name,
            working_dir
          ))
          list(success = TRUE, path = archive_path)
        },
        error = function(e) {
          .le$error("Archive creation failed: {e$message}")
          list(success = FALSE, error = e$message)
        }
      )

      # Show result modal
      if (archive_result$success) {
        # Success modal
        shiny::showModal(
          shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::modalButton("Close"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "Archive Created Successfully",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px; color: #28a745;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;")
            ),
            shiny::div(
              style = "text-align: center; padding: 20px;",
              shiny::div(
                shiny::icon(
                  "check-circle",
                  style = "font-size: 48px; color: #28a745; margin-bottom: 15px;"
                )
              ),
              shiny::h4("Your archive has been created successfully!"),
              shiny::p(
                style = "margin: 15px 0;",
                "Archive contains ",
                shiny::strong(nrow(archive_tibble)),
                " files"
              ),
              shiny::div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 15px 0; text-align: left;",
                shiny::strong("Archive location:"),
                shiny::br(),
                shiny::code(
                  archive_result$path,
                  style = "word-break: break-all;"
                )
              )
            ),
            size = "m",
            easyClose = TRUE,
            footer = NULL
          )
        )
        .le$info("Archive created successfully at: {archive_result$path}")
      } else {
        # Error modal
        shiny::showModal(
          shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::modalButton("Close"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "Archive Creation Failed",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px; color: #dc3545;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;")
            ),
            shiny::div(
              style = "text-align: center; padding: 20px;",
              shiny::div(
                shiny::icon(
                  "exclamation-triangle",
                  style = "font-size: 48px; color: #dc3545; margin-bottom: 15px;"
                )
              ),
              shiny::h4("Archive creation failed"),
              shiny::p(
                style = "margin: 15px 0;",
                "An error occurred while creating the archive."
              ),
              shiny::div(
                style = "background-color: #f8d7da; padding: 15px; border-radius: 5px; margin: 15px 0; text-align: left; border-left: 4px solid #dc3545;",
                shiny::strong("Error details:"),
                shiny::br(),
                shiny::code(
                  archive_result$error,
                  style = "word-break: break-all; color: #721c24;"
                )
              )
            ),
            size = "m",
            easyClose = TRUE,
            footer = NULL
          )
        )
      }
    })

    # Handle close and reset buttons
    shiny::observeEvent(input$close, {
      reset_triggered(TRUE)
      shiny::stopApp()
    })

    shiny::observeEvent(input$reset, {
      reset_triggered(TRUE)
      session$reload()
    })
  })
}

update_commit_options <- function(
  session,
  file_name,
  milestone_value,
  local_commits,
  cached_issues,
  loaded_issues_data,
  flattened_issues,
  working_dir
) {
  commit_input_id <- generate_input_id("commits", file_name)

  if (is.null(milestone_value) || milestone_value == "") {
    # No milestone selected - show all commits for this file
    commit_data <- get_commits_for_file(local_commits, file_name)

    # Add empty choice at beginning to prevent auto-selection

    shiny::updateSelectizeInput(
      session,
      commit_input_id,
      choices = commit_data$choices,
      selected = "",
      options = list(
        placeholder = if (length(commit_data$choices) == 0) {
          "No commits found for file"
        } else {
          "Select commit (required)"
        }
      )
    )
  } else {
    # Milestone selected - find issue and get latest commit
    # Find the issue number for this file and milestone
    issue_info <- flattened_issues |>
      dplyr::filter(name == file_name, milestone == milestone_value)

    if (nrow(issue_info) == 0) {
      .le$warn(
        "Could not find issue for file '{file_name}' in milestone '{milestone_value}'"
      )
      shiny::updateSelectizeInput(
        session,
        commit_input_id,
        choices = character(0),
        selected = NULL,
        options = list(placeholder = "Issue not found")
      )
      return()
    }

    issue_number <- issue_info$number[1]
    latest_commit_info <- get_latest_commit_from_issue(
      issue_number,
      cached_issues,
      loaded_issues_data,
      working_dir
    )

    if (
      is.null(latest_commit_info) ||
        is.null(latest_commit_info$commit) ||
        is.null(latest_commit_info$message)
    ) {
      # Just update the UI to show commit not found
      shiny::updateSelectizeInput(
        session,
        commit_input_id,
        choices = character(0),
        selected = NULL,
        options = list(placeholder = "Commit not found")
      )
      return()
    }

    # Create a single choice with the latest commit using message from get_issue_latest_commit_impl
    commit_display <- format_commit_choice(
      latest_commit_info$commit,
      latest_commit_info$message
    )
    commit_choice <- latest_commit_info$commit
    names(commit_choice) <- commit_display

    shiny::updateSelectizeInput(
      session,
      commit_input_id,
      choices = commit_choice,
      selected = latest_commit_info$commit,
      options = list(
        placeholder = "Commit selected"
      )
    )
  }
}

update_milestone_options <- function(
  session,
  current_selected_milestone,
  file_name,
  archive_files,
  milestone_df
) {
  id <- generate_input_id("milestones", file_name)
  milestone_options <- archive_files |>
    dplyr::filter(name == file_name)

  if (milestone_options |> dplyr::filter(from_milestone) |> nrow() > 0) {
    # File comes from milestone selection - lock to that milestone
    milestone <- milestone_options |>
      dplyr::filter(from_milestone) |>
      dplyr::slice(1) |>
      dplyr::pull(milestone)

    shiny::updateSelectizeInput(
      session,
      id,
      choices = milestone,
      selected = milestone
    )
  } else {
    # File is manually selected - show all available milestone options
    raw_choices <- milestone_options |> dplyr::pull(milestone)

    # Sort the choices by milestone number (newest first, empty string last)
    sorted_choices <- sort_milestones_by_number(raw_choices, milestone_df)

    # Determine what should be selected
    if (
      is.null(current_selected_milestone) ||
        !current_selected_milestone %in% sorted_choices
    ) {
      # Reset to empty if current selection is not available (per user preference)
      selected_value <- ""
    } else {
      # Preserve current selection if it's still valid
      selected_value <- current_selected_milestone
    }

    shiny::updateSelectizeInput(
      session,
      id,
      choices = sorted_choices,
      selected = selected_value,
      options = list(
        placeholder = if (is_empty(sorted_choices)) {
          "No milestones available"
        } else {
          "Select Milestone (optional)"
        }
      )
    )
  }
}

get_archive_name <- function(repo_name, selected_milestones) {
  c(repo_name, selected_milestones) |>
    paste0(collapse = "-") |>
    gsub(pattern = " ", replacement = "-") |>
    (\(file_name) file.path("archive", glue::glue("{file_name}.zip")))()
}

sort_milestones_by_number <- function(milestone_names, milestone_df) {
  if (is_empty(milestone_names)) {
    return(milestone_names)
  }

  # Separate empty strings from actual milestone names
  empty_strings <- milestone_names[milestone_names == ""]
  actual_milestones <- milestone_names[milestone_names != ""]

  if (is_empty(actual_milestones)) {
    return(milestone_names)
  }

  # Use milestone_df to get proper sorting by number
  sorted_milestones <- milestone_df |>
    dplyr::filter(name %in% actual_milestones) |>
    dplyr::arrange(dplyr::desc(number)) |> # Newest first (highest number)
    dplyr::pull(name)

  # Add any milestones not found in milestone_df (shouldn't happen, but for safety)
  missing_milestones <- setdiff(actual_milestones, sorted_milestones)

  # Return sorted milestones, then missing ones, then empty strings
  c(sorted_milestones, missing_milestones, empty_strings)
}

# Helper functions for commit processing
format_commit_choice <- function(commit_hash, message) {
  # Take first 7 characters of hash and truncate message if needed
  short_hash <- substr(commit_hash, 1, 7)
  truncated_message <- if (nchar(message) > 60) {
    paste0(substr(message, 1, 57), "...")
  } else {
    message
  }
  paste0(short_hash, " - ", truncated_message)
}

get_commits_for_file <- function(local_commits, file_name) {
  # Filter commits that changed the specific file
  file_commits <- local_commits |>
    dplyr::filter(file == file_name) |>
    dplyr::arrange(dplyr::desc(dplyr::row_number())) # Most recent first

  if (nrow(file_commits) == 0) {
    return(list(choices = character(0), values = character(0)))
  }

  # Format choices and return both display and values
  display_names <- purrr::map2_chr(
    file_commits$commit,
    file_commits$message,
    format_commit_choice
  )
  choices <- file_commits$commit
  names(choices) <- display_names

  list(choices = choices, values = file_commits$commit)
}

find_issue_in_loaded_issues <- function(issue_number, loaded_issues_data) {
  # Search through all milestone issue lists to find the specific issue
  for (milestone_issues in loaded_issues_data) {
    for (issue in milestone_issues) {
      if (!is.null(issue$number) && issue$number == issue_number) {
        return(issue)
      }
    }
  }
  return(NULL)
}

get_latest_commit_from_issue <- function(
  issue_number,
  cached_issues,
  loaded_issues_data,
  working_dir
) {
  # Check cache first
  cache_key <- as.character(issue_number)
  if (!is.null(cached_issues[[cache_key]])) {
    .le$debug("Using cached issue data for issue #{issue_number}")
    issue_data <- cached_issues[[cache_key]]
  } else {
    .le$debug("Fetching issue data for issue #{issue_number}")

    # Find the full issue object in loaded_issues
    full_issue <- find_issue_in_loaded_issues(issue_number, loaded_issues_data)
    if (is.null(full_issue)) {
      .le$warn("Could not find issue #{issue_number} in loaded_issues")
      return(NULL)
    }

    # Call get_issue_latest_commit_impl with the full issue object
    issue_data <- get_issue_latest_commit_impl(full_issue, working_dir)
    # Cache the result
    cached_issues[[cache_key]] <- issue_data
  }

  # Extract the latest commit info (assuming it's the first/most recent in the data)
  if (is_empty(issue_data)) {
    return(NULL)
  }

  # Validate that we have commit and message data
  if (
    is.null(issue_data$commit) ||
      length(issue_data$commit) == 0 ||
      is.null(issue_data$message) ||
      length(issue_data$message) == 0
  ) {
    .le$warn(
      "Issue data for #{issue_number} is missing commit or message information"
    )
    return(NULL)
  }

  # Return commit hash, message, and approved status
  list(
    commit = issue_data$commit[1],
    message = issue_data$message[1],
    approved = issue_data$approved[1]
  )
}

create_single_archive_file_ui <- function(
  file_name,
  milestones,
  milestone_df,
  ns,
  input_selections = NULL
) {
  input_id <- list()
  for (prefix in c(
    "milestones",
    "commits",
    "file_row",
    "file_preview"
  )) {
    input_id[prefix] <- generate_input_id(prefix, file_name)
  }

  file_preview_btn <- shiny::actionButton(
    ns(input_id$file_preview),
    label = file_name,
    style = "padding: 4px 8px; margin: 2px 0; font-size: 12px; border-radius: 4px; border: 1px solid #ccc; background-color: #f8f9fa; color: #495057; white-space: normal; word-wrap: break-word; max-width: 200px; text-align: left; line-height: 1.2;",
    class = "btn-sm"
  )

  milestone_input <- if (
    is_empty(milestones) || all(milestones$milestone == "")
  ) {
    shiny::selectizeInput(
      ns(input_id$milestones),
      label = NULL,
      choices = NULL,
      selected = NULL,
      options = list(
        placeholder = "No milestones available"
      )
    )
  } else if (milestones |> dplyr::filter(from_milestone) |> nrow() > 0) {
    milestone <- milestones |>
      dplyr::filter(from_milestone) |>
      dplyr::slice(1) |>
      dplyr::pull(milestone)

    shiny::selectizeInput(
      ns(input_id$milestones),
      label = NULL,
      choices = milestone,
      selected = milestone
    )
  } else {
    sorted_choices <- milestones$milestone |>
      sort_milestones_by_number(milestone_df)
    shiny::selectizeInput(
      ns(input_id$milestones),
      label = NULL,
      choices = sorted_choices,
      selected = "",
      options = list(
        placeholder = "Select Milestone (optional)"
      )
    )
  }

  commit_input <- shiny::selectizeInput(
    ns(input_id$commits),
    label = NULL,
    choices = NULL,
    width = "100%",
    selected = NULL,
    options = list(
      placeholder = "Select commit (required)"
    )
  )

  # Create the file row container
  .le$trace("Creating file row with id: {input_id$file_row}")
  shiny::div(
    id = ns(input_id$file_row),
    style = "display: contents;", # children participate in parent grid
    shiny::actionButton(
      ns(input_id$file_preview),
      label = file_name,
      style = "padding: 4px 8px; margin: 2px 0; font-size: 12px; border-radius: 4px; border: 1px solid #ccc; background-color: #f8f9fa; color: #495057; white-space: normal; word-wrap: break-word; max-width: 200px; text-align: left; line-height: 1.2;",
      class = "btn-sm"
    ),
    milestone_input,
    commit_input
  )
}

# Helper function to check milestone issue approval status for validation
check_milestone_issue_status <- function(
  file_name,
  milestone_value,
  flattened_issues,
  cached_issues = NULL,
  loaded_issues_data = NULL,
  working_dir = NULL
) {
  # If no milestone selected, no validation message needed
  if (is.null(milestone_value) || milestone_value == "") {
    return(NULL)
  }

  # Find the issue for this file and milestone combination
  issue_info <- flattened_issues |>
    dplyr::filter(name == file_name, milestone == milestone_value)

  if (nrow(issue_info) == 0) {
    # No issue found - this might be a manually selected file
    return(NULL)
  }

  issue_number <- issue_info$number[1]

  # Try to get commit info to check approval status
  if (
    !is.null(cached_issues) &&
      !is.null(loaded_issues_data) &&
      !is.null(working_dir)
  ) {
    latest_commit_info <- tryCatch(
      {
        get_latest_commit_from_issue(
          issue_number,
          cached_issues,
          loaded_issues_data,
          working_dir
        )
      },
      error = function(e) NULL
    )

    # If we can get commit info, check approval status
    if (
      !is.null(latest_commit_info) &&
        !is.null(latest_commit_info$commit) &&
        !is.null(latest_commit_info$message)
    ) {
      # Check if the issue is approved
      if (
        !is.null(latest_commit_info$approved) &&
          !latest_commit_info$approved
      ) {
        return("Issue is not approved")
      }

      # Issue is approved, no validation message needed
      return(NULL)
    }
  }

  # If we can't determine commit info, fall back to open/closed check
  if (issue_info$open[1]) {
    return("Issue is Open")
  }

  # Issue is closed but no commit found
  return(NULL)
}

# Helper function to check for branch mismatch warning on commit dropdowns
check_branch_mismatch_warning <- function(
  file_name,
  input,
  branch_warning_needed
) {
  # Only show warning if branch mismatch is detected
  if (!branch_warning_needed) {
    return(NULL)
  }

  # Check if this file has a milestone selected (auto-selected commits)
  milestone_input_id <- generate_input_id("milestones", file_name)
  milestone_value <- input[[milestone_input_id]]

  # If milestone is selected, commits are auto-selected, no warning needed
  if (!is.null(milestone_value) && milestone_value != "") {
    return(NULL)
  }

  # File has no milestone selected and there's a branch mismatch
  # Show this warning immediately and persistently
  return(
    "Current branch does not match any Issue's branch. May be missing relevant commits"
  )
}
