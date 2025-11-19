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
        href = "ghqcr/css/styles.css"
      ),
      shiny::tags$script(type = "module", src = "ghqcr/js/adjust_grid.js"),
      shiny::tags$script(
        type = "module",
        src = "ghqcr/js/toggle_sidebar.js"
      ),
      shiny::tags$style(shiny::HTML(
        "
    ::placeholder {
      color: #8e8e8e; /* match colors of placeholders */
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
              src = "ghqcr/ghqc_hex.png",
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
    waiter::waiter_hide()
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

  local_commits <- .catch(get_branch_commits(working_dir, branch))
  .le$debug(
    "Found {local_commits$commit |> unique() |> length()} commits on branch '{branch}'"
  )

  shiny::moduleServer(id, function(input, output, session) {
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
        shiny::checkboxInput(
          session$ns("include_relevant_files"),
          label = "Include Relevant Files",
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
        return(tibble::tibble(name = character(0), milestone = character(0)))
      }
      selected_issues <- flattened_loaded_issues() |>
        dplyr::filter(milestone %in% input$selected_milestones) |>
        dplyr::arrange(factor(milestone, levels = input$selected_milestones))
      if (!input$include_open_issues) {
        selected_issues <- selected_issues |> dplyr::filter(!open)
      }
      selected_issues |>
        dplyr::select(name, milestone) |>
        dplyr::mutate(from_milestone = TRUE)
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
          selected_milestone_files(),
          file_tree_selected_files()
        ) |>
          dplyr::filter(milestone %in% avail_milestones)
      }
    )

    rendered_files <- shiny::reactiveVal(c())
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
        rendered_files(archive_files()$name)

        if (!is_empty(files_to_remove)) {
          .le$info(
            "Removing {length(files_to_remove)} ui rows for: {paste(files_to_remove, collapse = ', ')}"
          )
        }

        for (file in files_to_remove) {
          milestone_id <- generate_input_id("milestone", file)
          row_id <- generate_input_id("file_row", file) |> session$ns()
          attr_selector <- glue::glue("[id='{row_id}']")

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
            selector = paste0("#", session$ns("main_panel_dynamic")),
            where = "beforeEnd",
            ui = file_ui
          )
        }
      },
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

    shiny::observeEvent(
      input$include_relevant_files,
      {
        # browser()
      },
      ignoreInit = TRUE
    )
  })
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
    "file_row"
  )) {
    input_id[prefix] <- generate_input_id(prefix, file_name)
  }

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
      placeholder = "Select a commit"
    )
  )

  # Create the file row container
  .le$trace("Creating file row with id: {input_id$file_row}")
  shiny::div(
    id = ns(input_id$file_row),
    # shiny::div(class = "item-a", file_preview, style = "padding-bottom: 5px;"),
    shiny::div(
      class = "grid-items",
      shiny::div(class = "item-a", shiny::h5(file_name)),
      shiny::div(class = "item-b", milestone_input),
      shiny::div(class = "item-c", commit_input)
    )
  )
}
