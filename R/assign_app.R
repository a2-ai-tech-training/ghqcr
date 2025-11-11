ghqc_assign_app <- function(
  working_dir = here::here(),
  config_dir = NULL
) {
  tryCatch(
    {
      .le$info("Loading Configuration...")
      configuration <- .catch(get_configuration_impl(config_dir))
      .le$trace("Configuration loaded. Parsing info out...")
      checklists <- .catch(get_checklists_impl(configuration)) |>
        dplyr::arrange(name, name == "Custom")
      .le$trace("Fetched {length(checklists)} checklists")
      checklist_display_name <- get_checklist_display_name_impl(
        configuration
      )
      .le$trace(
        "Determined checklist display name to be: {checklist_display_name}"
      )
      prepended_checklist_note <- get_prepended_checklist_note_impl(
        configuration
      )
      .le$trace(
        "Determined prepended checklist note to be: {prepended_checklist_note}"
      )

      .le$info("Loading Repo Users...")
      repo_users <- .catch(get_users_impl(working_dir))
      .le$debug("Found {nrow(repo_users)} repo users")
    },
    error = function(e) {
      stop("Failed to load data: ", conditionMessage(e))
    }
  )

  app <- shiny::shinyApp(
    ui = ghqc_assign_ui(id = "ghqc_assign_app"),
    server = function(input, output, session) {
      ghqc_assign_server(
        id = "ghqc_assign_app",
        working_dir = working_dir,
        checklists = checklists,
        checklist_display_name = checklist_display_name,
        prepended_checklist_note = prepended_checklist_note,
        repo_users = repo_users
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5254))
  shiny::runApp(app, port = port)
}

#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniButtonBlock
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter waiter_show_on_load spin_1
ghqc_assign_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- miniUI::miniPage(
    waiter::use_waiter(),
    shinyjs::useShinyjs(),
    # tags$head(tags$style(HTML(brio::read_file(system.file("css/styles.css", package = "ghqcr"))))),
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
          shiny::div("Assign QC file(s)", style = "white-space: nowrap;")
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
          shiny::actionButton(ns("create_qc_items"), "Assign QC file(s)")
        )
      )
    )
  )

  return(ui)
}

ghqc_assign_server <- function(
  id,
  working_dir,
  checklists,
  checklist_display_name,
  prepended_checklist_note,
  repo_users
) {
  .le$info("Fetching Milestones...")
  milestones <- get_milestones(working_dir)

  milestone_df <- create_safe_milestone_df(milestones)
  .le$debug(
    glue::glue(
      "Found {nrow(milestone_df)} milestones ({milestone_df |> dplyr::filter(open) |> nrow()} open)"
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

  shiny::moduleServer(id, function(input, output, session) {
    milestone_input_rv <- shiny::reactiveVal(NULL)
    issues_in_milestone_rv <- shiny::reactiveVal(NULL)

    # Reactive values for modal flow control
    ready_to_proceed <- shiny::reactiveVal(FALSE)

    # Store current input selections to persist them during UI updates
    input_selections <- shiny::reactiveValues()

    # Reset/load
    reset_rv <- shiny::reactiveVal(FALSE)
    session$onSessionEnded(function() {
      if (!isTRUE(shiny::isolate(reset_rv()))) {
        shiny::stopApp()
      }
    })

    # Get the open milestones and format when no existing milestone exist
    open_milestones <- if (length(milestone_df) == 0) {
      c()
    } else {
      milestone_df |>
        dplyr::filter(open) |>
        dplyr::arrange(dplyr::desc(number)) |>
        dplyr::pull(name)
    }

    if (length(open_milestones) == 0) {
      shiny::updateSelectizeInput(
        session,
        "existing_milestone",
        options = list(placeholder = "No open Milestones")
      )
    }

    # Set the existing milestone options
    shiny::observe({
      shiny::req(open_milestones)

      shiny::updateSelectizeInput(
        session,
        "existing_milestone",
        choices = open_milestones
      )
    })

    # set what the selected milestone is
    shiny::observe({
      shiny::req(input$milestone_toggle)

      if (input$milestone_toggle == "New") {
        # Clear any existing file highlighting when switching to New
        session$sendCustomMessage("highlightPaths", list())
        shiny::req(input$new_milestone)
        milestone_input_rv(input$new_milestone)
      } else if (input$milestone_toggle == "Existing") {
        shiny::req(input$existing_milestone)
        milestone_input_rv(input$existing_milestone)

        # Force re-highlighting when switching to existing (even if same milestone)
        milestone_index <- which(milestone_df$name == input$existing_milestone)
        if (length(milestone_index) > 0) {
          milestone <- milestones[[milestone_index[1]]]
          issues <- .catch(get_milestone_issues_impl(working_dir, milestone))
          issues_files <- sapply(issues, function(issue) {
            file.path(basename(working_dir), issue$title)
          })
          session$sendCustomMessage("highlightPaths", issues_files)
        }
      }
    })

    output$sidebar <- shiny::renderUI({
      # Hide the global waiter when sidebar is rendered
      waiter::waiter_hide()

      shiny::tagList(
        shiny::radioButtons(
          inputId = session$ns("milestone_toggle"),
          label = "Milestone State",
          choices = c("New", "Existing"),
          inline = TRUE
        ),
        # Drop down when 'Existing'
        shiny::conditionalPanel(
          condition = "input.milestone_toggle == 'Existing'",
          ns = session$ns,
          shiny::selectizeInput(
            inputId = session$ns("existing_milestone"),
            label = "Select Existing Milestone",
            choices = "",
            multiple = FALSE,
            width = "100%",
            options = list(placeholder = "(Required)")
          )
        ),
        # text box for name and description when 'New'
        shiny::conditionalPanel(
          condition = "input.milestone_toggle == 'New'",
          ns = session$ns,
          shiny::textAreaInput(
            inputId = session$ns("new_milestone"),
            label = "Milestone Name",
            placeholder = "Milestone Name",
            width = "100%"
          ),
          shiny::textAreaInput(
            inputId = session$ns("milestone_description"),
            label = "Milestone Description",
            placeholder = "(Optional)",
            width = "100%",
          )
        ),
        div(
          style = "font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif !important; font-weight: bold;",
          "Select File(s) for QC"
        ),
        treeNavigatorUI(session$ns("treeNavigator"))
      )
    })

    # validate new milestone is present or an existing milestone is selected
    shiny::observe({
      shiny::req(input$milestone_toggle)
      if (input$milestone_toggle == "New") {
        .le$trace("New Milestone radio button selected")
        validator$add_rule("new_milestone", shinyvalidate::sv_required())
        # Add custom rule to check if milestone name matches a closed milestone
        validator$add_rule("new_milestone", function(value) {
          if (is.null(value) || value == "") {
            return(NULL)
          }

          # Check if this name matches any closed milestone
          closed_milestones <- milestone_df[
            !milestone_df$open,
            "name",
            drop = TRUE
          ]
          if (value %in% closed_milestones) {
            return(
              "Closed Milestone found with the same name"
            )
          }
          return(NULL)
        })
      } else {
        .le$trace("Existing Milestone radio button selected")
        validator$add_rule("existing_milestone", shinyvalidate::sv_required())
      }
    })

    # Issue fetching for milestones
    shiny::observe({
      req(milestone_input_rv())
      milestone_index <- which(milestone_df$name == milestone_input_rv())
      milestone <- if (length(milestone_index) > 0) {
        milestones[[milestone_index[1]]]
      } else {
        NULL
      }
      issues_files <- if (is.null(milestone)) {
        .le$debug("Milestone '{milestone_input_rv()}' does not exist yet")
        issues_in_milestone_rv(NULL)

        # Update milestone description field for new milestone
        if (
          !is.null(input$milestone_toggle) &&
            input$milestone_toggle == "New" &&
            !is.null(input$new_milestone) &&
            nchar(trimws(input$new_milestone)) > 0
        ) {
          shinyjs::enable("milestone_description")
          shiny::updateTextAreaInput(
            session,
            "milestone_description",
            placeholder = "(Optional)"
          )
        }

        list()
      } else {
        .le$debug(
          "Milestone {milestone$number} - '{milestone_input_rv()}' exists. Fetching issues..."
        )

        issues <- .catch(get_milestone_issues_impl(working_dir, milestone))

        issues_in_milestone_rv(issues)

        # Update milestone description field for existing milestone
        if (
          !is.null(input$milestone_toggle) &&
            input$milestone_toggle == "New" &&
            !is.null(input$new_milestone) &&
            nchar(trimws(input$new_milestone)) > 0
        ) {
          .le$debug(
            "Input Milestone Exists. Disabling Milestone Description field..."
          )
          shinyjs::disable("milestone_description")
          shiny::updateTextAreaInput(
            session,
            "milestone_description",
            value = "",
            placeholder = "Description cannot be modified for existing milestones"
          )
        }

        sapply(issues, function(issue) {
          file.path(basename(working_dir), issue$title)
        })
      }

      session$sendCustomMessage("highlightPaths", issues_files)
    })

    # All checklists preview
    output$main_panel_static <- shiny::renderUI({
      shiny::div(
        style = "display: flex; justify-content: flex-end; padding-bottom: 20px;",
        shiny::actionButton(
          session$ns("checklist_options"),
          label = shiny::HTML(glue::glue(
            "<span style='font-size:2.0em;'>Preview {checklist_display_name} options</span>"
          )),
          class = "preview-button",
          style = "min-width: auto; display: inline-block; text-align: right; line-height: 2em; height: 2em;"
        )
      )
    })

    shiny::observeEvent(input$checklist_options, {
      shiny::req(checklists)
      .le$trace("checklist options button was clicked")

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              glue::glue("{capitalize(checklist_display_name)} Preview"),
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
          ),
          footer = NULL,
          easyClose = TRUE,
          shiny::selectInput(
            session$ns("checklist_name"),
            NULL,
            choices = checklists$name,
            width = "100%"
          ),
          shiny::uiOutput(session$ns("checklist_info_panel"))
        )
      )
    })

    output$checklist_info_panel <- shiny::renderUI({
      shiny::req(checklists)
      shiny::req(input$checklist_name)

      .le$debug(
        "{capitalize(checklist_display_name)} selected for review: {input$checklist_name}"
      )
      checklist <- checklists[checklists$name == input$checklist_name, ]
      shiny::HTML(.catch(format_checklist_as_html_impl(checklist)))
    })

    # Select Files Rows
    output$validation_message <- shiny::renderUI({
      shiny::validate(
        shiny::need(
          length(selected_files()) > 0,
          shiny::HTML("<div style='color: #d9534f;'>No files selected</div>")
        )
      )
    })

    # Track currently rendered files to manage dynamic UI
    rendered_files <- shiny::reactiveVal(character(0))

    output$main_panel_dynamic <- shiny::renderUI({
      # Static container that doesn't get re-rendered
      shiny::div(
        id = session$ns("files_container"),
        class = "grid-container-depth-0"
      )
    })

    # Observe changes to selected files and manage UI dynamically
    shiny::observeEvent(
      selected_files(),
      {
        current_files <- selected_files()
        previous_files <- rendered_files()

        if (length(current_files) == 0) {
          # Remove all files and show message
          for (file in previous_files) {
            file_id <- session$ns(generate_input_id("file_row", file))
            attr_selector <- paste0("[id='", file_id, "']")
            .le$debug("Removing {file} from UI using selector: {attr_selector}")
            shiny::removeUI(selector = attr_selector)
          }
          rendered_files(character(0))

          if (length(previous_files) > 0) {
            shiny::insertUI(
              selector = paste0("#", session$ns("files_container")),
              where = "afterBegin",
              ui = shiny::div(
                id = session$ns("no_files_message"),
                style = "font-size: small !important; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif !important; color: #a94442; font-weight: 700;",
                "No files selected (required)"
              )
            )
          }
          return()
        }

        # Remove "no files" message if it exists
        shiny::removeUI(selector = paste0("#", session$ns("no_files_message")))

        # Add new files
        files_to_add <- setdiff(current_files, previous_files)
        # Remove files that are no longer selected
        files_to_remove <- setdiff(previous_files, current_files)

        # Remove files that are no longer selected
        for (file in files_to_remove) {
          file_id <- session$ns(generate_input_id("file_row", file))
          attr_selector <- paste0("[id='", file_id, "']")
          .le$debug("Removing {file} from UI using selector: {attr_selector}")
          shiny::removeUI(selector = attr_selector)
        }

        # Add new files
        for (file in files_to_add) {
          file_ui <- create_single_file_ui(
            file,
            session$ns,
            checklists$name,
            checklist_display_name,
            input_selections
          )
          shiny::insertUI(
            selector = paste0("#", session$ns("files_container")),
            where = "beforeEnd",
            ui = file_ui
          )

          # Set up observers and validation for this new file
          local({
            current_file <- file

            # Set up validation
            checklist_input_id <- generate_input_id("checklist", current_file)
            validator$add_rule(checklist_input_id, shinyvalidate::sv_required())

            # Create preview button observers
            create_file_preview(input, current_file, working_dir)
            create_checklist_preview(
              input,
              current_file,
              checklists,
              checklist_display_name
            )

            # Create observers for input selections to persist them
            for (prefix in c("assignee", "checklist")) {
              input_id <- generate_input_id(prefix, current_file)
              shiny::observeEvent(
                input[[input_id]],
                {
                  input_selections[[input_id]] <- input[[input_id]]
                },
                ignoreInit = TRUE
              )
            }

            # Update assignees after UI is ready
            later::later(
              function() {
                update_assignees(
                  session,
                  c(current_file),
                  repo_users,
                  input_selections
                )
              },
              delay = 0.1
            )
          })
        }

        rendered_files(current_files)

        # Update grid after all changes
        if (length(current_files) > 0) {
          session$sendCustomMessage("adjust_grid", id)
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$create_qc_items, {
      .le$trace("Assign QC Items button was clicked")
      shiny::req(milestone_input_rv())

      # Reset ready flag and trigger git status check
      ready_to_proceed(FALSE)

      # Check git status for selected files
      .le$debug("Determining file git statuses:")
      git_statuses <- tryCatch(
        {
          file_git_status_impl(selected_files(), working_dir)
        },
        error = function(e) {
          .le$error("Failed to get file git status: {e$message}")
          shiny::showModal(shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::actionButton(session$ns("return"), "Return"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "Git Status Error",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
            ),
            glue::glue("Could not check git status for files: {e$message}"),
            footer = NULL,
            easyClose = TRUE
          ))
          return(NULL)
        }
      )

      if (is.null(git_statuses)) {
        .le$debug("No git statuses to report. Proceeding...")
        return()
      }

      # Check for duplicate issues in milestone
      existing_issues <- issues_in_milestone_rv()
      duplicate_files <- if (
        !is.null(existing_issues) && length(existing_issues) > 0
      ) {
        existing_file_paths <- sapply(existing_issues, function(issue) {
          issue$title
        })
        intersect(selected_files(), existing_file_paths)
      } else {
        character(0)
      }

      # Check for git issues using the modal check function
      modal_check <- git_issue_modal_check(git_statuses, duplicate_files)

      if (!is.null(modal_check$message)) {
        # Show modal with git status warnings/errors
        modal_title_text <- if (modal_check$state == "error") {
          "Git Issues Found - Cannot Proceed"
        } else {
          "Git Status Warning"
        }

        # Create title with appropriate buttons
        modal_title <- if (modal_check$state == "error") {
          .le$warn(
            "Git statuses exist that prevent user from assigning files for QC"
          )
          # Error state: only Return button on left
          shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::actionButton(session$ns("return"), "Return"),
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
          .le$info(
            "Git statuses exist that warn user, but do not require action"
          )
          shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::actionButton(session$ns("return"), "Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              modal_title_text,
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(
              shiny::actionButton(session$ns("proceed"), "Proceed Anyway"),
              style = "flex: 0 0 auto;"
            )
          )
        }

        shiny::showModal(shiny::modalDialog(
          title = modal_title,
          shiny::HTML(modal_check$message),
          footer = NULL,
          easyClose = modal_check$state != "error"
        ))

        return()
      }

      # If we get here, git status is clean - proceed to issue creation
      .le$info("No git statuses to report. Proceeding...")
      ready_to_proceed(TRUE)
    })

    # Handle "Proceed Anyway" button click
    shiny::observeEvent(input$proceed, {
      .le$trace("User proceeded from modal check")
      shiny::removeModal()
      ready_to_proceed(TRUE)
    })

    # Handle "Return" button click
    shiny::observeEvent(input$return, {
      .le$trace("User returned from modal check")
      shiny::removeModal()
      ready_to_proceed(FALSE)
    })

    # Create issues when ready to proceed
    shiny::observeEvent(ready_to_proceed(), {
      shiny::req(ready_to_proceed() == TRUE)
      shiny::req(milestone_input_rv())

      # Reset the reactive val
      ready_to_proceed(FALSE)

      .le$info("Creating QC Issues...")
      # Create the QC issues
      create_qc_issues(
        milestone_name = milestone_input_rv(),
        selected_files = selected_files(),
        checklists = checklists,
        repo_users = repo_users,
        milestones = milestones,
        prepended_checklist_note = prepended_checklist_note,
        working_dir = working_dir,
        input = input,
        session = session
      )
    })

    shiny::observe({
      if (
        length(selected_files()) > 0 && shiny::isTruthy(milestone_input_rv())
      ) {
        file_data <- extract_file_data(
          input,
          selected_files(),
          checklists,
          repo_users
        )
        # Only null when a checklist is not selected
        if (!is.null(file_data)) {
          .le$debug(
            "Activating 'Create QC Item' button as all requirements are met"
          )
          shinyjs::removeClass("create_qc_items", "disable-btn")
          shinyjs::addClass("create_qc_items", "enable-btn")
          shinyjs::enable("create_qc_items")
          return()
        }
      }

      .le$debug(
        "'Create QC Item' button is inactivated as not all requirements are met"
      )
      shinyjs::removeClass("create_qc_items", "enable-btn")
      shinyjs::addClass("create_qc_items", "disable-btn")
      shinyjs::disable("create_qc_items")
    })

    shiny::observeEvent(input$close, {
      .le$debug("App was closed through the close button")
      shiny::stopApp()
    })

    shiny::observeEvent(input$reset, {
      .le$debug("App was reset through reset button")
      reset_rv(TRUE)
      session$reload()
    })

    validator$enable()
    return(input)
  })
}
