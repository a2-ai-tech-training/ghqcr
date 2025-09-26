ghqc_assign_app <- function(
  working_dir = here::here(),
  config_dir = NULL
) {
  tryCatch(
    {
      .le$debug("Loading configuration...")
      configuration <- .catch(get_configuration_extr(config_dir))
      checklists <- .catch(get_checklists_extr(configuration)) |>
        dplyr::arrange(name, name == "Custom")
      checklist_display_name <- get_checklist_display_name_extr(
        configuration
      )
      prepended_checklist_note <- get_prepended_checklist_note_extr(
        configuration
      )

      .le$debug("Getting Repo Users...")
      repo_users <- .catch(get_repo_users_extr(working_dir))
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
  .le$debug("Getting Milestones...")
  milestones <- get_milestones(working_dir)

  milestone_df <- purrr::map_dfr(milestones, function(x) {
    tibble::tibble(
      name = x$title,
      open = identical(x$state, "open")
    )
  })

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
    proceed_with_warnings <- shiny::reactiveVal(FALSE)

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
          issues <- .catch(get_milestone_issues_extr(working_dir, milestone))
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
        .le$debug(glue::glue(
          "Milestone {milestone_input_rv()} does not exist yet"
        ))
        issues_in_milestone_rv(NULL)

        list()
      } else {
        .le$debug(glue::glue(
          "Milestone {milestone_input_rv()} exists. Fetching issues..."
        ))

        issues <- .catch(get_milestone_issues_extr(working_dir, milestone))

        issues_in_milestone_rv(issues)

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
            shiny::tags$span(
              glue::glue("{capitalize(checklist_display_name)} Preview"),
              style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"
            ),
            shiny::modalButton("Dismiss"),
            style = "text-align: right;"
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

      .le$debug(glue::glue(
        "{capitalize(checklist_display_name)} selected for review: {input$checklist_name}"
      ))
      checklist <- checklists[checklists$name == input$checklist_name, ]
      shiny::HTML(.catch(format_checklist_as_html_extr(checklist)))
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
            .le$debug(glue::glue(
              "Removing {file} from UI using selector: {attr_selector}"
            ))
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
          .le$debug(glue::glue(
            "Removing {file} from UI using selector: {attr_selector}"
          ))
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
      shiny::req(milestone_input_rv())

      # Reset proceed flag
      proceed_with_warnings(FALSE)

      # Check git status for selected files
      git_statuses <- file_git_status_extr(selected_files(), working_dir)

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

      modal_check <- assign_modal_check(git_statuses, duplicate_files)

      if (!is.null(modal_check$message)) {
        if (modal_check$state == "warning") {
          shiny::showModal(shiny::modalDialog(
            title = shiny::tags$div(
              shiny::tags$span(
                "Warning",
                style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"
              ),
              shiny::actionButton(session$ns("proceed"), "Proceed Anyway"),
              shiny::actionButton(session$ns("return"), "Return"),
              style = "text-align: right;"
            ),
            shiny::HTML(modal_check$message),
            footer = NULL,
            easyClose = TRUE
          ))
        } else if (modal_check$state == "error") {
          shiny::showModal(shiny::modalDialog(
            title = shiny::tags$div(
              shiny::tags$span(
                "Error",
                style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"
              ),
              shiny::actionButton(session$ns("return"), "Return"),
              style = "text-align: right;"
            ),
            shiny::HTML(modal_check$message),
            footer = NULL,
            easyClose = TRUE
          ))
        }
        return()
      }

      # If no issues, proceed directly to create issues
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

    # Handle "Proceed Anyway" button click
    shiny::observeEvent(input$proceed, {
      shiny::removeModal()
      proceed_with_warnings(TRUE)
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

    # Handle "Return" button click
    shiny::observeEvent(input$return, {
      shiny::removeModal()
      proceed_with_warnings(FALSE)
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

# render_selected_files <- function(
#   input,
#   output,
#   ns,
#   selected_files,
#   checklist_names,
#   checklist_display_name,
#   input_selections = NULL
# ) {
#   ul <- shiny::div(class = "grid-container-depth-0")

#   for (file_name in selected_files) {
#     input_id <- list()
#     for (prefix in c(
#       "checklist",
#       "assignee",
#       "file_preview",
#       "checklist_preview"
#     )) {
#       input_id[prefix] <- generate_input_id(prefix, file_name)
#     }

#     assignee_input <- shiny::selectizeInput(
#       ns(input_id$assignee),
#       label = NULL,
#       choices = NULL,
#       selected = NULL,
#       multiple = TRUE,
#       options = list(
#         placeholder = "QCer(s) (Optional)"
#       )
#     )

#     # Get the previously selected checklist for this file
#     current_checklist_selection <- if (!is.null(input_selections)) {
#       input_selections[[input_id$checklist]]
#     } else {
#       NULL
#     }

#     checklist_input <- shiny::selectizeInput(
#       ns(input_id$checklist),
#       label = NULL,
#       choices = c("", checklist_names),
#       width = "100%",
#       selected = current_checklist_selection,
#       options = list(
#         placeholder = capitalize(checklist_display_name)
#       )
#     )

#     file_preview <- shiny::actionButton(
#       ns(input_id$file_preview),
#       label = shiny::HTML(gsub(
#         "([^a-zA-Z0-9])",
#         "\\1<wbr>",
#         generate_input_id(name = file_name)
#       )),
#       style = "font-size: 12px !important; font-weight: bold; padding: 2px 2px 2px 2px !important; color: #5f5f5f !important; line-height: 1.2em",
#       class = "file-preview-button"
#     )

#     checklist_preview <- shiny::actionButton(
#       ns(input_id$checklist_preview),
#       label = shiny::HTML(glue::glue(
#         "<span>Preview<br>{checklist_display_name}</span>"
#       )),
#       style = "height: 34px !important; font-size: 12px !important; padding: 2px 2px 2px 2px !important; color: #5f5f5f !important; line-height: 1.2em",
#       class = "checklist-preview-button"
#     )

#     ul <- shiny::tagAppendChild(
#       ul,
#       shiny::div(class = "item-a", file_preview, style = "padding-bottom: 5px;")
#     )

#     ul <- shiny::tagAppendChild(
#       ul,
#       shiny::div(
#         class = "grid-items",
#         shiny::div(class = "item-a", assignee_input),
#         shiny::div(class = "item-b", checklist_input),
#         shiny::div(class = "item-c", checklist_preview),
#       )
#     )
#   }

#   ul
# }

create_single_file_ui <- function(
  file_name,
  ns,
  checklist_names,
  checklist_display_name,
  input_selections = NULL
) {
  input_id <- list()
  for (prefix in c(
    "checklist",
    "assignee",
    "file_preview",
    "checklist_preview",
    "file_row"
  )) {
    input_id[prefix] <- generate_input_id(prefix, file_name)
  }

  # Get previously selected checklist for this file
  current_checklist_selection <- if (!is.null(input_selections)) {
    tryCatch(
      {
        shiny::isolate(input_selections[[input_id$checklist]])
      },
      error = function(e) {
        NULL
      }
    )
  } else {
    NULL
  }

  assignee_input <- shiny::selectizeInput(
    ns(input_id$assignee),
    label = NULL,
    choices = NULL,
    selected = NULL,
    multiple = TRUE,
    options = list(
      placeholder = "QCer(s) (Optional)"
    )
  )

  checklist_input <- shiny::selectizeInput(
    ns(input_id$checklist),
    label = NULL,
    choices = c("", checklist_names),
    width = "100%",
    selected = current_checklist_selection,
    options = list(
      placeholder = capitalize(checklist_display_name)
    )
  )

  file_preview <- shiny::actionButton(
    ns(input_id$file_preview),
    label = shiny::HTML(gsub(
      "([^a-zA-Z0-9])",
      "\\1<wbr>",
      generate_input_id(name = file_name)
    )),
    style = "font-size: 12px !important; font-weight: bold; padding: 2px 2px 2px 2px !important; color: #5f5f5f !important; line-height: 1.2em",
    class = "file-preview-button"
  )

  checklist_preview <- shiny::actionButton(
    ns(input_id$checklist_preview),
    label = shiny::HTML(glue::glue(
      "<span>Preview<br>{checklist_display_name}</span>"
    )),
    style = "height: 34px !important; font-size: 12px !important; padding: 2px 2px 2px 2px !important; color: #5f5f5f !important; line-height: 1.2em",
    class = "checklist-preview-button"
  )

  # Create the file row container
  shiny::div(
    id = ns(input_id$file_row),
    shiny::div(class = "item-a", file_preview, style = "padding-bottom: 5px;"),
    shiny::div(
      class = "grid-items",
      shiny::div(class = "item-a", assignee_input),
      shiny::div(class = "item-b", checklist_input),
      shiny::div(class = "item-c", checklist_preview)
    )
  )
}

update_assignees <- function(
  session,
  selected_files,
  repo_users,
  input_selections = NULL
) {
  for (file_name in selected_files) {
    assignee_input_id <- generate_input_id("assignee", file_name)

    # Get previously saved selection for this input
    saved_selection <- if (!is.null(input_selections)) {
      # Access reactive value safely
      tryCatch(
        {
          shiny::isolate(input_selections[[assignee_input_id]])
        },
        error = function(e) {
          NULL
        }
      )
    } else {
      NULL
    }

    shiny::updateSelectizeInput(
      session,
      assignee_input_id,
      server = TRUE,
      choices = repo_users,
      selected = saved_selection,
      options = list(
        placeholder = "QCer(s) (Optional)",
        multiple = TRUE,
        valueField = "login",
        labelField = "login",
        searchField = c("login", "name"),
        render = I(
          '{ option: function(item, escape) {
            if (item.name !== null) {
              return "<div><strong>" + escape(item.login) + "</strong> (" + escape(item.name) +") </div>"
            } else {
              return "<div><strong>" + escape(item.login) + "</div>"
            }
          }}'
        )
      )
    )
  }
}

create_file_preview <- function(input, file_name, working_dir) {
  id <- generate_input_id("file_preview", file_name)

  shiny::observeEvent(
    input[[id]],
    {
      content <- read_to_string_extr(file.path(working_dir, file_name))
      preview <- if (is.null(content)) {
        shiny::div("File preview not available")
      } else {
        shiny::pre(content)
      }

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tags$div(
            shiny::tags$span(
              "File Preview",
              style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"
            ),
            shiny::modalButton("Dismiss"),
            style = "text-align: right;"
          ),
          footer = NULL,
          easyClose = TRUE,
          preview
        )
      )
    },
    ignoreInit = TRUE
  )
}

create_checklist_preview <- function(
  input,
  file_name,
  checklists,
  checklist_display_name
) {
  preview_input_id <- generate_input_id("checklist_preview", file_name)
  checklist_input_id <- generate_input_id("checklist", file_name)

  shiny::observeEvent(
    input[[preview_input_id]],
    {
      selected_checklist <- input[[checklist_input_id]]
      content <- if (selected_checklist %in% checklists$name) {
        checklist_content <- format_checklist_as_html_extr(checklists[
          checklists$name == selected_checklist,
        ])
        shiny::HTML(checklist_content)
      } else {
        glue::glue(
          "Select a {checklist_display_name} to preview in the {capitalize(checklist_display_name)} dropdown."
        )
      }

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tags$div(
            shiny::tags$span(
              glue::glue("{capitalize(checklist_display_name)} Preview"),
              style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"
            ),
            shiny::modalButton("Dismiss"),
            style = "text-align: right;"
          ),
          footer = NULL,
          easyClose = TRUE,
          shiny::renderUI(content)
        )
      )
    },
    ignoreInit = TRUE
  )
}

extract_file_data <- function(input, selected_files, checklists, repo_users) {
  file_data <- list()
  for (file_name in selected_files) {
    checklist_name <- input[[generate_input_id("checklist", file_name)]]

    # Validate checklist selection
    if (
      is.null(checklist_name) ||
        checklist_name == "" ||
        !checklist_name %in% checklists$name
    ) {
      .le$debug(glue::glue("Checklist not selected for file {file_name}"))
      return(NULL)
    }

    checklist <- checklists[checklists$name == checklist_name, ]

    assignee_logins <- input[[generate_input_id("assignee", file_name)]]

    # Handle case where no assignees are selected (assignees are optional)
    assignees_df <- if (
      is.null(assignee_logins) || length(assignee_logins) == 0
    ) {
      data.frame(login = character(0), name = character(0))
    } else {
      repo_users |>
        dplyr::filter(login %in% assignee_logins)
    }

    # Convert dataframes to list structures that Rust can deserialize
    # Convert assignees dataframe to list of objects
    assignees_list <- if (nrow(assignees_df) > 0) {
      lapply(1:nrow(assignees_df), function(i) {
        list(
          login = assignees_df$login[i],
          name = assignees_df$name[i]
        )
      })
    } else {
      list()
    }

    # Convert checklist dataframe to single object
    checklist_obj <- list(
      name = checklist$name[1],
      content = checklist$content[1]
    )

    # Create a single file data object that matches RFileData struct
    file_item <- list(
      name = file_name,
      assignees = assignees_list,
      checklist = checklist_obj
    )

    file_data <- append(file_data, list(file_item))
  }
  file_data
}

assign_modal_check <- function(git_statuses, duplicate_files = character(0)) {
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
  message_html <- markdown_to_html_extr(paste(messages, collapse = "\n\n"))

  return(list(message = message_html, state = state))
}

create_qc_issues <- function(
  milestone_name,
  selected_files,
  checklists,
  repo_users,
  milestones,
  prepended_checklist_note,
  working_dir,
  input,
  session
) {
  qc_waiter <- waiter::Waiter$new(
    id = session$ns("main_container"),
    html = shiny::tagList(
      waiter::spin_1(),
      shiny::h4("Creating QC Issues...", style = "color: white;")
    ),
    color = "darkgrey"
  )

  qc_waiter$show()

  # Extract file data when we actually need it
  file_data <- extract_file_data(
    input,
    selected_files,
    checklists,
    repo_users
  )

  res <- create_issues_extr(
    milestone_name,
    input$milestone_description,
    file_data,
    milestones,
    prepended_checklist_note,
    working_dir
  )

  qc_waiter$hide()

  modal_dialog <- if (inherits(res, "extendr_error")) {
    shiny::modalDialog(
      title = shiny::tags$div(
        shiny::tags$span(
          "Issue Creation Failed",
          style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"
        ),
        shiny::modalButton("Dismiss"),
        style = "text-align: right;"
      ),
      footer = NULL,
      easyClose = TRUE,
      shiny::HTML(markdown_to_html_extr(res$value))
    )
  } else {
    shiny::modalDialog(
      title = shiny::tags$div(
        shiny::tags$span(
          "QC Assigned",
          style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"
        ),
        shiny::modalButton("Dismiss"),
        style = "text-align: right;"
      ),
      footer = NULL,
      easyClose = TRUE,
      shiny::HTML(markdown_to_html_extr(res))
    )
  }

  shiny::showModal(modal_dialog)
}
