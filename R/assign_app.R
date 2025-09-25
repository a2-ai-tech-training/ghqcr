ghqc_assign_app <- function(
  working_dir = here::here(),
  config_dir = NULL
) {
  tryCatch(
    {
      .le$debug("Loading configuration...")
      configuration <- .catch(get_configuration_extr(config_dir))
      checklists <- .catch(get_checklists_extr(configuration))
      checklist_display_name <- get_checklist_display_name_extr(
        configuration
      )
      prepended_checklist_note <- get_prepended_checklist_note_extr(
        configuration
      )

      .le$debug("Getting Milestones...")
      milestones <- get_milestones(working_dir)

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
        repo_users = repo_users,
        milestones = milestones
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
  repo_users,
  milestones
) {
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

    # Reset/load
    reset_rv <- shiny::reactiveVal(FALSE)
    session$onSessionEnded(function() {
      if (!isTRUE(shiny::isolate(reset_rv()))) {
        shiny::stopApp()
      }
    })

    w_load_items <- waiter::Waiter$new(
      id = session$ns("main_panel_dynamic"),
      html = shiny::tagList(
        waiter::spin_2(),
      ),
      color = "white"
    )

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
        shiny::req(input$new_milestone)
        milestone_input_rv(input$new_milestone)
      } else if (input$milestone_toggle == "Existing") {
        shiny::req(input$existing_milestone)
        milestone_input_rv(input$existing_milestone)
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

    output$main_panel_dynamic <- shiny::renderUI({
      shiny::req(selected_files())

      if (length(selected_files()) == 0) {
        return(shiny::HTML(
          "<div style='font-size: small !important; font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif !important; color: #a94442; font-weight: 700;'>No files selected (required)</div>"
        ))
      }

      w_load_items$show()

      ui_result <- render_selected_files(
        input,
        output,
        session$ns,
        selected_files = selected_files(),
        checklists$name,
        checklist_display_name
      )

      # Update assignee dropdowns after UI is rendered
      # Need to do this way for drop-down formatting
      shiny::observeEvent(
        ui_result,
        {
          update_assignees(session, selected_files(), repo_users)
          session$sendCustomMessage("adjust_grid", id)
        },
        once = TRUE
      )

      ui_result
    })

    shiny::observe({
      shiny::req(input$adjust_grid_finished)
      shiny::req(selected_files())
      shiny::req(checklists)
      for (file_name in selected_files()) {
        # validate checklist selection
        checklist_input_id <- generate_input_id("checklist", file_name)
        validator$add_rule(checklist_input_id, shinyvalidate::sv_required())

        # Create preview buttons
        create_file_preview(input, file_name, working_dir)
        create_checklist_preview(
          input,
          file_name,
          checklists,
          checklist_display_name
        )
      }
      w_load_items$hide()
    })

    shiny::observeEvent(input$create_qc_items, {
      shiny::req(milestone_input_rv())
      shiny::req()
      qc_waiter <- waiter::Waiter$new(
        id = session$ns("main_container"),
        html = shiny::tagList(
          waiter::spin_1(),
          shiny::h4("Creating QC Issues...", style = "color: white;")
        ),
        color = "darkgrey"
      )

      qc_waiter$show()

      file_data <- extract_file_data(
        input,
        selected_files(),
        checklists,
        repo_users
      )

      res <- create_issues_extr(
        milestone_input_rv(),
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
            )
          ),
          footer = NULL,
          easyClose = TRUE,
          shiny::HTML(markdown_to_html_extr(res))
        )
      }

      shiny::showModal(modal_dialog)
    })
  })
}

render_selected_files <- function(
  input,
  output,
  ns,
  selected_files,
  checklist_names,
  checklist_display_name
) {
  ul <- shiny::div(class = "grid-container-depth-0")

  for (file_name in selected_files) {
    input_id <- list()
    for (prefix in c(
      "checklist",
      "assignee",
      "file_preview",
      "checklist_preview"
    )) {
      input_id[prefix] <- generate_input_id(prefix, file_name)
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
      selected = NULL,
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

    ul <- shiny::tagAppendChild(
      ul,
      shiny::div(class = "item-a", file_preview, style = "padding-bottom: 5px;")
    )

    ul <- shiny::tagAppendChild(
      ul,
      shiny::div(
        class = "grid-items",
        shiny::div(class = "item-a", assignee_input),
        shiny::div(class = "item-b", checklist_input),
        shiny::div(class = "item-c", checklist_preview),
      )
    )
  }

  ul
}

update_assignees <- function(session, selected_files, repo_users) {
  for (file_name in selected_files) {
    assignee_input_id <- generate_input_id("assignee", file_name)

    shiny::updateSelectizeInput(
      session,
      assignee_input_id,
      server = TRUE,
      choices = repo_users,
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
    # Checklist input must correspond to a known checklist
    checklist <- if (checklist_name %in% checklists$name) {
      checklists[checklists$name == checklist_name, ]
    } else {
      return(NULL)
    }

    assignee_logins <- input[[generate_input_id("assignee", file_name)]]
    assignees_df <- repo_users |>
      dplyr::filter(login %in% assignee_logins)

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
