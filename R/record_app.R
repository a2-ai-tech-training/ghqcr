#' Launch GHQC Record App
#'
#' Launch the QC Record app in the foreground for generating QC records.
#'
#' @param working_dir Character. Path to the working directory containing the
#'   git repository. Defaults to the current working directory via here::here().
#' @param config_dir Character. Path to the configuration directory. If NULL,
#'   uses default configuration location.
#'
#' @return Launches a Shiny app (no return value).
#'
#' @export
ghqc_record_app <- function(
  working_dir = here::here(),
  config_dir = NULL
) {
  tryCatch(
    {
      .le$debug("Loading configuration...")
      configuration <- .catch(get_configuration_impl(config_dir))
    },
    error = function(e) {
      stop("Failed to load data: ", conditionMessage(e))
    }
  )

  app <- shiny::shinyApp(
    ui = ghqc_record_ui(id = "ghqc_record_app"),
    server = function(input, output, session) {
      ghqc_record_server(
        id = "ghqc_record_app",
        working_dir = working_dir,
        configuration = configuration
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
NULL

ghqc_record_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- miniUI::miniPage(
    waiter::use_waiter(),
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "ghqc/css/styles.css"
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
          style = "display: inline-flex; align-items: center; justify-content: center; width: 100%; height: 100%;",
          shiny::div(
            style = "position: relative; flex-shrink: 0; width: 50px; height: 50px;",
            shiny::tags$img(
              src = "ghqc/ghqc_hex.png",
              class = "logo-img",
              style = "height: 46px; !important;"
            ) # this is important to ensure style priority so logo is the correct size
          ),
          shiny::div("Generate QC Record", style = "white-space: nowrap;")
        ),
        left = shiny::actionButton(ns("close"), "Close", class = "btn-sm"),
        right = shiny::actionButton(ns("reset"), "Reset", class = "btn-sm")
      ),
      miniUI::miniContentPanel(
        shiny::div(
          id = ns("center_content"),
          shiny::selectizeInput(
            ns("selected_milestones"),
            "Select Milestones",
            choices = "",
            multiple = TRUE
          ),
          shiny::div(
            style = "margin-top: -20px;",
            shiny::checkboxInput(
              ns("include_open"),
              "Include Open Milestones",
              FALSE
            )
          ),
          shiny::textAreaInput(
            inputId = ns("record_path"),
            label = "Record File Path"
          ),
          shiny::checkboxInput(ns("just_tables"), "Just tables", FALSE)
        )
      ),
      div(
        class = "button_block",
        miniButtonBlock(
          actionButton(ns("generate_record"), "Generate QC Record")
        )
      )
    )
  )
  return(ui)
}

ghqc_record_server <- function(
  id,
  working_dir,
  configuration
) {
  shiny::moduleServer(id, function(input, output, session) {
    checklist_name <- get_checklist_display_name_impl(configuration)

    .le$debug("Fetching Milestones...")
    milestones <- get_milestones(working_dir)

    milestone_df <- create_safe_milestone_df(milestones)

    .le$debug(
      glue::glue(
        "Found {nrow(milestone_df)} milestones ({milestone_df |> dplyr::filter(!open) |> nrow()} closed)"
      )
    )

    validator <- shinyvalidate::InputValidator$new()
    validator$add_rule("selected_milestones", shinyvalidate::sv_required())
    validator$add_rule("record_path", shinyvalidate::sv_required())

    shiny::observe(waiter::waiter_hide())

    # Select Milestones dropdown update
    shiny::observeEvent(input$include_open, {
      .le$trace("Checked Include Open Milestones: {input$include_open}")
      previously_selected <- shiny::isolate(input$selected_milestones)

      placeholder <- "Select Milestone(s)"

      if (input$include_open || is_empty(milestone_df)) {
        milestone_options <- milestone_df |>
          dplyr::pull(name)

        if (is_empty(milestone_options)) {
          placeholder <- "No Milestones Available"
        }
      } else {
        milestone_options <- milestone_df |>
          dplyr::filter(!open) |>
          dplyr::pull(name)

        if (is_empty(milestone_options)) {
          placeholder <- "No Closed Milestones"
        }
      }

      shiny::updateSelectizeInput(
        session,
        "selected_milestones",
        choices = milestone_options,
        selected = previously_selected,
        options = list(placeholder = placeholder)
      )
    })

    repo_name <- normalizePath(working_dir, mustWork = FALSE) |>
      basename() |>
      as.character()

    if (is.na(repo_name) || !nzchar(repo_name)) {
      repo_name <- "repo"
    }

    custom_pdf_name <- shiny::reactiveVal(FALSE)

    # Track when user manually changes the record path
    shiny::observeEvent(
      input$record_path,
      {
        # Only set custom flag if the change wasn't programmatic
        # We can detect this by checking if the current value differs from what we would generate
        expected_name <- get_pdf_name(
          repo_name,
          input$selected_milestones %||% character(0),
          input$just_tables %||% FALSE
        )

        if (!is.null(input$record_path)) {
          if (nzchar(input$record_path) && input$record_path != expected_name) {
            .le$debug("Determined PDF path to be edited by user")
            custom_pdf_name(TRUE)
          } else if (!nzchar(input$record_path)) {
            # If user clears the field, reset to automatic naming
            .le$debug("PDF path is empty. Will reset to default...")
            custom_pdf_name(FALSE)
          }
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(c(input$selected_milestones, input$just_tables), {
      shiny::req(!custom_pdf_name())

      pdf_name <- get_pdf_name(
        repo_name,
        input$selected_milestones,
        input$just_tables
      )

      .le$debug("Updating pdf name to: {pdf_name}...")

      shiny::updateTextAreaInput(
        session,
        "record_path",
        value = pdf_name
      )
    })

    shiny::observe({
      shinyjs::removeClass("generate_record", "enabled-btn")
      shinyjs::addClass("generate_record", "disabled-btn")
      shinyjs::disable("generate_record")
      if (
        length(input$selected_milestones) != 0 &&
          nzchar(input$record_path)
      ) {
        .le$debug(
          "Enabling Generate QC Record button as all required values are present"
        )
        shinyjs::removeClass("generate_record", "disabled-btn")
        shinyjs::addClass("generate_record", "enabled-btn")
        shinyjs::enable("generate_record")
      }
    })

    selected_milestone_objects <- shiny::reactive(
      milestones[milestone_df$name %in% input$selected_milestones]
    )

    milestone_issue_information <- shiny::eventReactive(input$generate_record, {
      w_check_status <- waiter::Waiter$new(
        id = session$ns("main_container"),
        html = shiny::tagList(
          waiter::spin_1(),
          shiny::h4(
            "Loading selected Milestone Issues...",
            style = "color: white;"
          )
        ),
        color = "darkgrey"
      )
      w_check_status$show()
      on.exit(w_check_status$hide())

      .le$debug(
        "Loading issue information for the selected milestones: {paste(selected_milestone_objects(), collapse = \", \")}"
      )
      .catch(get_milestone_issue_information_impl(
        selected_milestone_objects(),
        working_dir
      ))
    })

    generate_record <- shiny::reactiveVal(FALSE)
    reset_triggered <- shiny::reactiveVal(FALSE)
    session$onSessionEnded(function() {
      if (!isTRUE(shiny::isolate(reset_triggered()))) {
        stopApp()
      }
    })

    shiny::observeEvent(milestone_issue_information(), {
      .le$debug("Checking issue states...")
      modal_msg <- .catch(record_issue_modal_check_impl(
        milestone_issue_information(),
        checklist_name
      ))

      # If no modal message, we return like the modal was "proceed anyway"
      if (!nzchar(modal_msg)) {
        .le$debug("No notes to notify. Creating record...")
        generate_record(TRUE)
        return()
      }
      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::actionButton(session$ns("return"), "Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              shiny::HTML("<span style='font-size: 24px; vertical-align: middle;'>&#9888;</span> Warning"),
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(
              shiny::actionButton(session$ns("proceed"), "Proceed Anyway"),
              style = "flex: 0 0 auto;"
            )
          ),
          shiny::HTML(markdown_to_html_impl(modal_msg)),
          footer = NULL,
          easyClose = TRUE
        )
      )
    })

    shiny::observeEvent(input$proceed, {
      shiny::removeModal()
      .le$trace("Proceed button clicked")
      generate_record(TRUE)
    })

    shiny::observeEvent(input$return, {
      shiny::removeModal()
      .le$trace("Return button clicked")
      generate_record(FALSE)
    })

    shiny::observeEvent(generate_record(), {
      shiny::req(generate_record())
      .le$debug("Rendering QC Record...")
      w_render_record <- waiter::Waiter$new(
        id = session$ns("main_container"),
        html = shiny::tagList(
          waiter::spin_1(),
          shiny::h4(
            "Rendering QC Record...",
            style = "color: white;"
          )
        ),
        color = "darkgrey"
      )
      w_render_record$show()
      on.exit(w_render_record$hide())

      res <- tryCatch(
        {
          generate_record_impl(
            milestones = selected_milestone_objects(),
            milestone_issues = milestone_issue_information(),
            configuration = configuration,
            record_path = input$record_path,
            working_dir = working_dir,
            just_tables = input$just_tables
          )
        },
        error = function(e) {
          paste("Error:", conditionMessage(e))
        }
      )

      .le$debug("Record finished rendering. Creating modal...")

      # Show modal with the result
      show_result_modal(session, res)
    })

    # Handle close button for result modal
    shiny::observeEvent(input$close_result_modal, {
      shiny::removeModal()
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

    validator$enable()
  })
}

show_result_modal <- function(session, result) {
  # Determine if the result indicates success or error
  is_success <- !grepl("^Error:", result)

  # Create appropriate title and styling
  if (is_success) {
    title <- "<span style='font-size: 20px; vertical-align: middle;'>&#10004;</span> Record Generated Successfully"
    title_class <- "text-success"
    message <- result
  } else {
    title <- "<span style='font-size: 20px; vertical-align: middle;'>&#10060;</span> Record Generation Failed"
    title_class <- "text-danger"
    message <- result
  }

  shiny::showModal(
    shiny::modalDialog(
      title = shiny::tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
        shiny::tags$div(
          shiny::actionButton(session$ns("close_result_modal"), "Return"),
          style = "flex: 0 0 auto;"
        ),
        shiny::tags$div(
          shiny::HTML(title),
          class = title_class,
          style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
        ),
        shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
      ),
      shiny::tags$div(
        style = "padding: 15px; font-family: monospace; background-color: #f8f9fa; border-radius: 5px; margin: 10px 0;",
        message
      ),
      NULL,
      footer = NULL,
      easyClose = TRUE,
      size = "l"
    )
  )
}

get_pdf_name <- function(repo, milestone_names, just_tables) {
  if (is_empty(milestone_names)) {
    return(glue::glue("{repo}.pdf"))
  }

  milestone_str <- glue::glue_collapse(milestone_names, "-")

  base_name <- glue::glue("{repo}-{milestone_str}")

  if (just_tables) {
    base_name <- glue::glue("tables-{base_name}")
  }

  # file name at max 64 chars (60 + .pdf)
  if (nchar(base_name) > 60) {
    base_name <- substr(base_name, 1, 60)
  }

  # replace spaces with -
  clean_name <- stringr::str_replace_all(base_name, " ", "-")

  # remove special characters except for dashes and numbers
  clean_name <- stringr::str_remove_all(clean_name, "[^0-9A-Za-z\\-_]")

  # make lowercase
  clean_name <- tolower(clean_name)

  pdf_name <- glue::glue("{clean_name}.pdf")
  return(pdf_name)
}
