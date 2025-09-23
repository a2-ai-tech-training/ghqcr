ghqc_assign_app <- function(
  working_dir = here::here(),
  config_dir = NULL
) {
  cat("Starting ghqc_assign_app with working_dir:", working_dir, "\n")

  tryCatch({
    cat("Getting checklists...\n")
    checklists <- .catch(get_checklists_extr(config_dir))
    cat("Got checklists successfully\n")

    cat("Getting milestones...\n")
    milestones <- .catch(get_milestones_extr(working_dir))
    cat("Got milestones successfully, count:", length(milestones), "\n")

    milestone_df <- purrr::map_dfr(milestones, function(x) {
      tibble::tibble(
        name = x$title,
        open = identical(x$state, "open")
      )
    })
    cat("Created milestone_df successfully\n")

    cat("Getting repo users...\n")
    repo_users <- .catch(get_repo_users_extr(working_dir))
    cat("Got repo users successfully\n")
  }, error = function(e) {
    cat("Error during data loading:", conditionMessage(e), "\n")
    stop("Failed to load data: ", conditionMessage(e))
  })

  app <- shiny::shinyApp(
    ui = ghqc_assign_ui(id = "ghqc_assign_app"),
    server = function(input, output, session) {
      ghqc_assign_server(
        id = "ghqc_assign_app",
        working_dir = working_dir,
        checklists = checklists,
        repo_users = repo_users,
        milestone_df = milestone_df
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
  repo_users,
  milestone_df
) {
  working_dir_rv <- shiny::reactive(working_dir)

  selected_items <- treeNavigatorServer(
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
    open_milestones <- milestone_df |>
      dplyr::filter(open) |>
      dplyr::pull(name)

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
  })
}
