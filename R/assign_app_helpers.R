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
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "File Preview",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
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
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
        shiny::tags$div(
          shiny::modalButton("Return"),
          style = "flex: 0 0 auto;"
        ),
        shiny::tags$div(
          "Issue Creation Failed",
          style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
        ),
        shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
      ),
      footer = NULL,
      easyClose = TRUE,
      shiny::HTML(markdown_to_html_extr(res$value))
    )
  } else {
    shiny::modalDialog(
      title = shiny::tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
        shiny::tags$div(
          shiny::modalButton("Return"),
          style = "flex: 0 0 auto;"
        ),
        shiny::tags$div(
          "QC Assigned",
          style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
        ),
        shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
      ),
      footer = NULL,
      easyClose = TRUE,
      shiny::HTML(markdown_to_html_extr(res))
    )
  }

  shiny::showModal(modal_dialog)
}
