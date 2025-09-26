#' @import shiny
NULL

# repurposed some functions and js from https://github.com/stla/jsTreeR
# changes: allows more filtering of lists, excluded dir selections of only excluded
# files give modals w/ excluded file list, renames rootFolder to basename,
# redid id/naming so ns can be passed in

#' Generate Excluded File Message
#'
#' This function generates an HTML formatted message indicating which files are excluded
#' from selection as Issues in a directory.
#'
#' @param excluded_files A character vector of file paths that are excluded from selection.
#'
#' @return A character string containing an HTML formatted message listing the excluded files,
#' or an empty string if no files are excluded.
#' @examples
#' excluded_files <- c("path/to/file1.txt", "path/to/file2.pdf")
#' generate_excluded_file_message(excluded_files)
#' @noRd
generate_excluded_file_message <- function(excluded_files) {
  error_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#10071;</span>"
  messages <- c()
  if (length(excluded_files) > 0) {
    messages <- sprintf(
      "%s The selected directory contains only the following files which are not selectable QC items:<ul>%s</ul><br>",
      error_icon_html,
      paste0("<li>", basename(excluded_files), "</li>", collapse = "")
    )
  }
  return(messages)
}

#' Generate Exclude Patterns
#'
#' This function generates patterns to exclude binary files and specific directories,
#' such as the `renv` directory, from a file listing.
#'
#' @return A character string containing the exclusion patterns.
#' @noRd
exclude_patterns <- function() {
  # excludes binaries as won't be qc items
  exclude_pattern <- "\\.(rda|rds|RData|jpg|jpeg|pdf|png|bmp|gif|tif|tiff|emf|svgz|ico|webp|eps|ppm|pgm|pbm|pnm|o|so|rdb|rdx|woff2|woff|otf|ttf|eot|docx|xlsx|pptx|xltx|potx|doc|xls|ppt|odt|ods|odp|odg|odc|odf|odi|odm|odb|sas7bdat|sas7bcat|xpt|xpt5|xpt8|zip|tar|gz|tgz|bz2|7z|xz|sqlite|sqlite3|pyc|jar|mo|shx|shp|laz|sbx|sbn|nc|gpkg|bam|bai|wav|mp3|mid|ogg|au|m4a|mp4|avi|mov|mkv|webm|Rproj)$"

  # removes renv folder and specifically
  # makes sure to only scope exactly for "renv/" only so renv2/ 2renv/ renv.R gets picked up
  exclude_pattern <- c(exclude_pattern, "\\brenv\\b")
  exclude_pattern <- paste(exclude_pattern, collapse = "|")
  return(exclude_pattern)
}

#' List Files and Directories
#'
#' This function lists files and directories in a specified path,
#' filtering out those that match a given pattern. It ensures that
#' only non-empty directories are included in the list.
#'
#' @param path A character string specifying the path to list files and directories from.
#' @param pattern A character string containing the pattern to filter out files and directories.
#' @param all.files A logical value indicating whether to list all files, including hidden files.
#'
#' @return A list containing two elements:
#' @importFrom fs dir_ls dir_exists is_file is_dir
#' @noRd
list_files_and_dirs <- function(path, pattern, all.files) {
  .le$debug(
    as.character(glue::glue("Listing files and directories for path: {path}"))
  )

  # changed so pattern is only filtered out after retrieving all non filtered out values
  included_files <- fs::dir_ls(
    path = path,
    all = all.files,
    regexp = NULL,
    recurse = F,
    ignore.case = TRUE,
    invert = TRUE
  )

  if (!is.null(pattern)) {
    included_files <- included_files[!grepl(pattern, included_files)]
  }

  .le$trace(
    as.character(glue::glue(
      "Included files: {paste(included_files, collapse = ', ')}"
    ))
  )

  non_empty_dirs <- sapply(included_files, function(x) {
    if (fs::dir_exists(x)) {
      length(fs::dir_ls(x)) > 0
    } else {
      TRUE
    }
  })

  # remove dirs w/o ANY files as otherwise will be unclickable dir
  if (any(!non_empty_dirs)) {
    included_files <- included_files[non_empty_dirs]
  }

  .le$trace(as.character(glue::glue(
    "Filtered included files: {paste(included_files, collapse = ', ')}"
  )))

  # if included_files returns an empty list because all files were filtered out, dir_ls is rerun
  # w/ recurse to expose those files to show user as to why dir is not able to be indexed into
  # didn't reuse lfs because wanted only files rather than both files and dirs + recurse
  if (length(included_files) == 0) {
    list_all <- fs::dir_ls(
      path = path,
      all = TRUE,
      regexp = NULL,
      recurse = T,
      ignore.case = TRUE,
      type = "file"
    )
    .le$debug(as.character(glue::glue(
      "All files (when included_files is empty): {paste(list_all, collapse = ', ')}"
    )))
    return(list(files = list_all, empty = TRUE))
  }

  files <- sort(included_files[fs::is_file(included_files)])
  dirs <- sort(included_files[fs::is_dir(included_files)])

  files_and_dirs <- c(dirs, files)

  return(list(files = files_and_dirs, empty = FALSE))
}


#' @importFrom jsTreeR jstreeOutput
treeNavigatorUI <- function(id, width = "100%", height = "auto") {
  tree <- jstreeOutput(outputId = id, width = width, height = height)
  tagList(
    tree,
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "ghqcr/css/tree.css"
    ),
    tags$script(type = "module", src = "ghqcr/js/highlight_files.js"),
    tags$script(type = "module", src = "ghqcr/js/tree.js")
  )
}


#' @importFrom jsTreeR renderJstree jstree
treeNavigatorServer <- function(
  id,
  rootFolder,
  search = TRUE,
  wholerow = FALSE,
  contextMenu = FALSE,
  theme = "proton",
  pattern = NULL,
  all.files = FALSE,
  preselected_files = c(),
  ...
) {
  theme <- match.arg(theme, c("default", "proton"))
  moduleServer(id, function(input, output, session) {
    .le$debug(as.character(glue::glue(
      "Initializing treeNavigatorServer module with id: {id}"
    )))

    output[["treeNavigator"]] <- renderJstree({
      req(...)
      req(rootFolder())
      .le$debug(as.character(glue::glue(
        "Rendering jstree for rootFolder: {rootFolder()}"
      )))

      suppressMessages(jstree(
        nodes = list(
          list(
            text = basename(rootFolder()),
            type = "folder",
            #state = list(opened = TRUE),
            children = FALSE,
            li_attr = list(
              class = "jstree-x"
            )
          )
        ),
        types = list(
          folder = list(
            icon = "fa fa-folder"
          ),
          file = list(
            icon = "far fa-file"
          )
        ),
        checkCallback = TRUE,
        theme = theme,
        checkboxes = TRUE,
        search = search,
        wholerow = wholerow,
        contextMenu = contextMenu,
        selectLeavesOnly = TRUE
      ))
    })

    # observe({
    #   session$sendCustomMessage(type = "expandRoot", list())
    # })

    # example: given input "testTree/inst/www", full_path will be "/path/to/proj/testTree/inst/www"
    observeEvent(input[["path_from_js"]], {
      input <- input[["path_from_js"]]
      .le$debug(as.character(glue::glue(
        "Received path_from_js input: {paste(input, collapse = ', ')}"
      )))

      # null is sent back to reset the input if user wants to reselect unviable dirs
      if (is.null(input)) {
        .le$debug("Input is NULL, resetting selection")
        return()
      }
      # changed text of rootFolder to give back basename so need to
      # reconstruct original/full pathing of files to allow js to incrementally load in files
      dirname <- dirname(rootFolder())
      full_path <- fs::path(dirname, input)
      .le$debug(as.character(glue::glue("Full path constructed: {full_path}")))

      lf <- list_files_and_dirs(
        full_path,
        pattern = pattern,
        all.files = all.files
      )
      .le$trace(as.character(glue::glue(
        "List files and dirs result: {paste(lf$files, collapse = ', ')}"
      )))

      # if no viable children found, send msg to revert state and open modal
      # otherwise tree state will have miscalculated state and think node exists when it does not
      if (lf$empty) {
        message_content <- generate_excluded_file_message(lf$files)
        session$sendCustomMessage("noChildrenFound", lf$empty)
        showModal(modalDialog(
          title = tags$div(
            modalButton("Dismiss"),
            style = "text-align: right;"
          ),
          footer = NULL,
          easyClose = TRUE,
          HTML(message_content)
        ))
        .le$debug("Modal shown due to no viable children found")
        return()
      }

      fi <- file.info(lf$files, extra_cols = FALSE)
      x <- list(
        "elem" = as.list(basename(lf$files)),
        "folder" = as.list(fi[["isdir"]])
      )

      session$sendCustomMessage("getChildren", x)
    })

    # example: given input "testTree/inst/www", Paths is "inst/www"
    Paths <- reactiveVal()
    observeEvent(input[["treeNavigator_selected_paths"]], {
      selected <- input[["treeNavigator_selected_paths"]]
      .le$debug(
        as.character(glue::glue(
          "Selected paths: {paste(selected, collapse = ', ')}"
        ))
      )

      adjusted_paths <- sapply(selected, function(item) {
        fs::path_rel(item[["path"]], start = basename(rootFolder()))
      })
      Paths(adjusted_paths)
    })

    Paths
  })
}
