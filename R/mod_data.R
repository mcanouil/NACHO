#' @include load_rcc.R
NULL

#' Load the files of a Shiny upload
#'
#' @param files The data frame of [shiny::fileInput()]: `name`, `size`,
#'   `type` and `datapath`.
#'
#' @return A `nacho` object.
#'
#' @noRd
read_uploads <- function(files) {
  targets <- do.call(
    rbind,
    lapply(seq_len(nrow(files)), function(i) {
      expand_upload(
        files[["name"]][i],
        files[["datapath"]][i],
        files[["type"]][i]
      )
    })
  )
  is_sheet <- grepl("\\.csv$", targets[["name"]], ignore.case = TRUE)
  sheets <- targets[is_sheet, , drop = FALSE]
  targets <- targets[
    grepl("\\.rcc(\\.gz)?$", targets[["name"]], ignore.case = TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(targets) == 0) {
    nacho_abort(
      c(
        "The upload holds no RCC file.",
        i = "Upload files ending in {.file .RCC} or {.file .RCC.gz}, or a zip archive of them."
      ),
      class = "bad_upload"
    )
  }
  plexset <- all(vapply(targets[["datapath"]], is_plexset_rcc, logical(1)))
  if (plexset) {
    targets <- merge(
      targets,
      expand.grid(
        IDFILE = targets[["IDFILE"]],
        plexset_id = paste0("S", seq_len(8)),
        stringsAsFactors = FALSE
      ),
      by = "IDFILE"
    )
  }
  if (nrow(sheets) > 1) {
    nacho_warn(
      "Only the first sample sheet is used, so {.file {sheets[['name']][-1]}} {?was/were} ignored.",
      class = "sample_sheet_discarded"
    )
  }
  if (nrow(sheets) > 0) {
    sheet <- data.table::fread(sheets[["datapath"]][1], data.table = FALSE)
    merge_by <- if (plexset) c("IDFILE", "plexset_id") else "IDFILE"
    missing_columns <- setdiff(merge_by, names(sheet))
    if (length(missing_columns) > 0) {
      nacho_warn(
        "The sample sheet was discarded, because it has no {.val {missing_columns}} column{?s}.",
        class = "sample_sheet_discarded"
      )
    } else {
      targets <- merge(targets, sheet, by = merge_by)
    }
  }
  load_rcc(
    data_directory = unique(mapply(
      function(id, path) sub(id, "", path, fixed = TRUE),
      targets[["IDFILE"]],
      targets[["datapath"]]
    )),
    ssheet_csv = targets,
    id_colname = "IDFILE"
  )
}

#' List the files of one upload
#'
#' A zip archive is extracted next to the upload.
#' Any other file is renamed to its original name.
#'
#' @param name,datapath,type One row of the [shiny::fileInput()] table.
#'
#' @return A data frame with `name`, `datapath`, `type` and `IDFILE`.
#'
#' @noRd
expand_upload <- function(name, datapath, type) {
  if (is_zip_upload(name, type)) {
    extract_directory <- file.path(
      dirname(datapath),
      sub("\\.zip$", "", name, ignore.case = TRUE)
    )
    utils::unzip(datapath, exdir = extract_directory)
    files <- list.files(extract_directory, recursive = TRUE)
    extracted <- file.path(basename(extract_directory), files)
    return(data.frame(
      name = extracted,
      datapath = file.path(extract_directory, files),
      type = type,
      IDFILE = extracted
    ))
  }
  renamed <- file.path(dirname(datapath), name)
  file.rename(from = datapath, to = renamed)
  data.frame(name = name, datapath = renamed, type = type, IDFILE = name)
}

#' Tell whether an upload is a zip archive
#'
#' @noRd
is_zip_upload <- function(name, type) {
  type %in%
    c("application/zip", "application/x-zip-compressed") ||
    grepl("\\.zip$", name, ignore.case = TRUE)
}

#' The example data of the app
#'
#' @noRd
example_data <- function() {
  env <- new.env()
  utils::data("GSE74821", package = "NACHO", envir = env)
  env[["GSE74821"]]
}

#' Tell the app user something
#'
#' @noRd
notify_user <- function(message, type = c("message", "warning", "error")) {
  type <- match.arg(type)
  shiny::showNotification(
    message,
    type = type,
    duration = if (type == "error") NULL else 8
  )
}

#' The upload panel of the app
#'
#' @noRd
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Load RCC files"),
    shiny::fileInput(
      ns("files"),
      "RCC files, zip archives and an optional sample sheet",
      multiple = TRUE,
      accept = c(".RCC", ".rcc", ".gz", ".zip", ".csv")
    ),
    shiny::helpText(
      "The sample sheet is a CSV file with an IDFILE column holding the RCC file names, ",
      "and plexset_id (S1 to S8) for PlexSet files."
    ),
    shiny::actionButton(ns("import"), "Import", class = "btn-primary")
  )
}

#' The server of the upload panel
#'
#' Returns a reactive value that holds the loaded object, or `NULL`.
#'
#' @noRd
mod_data_server <- function(id, initial = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    current <- shiny::reactiveVal(initial)
    shiny::observeEvent(input$import, {
      files <- shiny::req(input$files)
      loaded <- withCallingHandlers(
        tryCatch(
          rlang::with_options(read_uploads(files), nacho.quiet = TRUE),
          error = function(cnd) {
            message <- cli::ansi_strip(rlang::cnd_message(cnd))
            if (!inherits(cnd, "nacho_error")) {
              message <- paste("The upload could not be read:", message)
            }
            notify_user(message, "error")
            NULL
          }
        ),
        nacho_warning_sample_sheet_discarded = function(cnd) {
          notify_user(cli::ansi_strip(rlang::cnd_message(cnd)), "warning")
          invokeRestart("muffleWarning")
        }
      )
      if (!is.null(loaded)) {
        current(loaded)
      }
    })
    current
  })
}
