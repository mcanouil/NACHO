#' @include load_rcc.R daemon.R
NULL

#' List the files of a Shiny upload
#'
#' Zip archives are extracted and the other files get their original names
#' back, so the function runs once for each upload.
#'
#' @param files The data frame of [shiny::fileInput()]: `name`, `size`,
#'   `type` and `datapath`.
#'
#' @return A list with `rcc`, a data frame of the RCC files with `name`,
#'   `datapath`, `IDFILE`, `from`, the zip archive of the file or `""`, and
#'   `plexset`; `sheet`, the first sample sheet or `NULL`; and
#'   `ignored_sheets`, the names of the other sample sheets.
#'
#' @noRd
list_upload <- function(files) {
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
  rcc <- targets[
    grepl("\\.rcc(\\.gz)?$", targets[["name"]], ignore.case = TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(rcc) == 0) {
    nacho_abort(
      c(
        "The upload holds no RCC file.",
        i = "Upload files ending in {.file .RCC} or {.file .RCC.gz}, or a zip archive of them."
      ),
      class = "bad_upload"
    )
  }
  rcc[["plexset"]] <- unname(vapply(
    rcc[["datapath"]],
    is_plexset_rcc,
    logical(1)
  ))
  rownames(rcc) <- NULL
  list(
    rcc = rcc,
    sheet = if (nrow(sheets) > 0) {
      data.table::fread(sheets[["datapath"]][1], data.table = FALSE)
    },
    ignored_sheets = sheets[["name"]][-1]
  )
}

#' The columns that join the sample sheet to the RCC files
#'
#' @noRd
sheet_keys <- function(plexset) {
  if (plexset) c("IDFILE", "plexset_id") else "IDFILE"
}

#' Load the RCC files of an upload
#'
#' @param listing The output of `list_upload()`.
#'
#' @return A `nacho` object.
#'
#' @noRd
load_upload <- function(listing) {
  targets <- listing[["rcc"]]
  plexset <- all(targets[["plexset"]])
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
  ignored <- listing[["ignored_sheets"]]
  if (length(ignored) > 0) {
    nacho_warn(
      "Only the first sample sheet is used, so {.file {ignored}} {?was/were} ignored.",
      class = "sample_sheet_discarded"
    )
  }
  sheet <- listing[["sheet"]]
  sheet_columns <- character()
  if (!is.null(sheet)) {
    merge_by <- sheet_keys(plexset)
    missing_columns <- setdiff(merge_by, names(sheet))
    if (length(missing_columns) > 0) {
      nacho_warn(
        "The sample sheet was discarded, because it has no {.val {missing_columns}} column{?s}.",
        class = "sample_sheet_discarded"
      )
    } else {
      matched <- merge_sample_sheet(targets, sheet, merge_by)
      if (nrow(matched) == 0) {
        nacho_warn(
          "The sample sheet was discarded, because it matches no uploaded RCC file.",
          class = "sample_sheet_discarded"
        )
      } else {
        targets <- matched
        sheet_columns <- names(sheet)
      }
    }
  }
  sample_columns <- c(
    "IDFILE",
    if (plexset) "plexset_id",
    setdiff(sheet_columns, c("IDFILE", "plexset_id"))
  )
  load_rcc(
    data_directory = unique(mapply(
      function(id, path) sub(id, "", path, fixed = TRUE),
      targets[["IDFILE"]],
      targets[["datapath"]]
    )),
    ssheet_csv = targets[, sample_columns, drop = FALSE],
    id_colname = "IDFILE"
  )
}

#' Describe an upload before it is imported
#'
#' Sample sheet rows match a file on its bare name, as in
#' `merge_sample_sheet()`.
#'
#' @param listing The output of `list_upload()`.
#'
#' @return A list with `summary`, one line of text, and `table`, one row for
#'   each RCC file.
#'
#' @noRd
upload_preview <- function(listing) {
  rcc <- listing[["rcc"]]
  plexset <- all(rcc[["plexset"]])
  files <- basename(rcc[["IDFILE"]])
  table <- data.frame(
    File = basename(rcc[["name"]]),
    From = rcc[["from"]],
    PlexSet = ifelse(rcc[["plexset"]], "yes", "no"),
    check.names = FALSE
  )
  counts <- c(
    cli::format_inline("{nrow(rcc)} RCC file{?s}"),
    paste(sum(rcc[["plexset"]]), "PlexSet")
  )
  notes <- character()
  sheet <- listing[["sheet"]]
  missing_columns <- setdiff(sheet_keys(plexset), names(sheet))
  if (is.null(sheet)) {
    notes <- "No sample sheet."
  } else if (length(missing_columns) > 0) {
    notes <- cli::format_inline(
      "The sample sheet has no {.val {missing_columns}} column{?s}, so it is not used."
    )
  } else {
    sheet_files <- basename(sheet[["IDFILE"]])
    if (plexset) {
      wells <- vapply(
        files,
        function(file) {
          found <- sheet[["plexset_id"]][sheet_files == file]
          length(intersect(found, paste0("S", seq_len(8))))
        },
        integer(1)
      )
      table[["In sample sheet"]] <- paste(wells, "of 8")
      absent <- sum(wells == 0)
    } else {
      found <- files %in% sheet_files
      table[["In sample sheet"]] <- ifelse(found, "yes", "no")
      absent <- sum(!found)
    }
    orphans <- sum(!sheet_files %in% files)
    counts <- c(
      counts,
      paste(absent, "not in the sample sheet"),
      if (orphans > 0) {
        cli::format_inline("{orphans} sample sheet row{?s} with no file")
      }
    )
  }
  ignored <- listing[["ignored_sheets"]]
  if (length(ignored) > 0) {
    notes <- c(
      notes,
      cli::format_inline(
        "Only the first sample sheet is used, so {ignored} {?is/are} ignored."
      )
    )
  }
  list(
    summary = cli::ansi_strip(
      paste(
        c(paste0(paste(counts, collapse = ", "), "."), notes),
        collapse = " "
      )
    ),
    table = table
  )
}

#' Load the files of a Shiny upload and keep what the user must see
#'
#' The app runs this function in a mirai daemon, where warnings and errors
#' do not reach the session, so they come back as text.
#'
#' @param listing The output of `list_upload()`.
#' @param progress_file The file that receives each stage of the import, or
#'   `NULL`.
#'
#' @return A list with `object`, the `nacho` object or `NULL`, `warnings`, the
#'   messages of the NACHO warnings, and `error`, the message of the error or
#'   `NULL`.
#'
#' @noRd
import_uploads <- function(listing, progress_file = NULL) {
  warnings <- character()
  error <- NULL
  object <- withCallingHandlers(
    tryCatch(
      rlang::with_options(
        load_upload(listing),
        nacho.quiet = TRUE,
        nacho.progress_file = progress_file
      ),
      error = function(cnd) {
        error <<- upload_error(cnd)
        NULL
      }
    ),
    nacho_warning_metric_unavailable = function(cnd) {
      invokeRestart("muffleWarning")
    },
    nacho_warning = function(cnd) {
      warnings <<- c(warnings, cli::ansi_strip(rlang::cnd_message(cnd)))
      invokeRestart("muffleWarning")
    }
  )
  list(object = object, warnings = warnings, error = error)
}

#' Give the message of an error raised by an upload
#'
#' @noRd
upload_error <- function(cnd) {
  message <- cli::ansi_strip(rlang::cnd_message(cnd))
  if (inherits(cnd, "nacho_error")) {
    return(message)
  }
  paste("The upload could not be read:", message)
}

#' Join a sample sheet to the uploaded files
#'
#' Files from a zip archive carry the archive name in `IDFILE`, so the sheet
#' is matched on the bare file name and the upload keeps its own `IDFILE`.
#'
#' @noRd
merge_sample_sheet <- function(targets, sheet, merge_by) {
  key <- function(data) {
    data[["IDFILE"]] <- basename(data[["IDFILE"]])
    data[, merge_by, drop = FALSE]
  }
  sheet_keys <- key(sheet)
  sheet_keys[["row"]] <- seq_len(nrow(sheet))
  target_keys <- key(targets)
  target_keys[["position"]] <- seq_len(nrow(targets))
  links <- merge(target_keys, sheet_keys, by = merge_by)
  links <- links[order(links[["position"]]), ]
  extra <- sheet[links[["row"]], setdiff(names(sheet), merge_by), drop = FALSE]
  cbind(targets[links[["position"]], , drop = FALSE], extra)
}

#' List the files of one upload
#'
#' A zip archive is extracted next to the upload.
#' Any other file is renamed to its original name.
#'
#' @param name,datapath,type One row of the [shiny::fileInput()] table.
#'
#' @return A data frame with `name`, `datapath`, `type`, `IDFILE` and `from`,
#'   the zip archive of the file or `""`.
#'
#' @noRd
expand_upload <- function(name, datapath, type) {
  name <- basename(name)
  if (is_zip_upload(name, type)) {
    extract_directory <- file.path(
      dirname(datapath),
      sub("\\.zip$", "", name, ignore.case = TRUE)
    )
    not_a_zip <- function(cnd) {
      nacho_abort(
        c(
          "{.file {name}} is not a valid zip archive.",
          i = "Upload the RCC files themselves, or zip them again."
        ),
        class = "bad_upload"
      )
    }
    entries <- tryCatch(
      utils::unzip(datapath, list = TRUE)[["Name"]],
      warning = not_a_zip,
      error = not_a_zip
    )
    check_zip_entries(entries, name)
    tryCatch(
      utils::unzip(datapath, exdir = extract_directory),
      warning = not_a_zip
    )
    files <- list.files(extract_directory, recursive = TRUE)
    if (length(files) == 0) {
      return(data.frame(
        name = character(),
        datapath = character(),
        type = character(),
        IDFILE = character(),
        from = character()
      ))
    }
    extracted <- file.path(basename(extract_directory), files)
    return(data.frame(
      name = extracted,
      datapath = file.path(extract_directory, files),
      type = type,
      IDFILE = extracted,
      from = name
    ))
  }
  renamed <- file.path(dirname(datapath), name)
  file.rename(from = datapath, to = renamed)
  data.frame(
    name = name,
    datapath = renamed,
    type = type,
    IDFILE = name,
    from = ""
  )
}

#' Refuse zip entries that would be extracted outside the upload folder
#'
#' The internal `utils::unzip()` method keeps `../` in entry names before
#' R 4.5.1, so absolute paths and `..` components are refused here.
#'
#' @noRd
check_zip_entries <- function(entries, name) {
  outside <- grepl("^([/\\\\]|[A-Za-z]:)", entries) |
    vapply(
      strsplit(entries, "[/\\\\]"),
      function(parts) any(parts == ".."),
      logical(1)
    )
  if (any(outside)) {
    nacho_abort(
      c(
        "{.file {name}} has files outside its own folder, so NACHO does not extract it.",
        x = "Refused: {.file {entries[outside]}}.",
        i = "Zip the RCC files again from their folder."
      ),
      class = "bad_upload"
    )
  }
  invisible(entries)
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
  bslib::show_toast(bslib::toast(
    message,
    header = c(message = "NACHO", warning = "Warning", error = "Error")[[type]],
    type = c(message = "info", warning = "warning", error = "danger")[[type]],
    duration_s = if (type == "error") 0 else 8
  ))
}

#' The upload panel of the app
#'
#' @noRd
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    id = ns("card"),
    bslib::card_header("Load RCC files"),
    shiny::fileInput(
      ns("files"),
      "RCC files, zip archives and an optional sample sheet",
      multiple = TRUE,
      accept = c(".RCC", ".rcc", ".gz", ".zip", ".csv")
    ),
    shiny::div(
      shiny::helpText(
        "The sample sheet is a CSV file with an IDFILE column holding the RCC file names, ",
        "and plexset_id (S1 to S8) for PlexSet files."
      ),
      shiny::div(class = "small mt-2", shiny::textOutput(ns("preview_summary"))),
      shiny::div(
        class = "overflow-auto small",
        style = "max-height: 300px;",
        shiny::tableOutput(ns("preview_table"))
      )
    ),
    if (!has_package("mirai")) {
      shiny::helpText(
        "Importing blocks the app until it finishes;",
        "install mirai to import in the background."
      )
    },
    shiny::div(
      bslib::input_task_button(ns("import"), "Import", class = "w-100"),
      shiny::div(class = "small text-muted", shiny::textOutput(ns("progress")))
    ),
    shiny::actionButton(
      ns("example"),
      "Load the example data (GSE74821)",
      class = "btn-outline-secondary mt-2"
    )
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
    progress_file <- tempfile("nacho-import-", fileext = ".txt")
    awaited <- FALSE
    task <- session_task(
      session,
      "import",
      "import_uploads",
      on_success = function(imported) {
        if (!awaited) {
          return(invisible(NULL))
        }
        awaited <<- FALSE
        for (message in imported[["warnings"]]) {
          notify_user(message, "warning")
        }
        if (is.null(imported[["error"]])) {
          current(imported[["object"]])
        } else {
          notify_user(imported[["error"]], "error")
        }
      },
      on_error = function(message) {
        notify_user(paste("The upload could not be read:", message), "error")
      },
      on_end = function() unlink(progress_file),
      warm_on = shiny::reactive(input$files)
    )
    listing <- shiny::reactive({
      if (is.null(input$files)) {
        return(NULL)
      }
      tryCatch(list_upload(input$files), error = function(cnd) cnd)
    })
    preview <- shiny::reactive({
      if (is.null(listing())) {
        return(list(summary = "", table = NULL))
      }
      if (inherits(listing(), "error")) {
        return(list(summary = upload_error(listing()), table = NULL))
      }
      upload_preview(listing())
    })
    output$preview_summary <- shiny::renderText(preview()[["summary"]])
    output$preview_table <- shiny::renderTable(preview()[["table"]])
    shiny::observeEvent(input$import, {
      if (is.null(input$files)) {
        notify_user("Choose the RCC files to import first.", "warning")
        return()
      }
      if (inherits(listing(), "error")) {
        notify_user(upload_error(listing()), "error")
        return()
      }
      unlink(progress_file)
      awaited <<- TRUE
      task$invoke(listing(), progress_file)
    })
    output$progress <- shiny::renderText({
      if (task$status() != "running") {
        return("")
      }
      shiny::invalidateLater(250)
      if (file.exists(progress_file)) {
        readLines(progress_file, warn = FALSE)
      }
    })
    shiny::observeEvent(input$example, {
      awaited <<- FALSE
      current(example_data())
    })
    current
  })
}
