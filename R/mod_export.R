#' @include render.R
NULL

#' Render the report, in a mirai daemon when mirai is installed
#'
#' The background render uses the installed NACHO package, not the loaded
#' one, so reinstall NACHO after you change `render()`.
#'
#' @param compute The mirai compute profile that runs the render.
#'
#' @noRd
render_in_background <- function(
  object,
  format,
  output_dir,
  compute = NULL,
  title = NULL,
  author = NULL
) {
  if (has_package("mirai")) {
    libs <- .libPaths()
    return(mirai::mirai(
      {
        .libPaths(libs)
        NACHO::render(
          object,
          format = format,
          output_dir = output_dir,
          title = title,
          author = author
        )
      },
      object = object,
      format = format,
      output_dir = output_dir,
      title = title,
      author = author,
      libs = libs,
      .compute = compute
    ))
  }
  render(
    object,
    format = format,
    output_dir = output_dir,
    title = title,
    author = author
  )
}

#' Turn blank text input into NULL, so render() leaves the field out
#'
#' @noRd
blank_to_null <- function(x) {
  x <- trimws(x %||% "")
  if (nzchar(x)) x else NULL
}

#' Hand a background render to an ExtendedTask
#'
#' After the session ends, the promise never settles, so the task does not
#' write to the reactive values of a session that no longer exists.
#'
#' @param running A mirai, or the result of a render that is already done.
#' @param ended A function that returns `TRUE` after the session ends.
#'
#' @noRd
settle_while_open <- function(running, ended) {
  if (!inherits(running, "mirai")) {
    return(running)
  }
  promises::promise(function(resolve, reject) {
    promises::then(
      running,
      onFulfilled = function(value) if (!ended()) resolve(value),
      onRejected = function(error) if (!ended()) reject(error)
    )
  })
}

spreadsheet_safe <- function(data) {
  data[] <- lapply(data, function(column) {
    if (is.character(column) || is.factor(column)) {
      column <- as.character(column)
      risky <- grepl("^[-=+@\t\r]", column)
      column[risky %in% TRUE] <- paste0("'", column[risky %in% TRUE])
    }
    column
  })
  data
}

#' Write R code that reproduces an object tuned in the app
#'
#' The script loads the data with the settings of the object, or uses the
#' example data when the object is the example data, then normalises it with
#' the method, background and thresholds chosen in the app.
#' The thresholds are written into the script and read back with
#' [read_thresholds()], so the script is one file.
#'
#' @param object The `nacho` object of the app.
#'
#' @return The lines of the script.
#'
#' @noRd
reproduce_script <- function(object) {
  settings <- object@settings
  thresholds <- object@thresholds
  value <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = " ")
  argument <- function(name, x, last = FALSE) {
    paste0("  ", name, " = ", x, if (!last) ",")
  }
  load <- if (identical(data_fingerprint(object), example_fingerprint())) {
    "nacho <- GSE74821"
  } else {
    directory <- object@provenance[["data_directory"]]
    uploaded <- !rlang::is_string(directory) ||
      startsWith(directory, tempdir()) ||
      startsWith(
        normalizePath(directory, mustWork = FALSE),
        normalizePath(tempdir(), mustWork = FALSE)
      )
    c(
      "# This loads every RCC file of the folder, so samples left out before",
      "# the app are back.",
      if (uploaded) {
        c(
          "# Point these to your RCC files and sample sheet. The sample sheet",
          "# is a CSV file with an IDFILE column of RCC file names, and a",
          "# plexset_id column (S1 to S8) for PlexSet files."
        )
      } else {
        "# Point this to your sample sheet."
      },
      "nacho <- load_rcc(",
      argument(
        "data_directory",
        if (uploaded) "\"path/to/rcc-files\"" else value(directory)
      ),
      argument("ssheet_csv", "\"path/to/samplesheet.csv\""),
      argument("id_colname", value(settings[["id_colname"]])),
      argument("housekeeping_genes", value(settings[["housekeeping_genes"]])),
      argument(
        "housekeeping_predict",
        value(settings[["housekeeping_predict"]])
      ),
      argument("housekeeping_norm", value(settings[["housekeeping_norm"]])),
      argument(
        "instrument",
        if (is.na(thresholds[["instrument"]])) {
          "NULL"
        } else {
          value(thresholds[["instrument"]])
        }
      ),
      argument("preset", value(thresholds[["preset"]])),
      argument("n_comp", value(settings[["n_comp"]]), last = TRUE),
      ")"
    )
  }
  yaml_lines <- encodeString(
    strsplit(thresholds_yaml(thresholds), "\n", fixed = TRUE)[[1]],
    quote = "\""
  )
  c(
    paste0(
      "# Reproduces a NACHO app session (NACHO ",
      utils::packageVersion("NACHO"),
      ", ",
      format(Sys.Date()),
      ")."
    ),
    "library(NACHO)",
    "",
    load,
    "",
    "# The thresholds set in the app.",
    "thresholds_file <- tempfile(fileext = \".yml\")",
    "writeLines(",
    "  c(",
    paste0("    ", yaml_lines, c(rep(",", length(yaml_lines) - 1), "")),
    "  ),",
    "  thresholds_file",
    ")",
    "",
    "nacho <- normalise(",
    "  nacho,",
    argument(
      "normalisation_method",
      value(settings[["normalisation_method"]])
    ),
    argument("background", value(settings[["background"]])),
    argument("background_mode", value(settings[["background_mode"]])),
    argument("ruv_k", value(settings[["ruv_k"]])),
    argument(
      "outliers_thresholds",
      "read_thresholds(thresholds_file)",
      last = TRUE
    ),
    ")"
  )
}

#' Tell loaded data apart by their load time and samples
#'
#' `normalise()` adds to the provenance, so only the load time is compared.
#'
#' @noRd
data_fingerprint <- function(object) {
  list(object@provenance[["created"]], colnames(object@counts))
}

example_cache <- new.env(parent = emptyenv())

#' The fingerprint of the example data, worked out once
#'
#' @noRd
example_fingerprint <- function() {
  if (is.null(example_cache$fingerprint)) {
    example_cache$fingerprint <- data_fingerprint(example_data())
  }
  example_cache$fingerprint
}

mod_export_ui <- function(id, quarto) {
  ns <- shiny::NS(id)
  reproduce <- bslib::card(
    bslib::card_header("Reproduce in R"),
    shiny::helpText(
      "This R script repeats the analysis of the app, with the same",
      "normalisation and thresholds."
    ),
    shiny::downloadButton(ns("script_r"), "R script"),
    shiny::div(
      class = "overflow-auto",
      style = "max-height: 400px;",
      shiny::verbatimTextOutput(ns("script"))
    )
  )
  cards <- bslib::layout_column_wrap(
    id = ns("cards"),
    width = "320px",
    bslib::card(
      bslib::card_header("Data"),
      shiny::downloadButton(ns("qc_csv"), "Quality-control table (CSV)"),
      shiny::downloadButton(ns("thresholds_yaml"), "Thresholds (YAML)"),
      shiny::downloadButton(ns("object_rds"), "nacho object (RDS)"),
      shiny::helpText("Read the object back in R with read_nacho().")
    ),
    bslib::card(
      bslib::card_header("Report"),
      if (quarto) {
        shiny::tagList(
          shiny::textInput(
            ns("title"),
            "Title (optional)",
            placeholder = "NanoString quality-control report"
          ),
          shiny::textInput(ns("author"), "Author (optional)"),
          shiny::radioButtons(
            ns("format"),
            "Format",
            c(HTML = "html", "PDF (Typst)" = "typst"),
            inline = TRUE
          ),
          if (!has_package("mirai")) {
            shiny::helpText(
              "Rendering blocks the app until it finishes;",
              "install mirai to render in the background."
            )
          },
          bslib::input_task_button(ns("render"), "Render the report"),
          shiny::uiOutput(ns("report_ready"))
        )
      } else {
        shiny::helpText(
          "This app cannot render the report, because the Quarto",
          "command-line interface 1.9.18 or newer and the quarto R package",
          "are not installed where the app runs."
        )
      }
    )
  )
  shiny::tagList(cards, reproduce)
}

mod_export_server <- function(id, object, quarto) {
  shiny::moduleServer(id, function(input, output, session) {
    output$qc_csv <- shiny::downloadHandler(
      filename = "nacho-qc.csv",
      content = function(file) {
        utils::write.csv(
          spreadsheet_safe(nacho_qc(object())),
          file,
          row.names = FALSE
        )
      }
    )
    output$thresholds_yaml <- shiny::downloadHandler(
      filename = "nacho-thresholds.yml",
      content = function(file) write_thresholds(object()@thresholds, file)
    )
    script <- shiny::reactive(reproduce_script(object()))
    output$script <- shiny::renderText(paste(script(), collapse = "\n"))
    output$script_r <- shiny::downloadHandler(
      filename = "nacho-reproduce.R",
      content = function(file) writeLines(script(), file)
    )
    output$object_rds <- shiny::downloadHandler(
      filename = "nacho.rds",
      content = function(file) saveRDS(object(), file)
    )
    if (!quarto) {
      return(invisible(NULL))
    }
    report_dir <- tempfile("nacho-report-")
    profile <- basename(report_dir)
    report_path <- shiny::reactiveVal(NULL)
    requested <- NULL
    running <- NULL
    daemons_on <- FALSE
    ended <- FALSE
    session$onSessionEnded(function() {
      ended <<- TRUE
      if (inherits(running, "mirai")) {
        mirai::stop_mirai(running)
      }
      if (daemons_on) {
        mirai::daemons(0, .compute = profile)
      }
      unlink(report_dir, recursive = TRUE)
    })
    start_render <- function(object, format, output_dir, title, author) {
      if (has_package("mirai") && !daemons_on) {
        mirai::daemons(1, dispatcher = TRUE, .compute = profile)
        daemons_on <<- TRUE
      }
      running <<- render_in_background(
        object,
        format,
        output_dir,
        profile,
        title,
        author
      )
      settle_while_open(running, function() ended)
    }
    task <- shiny::ExtendedTask$new(start_render) |>
      bslib::bind_task_button("render")
    drop_report <- function() {
      unlink(report_path())
      report_path(NULL)
    }
    shiny::observeEvent(input$render, {
      drop_report()
      requested <<- object()
      task$invoke(
        requested,
        input$format,
        report_dir,
        blank_to_null(input$title),
        blank_to_null(input$author)
      )
    })
    shiny::observeEvent(object(), drop_report(), ignoreInit = TRUE)
    shiny::observeEvent(task$status(), {
      if (ended) {
        return(invisible(NULL))
      }
      if (task$status() == "success") {
        if (identical(object(), requested)) {
          report_path(task$result())
        } else {
          unlink(task$result())
        }
      } else if (task$status() == "error") {
        reason <- tryCatch(
          task$result(),
          error = function(cnd) cli::ansi_strip(rlang::cnd_message(cnd))
        )
        notify_user(reason, "error")
      }
    })
    output$report_ready <- shiny::renderUI({
      shiny::req(report_path())
      shiny::downloadButton(
        session$ns("report"),
        paste("Download", basename(report_path()))
      )
    })
    output$report <- shiny::downloadHandler(
      filename = function() basename(shiny::req(report_path())),
      content = function(file) file.copy(shiny::req(report_path()), file)
    )
  })
}
