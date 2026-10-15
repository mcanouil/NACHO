#' @include render.R daemon.R
NULL

#' Turn blank text input into NULL, so render() leaves the field out
#'
#' @noRd
blank_to_null <- function(x) {
  x <- trimws(x %||% "")
  if (nzchar(x)) x else NULL
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

mod_export_ui <- function(id, quarto) {
  ns <- shiny::NS(id)
  bslib::layout_column_wrap(
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
    output$object_rds <- shiny::downloadHandler(
      filename = "nacho.rds",
      content = function(file) saveRDS(object(), file)
    )
    if (!quarto) {
      return(invisible(NULL))
    }
    report_dir <- tempfile("nacho-report-")
    report_path <- shiny::reactiveVal(NULL)
    requested <- NULL
    task <- session_task(
      session,
      "render",
      "render",
      on_success = function(path) {
        if (identical(object(), requested)) {
          report_path(path)
        } else {
          unlink(path)
        }
      },
      on_error = function(message) notify_user(message, "error"),
      on_end = function() unlink(report_dir, recursive = TRUE)
    )
    drop_report <- function() {
      unlink(report_path())
      report_path(NULL)
    }
    shiny::observeEvent(input$render, {
      drop_report()
      requested <<- object()
      task$invoke(
        requested,
        format = input$format,
        output_dir = report_dir,
        title = blank_to_null(input$title),
        author = blank_to_null(input$author)
      )
    })
    shiny::observeEvent(object(), drop_report(), ignoreInit = TRUE)
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
