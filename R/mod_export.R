#' @include render.R
NULL

render_in_background <- function(object, format, output_dir) {
  if (has_package("mirai")) {
    libs <- .libPaths()
    return(mirai::mirai(
      {
        .libPaths(libs)
        NACHO::render(object, format = format, output_dir = output_dir)
      },
      object = object,
      format = format,
      output_dir = output_dir,
      libs = libs
    ))
  }
  render(object, format = format, output_dir = output_dir)
}

mod_export_ui <- function(id, quarto) {
  ns <- shiny::NS(id)
  bslib::layout_column_wrap(
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
          "To render the report from the app, install the Quarto",
          "command-line interface 1.9 or newer and the quarto R package,",
          "then restart the app."
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
        utils::write.csv(nacho_qc(object()), file, row.names = FALSE)
      }
    )
    output$thresholds_yaml <- shiny::downloadHandler(
      filename = "nacho-thresholds.yml",
      content = function(file) yaml::write_yaml(object()@thresholds, file)
    )
    output$object_rds <- shiny::downloadHandler(
      filename = "nacho.rds",
      content = function(file) saveRDS(object(), file)
    )
    if (!quarto) {
      return(invisible(NULL))
    }
    report_dir <- tempfile("nacho-report-")
    session$onSessionEnded(function() unlink(report_dir, recursive = TRUE))
    report_path <- shiny::reactiveVal(NULL)
    requested <- NULL
    task <- shiny::ExtendedTask$new(render_in_background) |>
      bslib::bind_task_button("render")
    drop_report <- function() {
      unlink(report_path())
      report_path(NULL)
    }
    shiny::observeEvent(input$render, {
      drop_report()
      requested <<- object()
      task$invoke(requested, input$format, report_dir)
    })
    shiny::observeEvent(object(), drop_report(), ignoreInit = TRUE)
    shiny::observeEvent(task$status(), {
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
