#' @include render.R
NULL

render_in_background <- function(object, format, output_dir) {
  if (has_package("mirai")) {
    return(mirai::mirai(
      NACHO::render(object, format = format, output_dir = output_dir),
      object = object,
      format = format,
      output_dir = output_dir
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
    task <- shiny::ExtendedTask$new(render_in_background) |>
      bslib::bind_task_button("render")
    shiny::observeEvent(input$render, {
      task$invoke(object(), input$format, tempfile("nacho-report-"))
    })
    shiny::observeEvent(task$status(), {
      if (task$status() == "error") {
        message <- tryCatch(
          task$result(),
          error = function(cnd) cli::ansi_strip(rlang::cnd_message(cnd))
        )
        notify_user(message, "error")
      }
    })
    output$report_ready <- shiny::renderUI({
      shiny::req(task$status() == "success")
      shiny::downloadButton(
        session$ns("report"),
        paste("Download", basename(task$result()))
      )
    })
    output$report <- shiny::downloadHandler(
      filename = function() basename(task$result()),
      content = function(file) file.copy(task$result(), file)
    )
  })
}
