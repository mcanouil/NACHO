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
render_in_background <- function(object, format, output_dir, compute = NULL) {
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
      libs = libs,
      .compute = compute
    ))
  }
  render(object, format = format, output_dir = output_dir)
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
          "This app cannot render the report, because the Quarto",
          "command-line interface 1.9 or newer and the quarto R package",
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
    start_render <- function(object, format, output_dir) {
      if (has_package("mirai") && !daemons_on) {
        mirai::daemons(1, dispatcher = TRUE, .compute = profile)
        daemons_on <<- TRUE
      }
      running <<- render_in_background(object, format, output_dir, profile)
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
      task$invoke(requested, input$format, report_dir)
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
