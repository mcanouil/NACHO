#' @include mod_data.R mod_thresholds.R mod_qc_plot.R mod_overview.R mod_outliers.R
NULL

#' Run the NACHO app
#'
#' Opens the quality-control app on a `nacho` object, or on an empty upload
#' page.
#' Use [visualise()] to get the tuned object back in the R session.
#'
#' @param x A `nacho` object from [load_rcc()] or [normalise()], or `NULL` to
#'   start by uploading RCC files.
#' @param done If `TRUE`, the app shows a "Done" button that closes it and
#'   returns the tuned object.
#'   Keep `FALSE` for a deployed app, because closing it would stop the
#'   server for every user.
#'
#' @return A Shiny app object; print it or pass it to [shiny::runApp()].
#' @export
#'
#' @examples
#' if (interactive()) {
#'   shiny::runApp(nacho_app(GSE74821))
#' }
nacho_app <- function(x = NULL, done = FALSE) {
  if (!is.null(x)) {
    check_nacho(x)
  }
  shiny::addResourcePath("nacho-brand", brand_path())
  shiny::shinyApp(
    ui = function(request) app_ui(done),
    server = app_server(x, done)
  )
}

help_page <- function(name) {
  shiny::markdown(help_text(name))
}

plot_page <- function(page) {
  bslib::layout_columns(
    col_widths = 6,
    !!!lapply(app_plot_types[[page]], mod_qc_plot_ui)
  )
}

app_ui <- function(done = FALSE) {
  bslib::page_navbar(
    title = shiny::tags$span(
      shiny::tags$img(src = "nacho-brand/nacho_hex.png", height = 24, alt = ""),
      "NACHO"
    ),
    window_title = "NACHO",
    id = "page",
    theme = nacho_theme(),
    sidebar = bslib::sidebar(
      title = "Thresholds",
      width = 320,
      mod_thresholds_ui("thresholds"),
      if (done) shiny::actionButton("done", "Done", class = "btn-primary")
    ),
    bslib::nav_panel("Data", mod_data_ui("data"), mod_overview_ui("overview")),
    bslib::nav_panel("QC metrics", plot_page("qc_metrics")),
    bslib::nav_panel("Controls", plot_page("controls")),
    bslib::nav_panel("Counts", plot_page("counts")),
    bslib::nav_panel("Normalisation", plot_page("normalisation")),
    bslib::nav_panel("Batch", plot_page("batch")),
    bslib::nav_panel("Flagged samples", mod_outliers_ui("outliers")),
    bslib::nav_panel("About", help_page("nacho"))
  )
}

tune_object <- function(object, chosen, thresholds) {
  tryCatch(
    rlang::with_options(
      normalise(
        object,
        normalisation_method = chosen$normalisation_method,
        ruv_k = chosen$ruv_k,
        background = chosen$background,
        background_mode = chosen$background_mode,
        outliers_thresholds = thresholds
      ),
      nacho.quiet = TRUE
    ),
    nacho_error = function(cnd) {
      shiny::validate(
        shiny::need(FALSE, cli::ansi_strip(rlang::cnd_message(cnd)))
      )
    }
  )
}

app_server <- function(x, done = FALSE) {
  function(input, output, session) {
    data <- mod_data_server("data", initial = x)
    settings <- mod_thresholds_server("thresholds", data = data)
    dark <- shiny::reactive(identical(input$dark_mode, "dark"))
    tuned <- shiny::reactive(
      tune_object(
        shiny::req(data()),
        settings$settings(),
        settings$thresholds()
      )
    )
    qc <- shiny::reactive(nacho_qc(tuned()))
    mod_overview_server("overview", tuned, qc)
    mod_outliers_server("outliers", tuned, qc)
    lapply(unlist(app_plot_types, use.names = FALSE), function(type) {
      mod_qc_plot_server(type, object = tuned, type = type, dark = dark)
    })
    if (done) {
      finished <- new.env()
      observe_done(input, data, settings, finished)
      session$onSessionEnded(function() {
        if (!isTRUE(finished$done)) shiny::stopApp(NULL)
      })
    }
  }
}

observe_done <- function(input, data, settings, finished = new.env()) {
  shiny::observeEvent(input$done, {
    if (is.null(data())) {
      finished$done <- TRUE
      return(shiny::stopApp(NULL))
    }
    problem <- NULL
    result <- tryCatch(
      tune_object(
        data(),
        settings$settings(),
        settings$current_thresholds()
      ),
      shiny.silent.error = function(cnd) {
        problem <<- conditionMessage(cnd)
        NULL
      }
    )
    if (is.null(result)) {
      notify_user(
        paste(
          problem,
          "Choose a normalisation method these data support before clicking Done."
        ),
        "warning"
      )
    } else {
      finished$done <- TRUE
      shiny::stopApp(result)
    }
  })
}
