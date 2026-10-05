#' @include report.R
NULL

app_overview <- function(x, qc = nacho_qc(x)) {
  metrics <- intersect(qc_metrics, names(qc))
  reasons <- vapply(
    metrics,
    function(metric) sum(qc[[paste0(metric, "_status")]] %in% "fail"),
    integer(1)
  )
  reasons <- reasons[reasons > 0]
  if (length(reasons) == 0) {
    reasons <- integer()
  }
  list(
    samples = nrow(qc),
    cartridges = length(unique(stats::na.omit(qc[["CartridgeID"]]))),
    flagged = sum(qc[["status"]] %in% "fail"),
    method = x@settings[["normalisation_method"]],
    preset = x@thresholds[["preset"]],
    reasons = reasons
  )
}

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  icon <- function(name) shiny::icon(name, `aria-hidden` = "true")
  bslib::layout_column_wrap(
    width = "220px",
    fill = FALSE,
    bslib::value_box(
      "Samples",
      shiny::textOutput(ns("samples")),
      showcase = icon("vial")
    ),
    bslib::value_box(
      "Cartridges",
      shiny::textOutput(ns("cartridges")),
      showcase = icon("layer-group")
    ),
    bslib::value_box(
      "Flagged samples",
      shiny::textOutput(ns("flagged_count")),
      shiny::textOutput(ns("reasons")),
      showcase = icon("triangle-exclamation")
    ),
    bslib::value_box(
      "Method",
      shiny::textOutput(ns("method")),
      shiny::textOutput(ns("preset")),
      showcase = icon("scale-balanced")
    )
  )
}

mod_overview_server <- function(id, object, qc) {
  shiny::moduleServer(id, function(input, output, session) {
    overview <- shiny::reactive(
      app_overview(shiny::req(object()), shiny::req(qc()))
    )
    output$samples <- shiny::renderText(overview()$samples)
    output$cartridges <- shiny::renderText(overview()$cartridges)
    output$flagged_count <- shiny::renderText(overview()$flagged)
    output$reasons <- shiny::renderText({
      reasons <- overview()$reasons
      if (length(reasons) == 0) {
        "None"
      } else {
        paste(names(reasons), reasons, sep = ": ", collapse = ", ")
      }
    })
    output$method <- shiny::renderText(overview()$method)
    output$preset <- shiny::renderText(paste(
      preset_label(overview()$preset),
      "preset"
    ))
    overview
  })
}
