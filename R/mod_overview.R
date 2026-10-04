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
    cartridges = length(unique(qc[["CartridgeID"]])),
    flagged = sum(qc[["status"]] %in% "fail"),
    method = x@settings[["normalisation_method"]],
    preset = x@thresholds[["preset"]],
    reasons = reasons
  )
}

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Overview"),
    shiny::tags$dl(
      shiny::tags$dt("Samples"),
      shiny::tags$dd(shiny::textOutput(ns("samples"), inline = TRUE)),
      shiny::tags$dt("Cartridges"),
      shiny::tags$dd(shiny::textOutput(ns("cartridges"), inline = TRUE)),
      shiny::tags$dt("Flagged samples"),
      shiny::tags$dd(shiny::textOutput(ns("flagged"), inline = TRUE)),
      shiny::tags$dt("Method"),
      shiny::tags$dd(shiny::textOutput(ns("method"), inline = TRUE))
    )
  )
}

mod_overview_server <- function(
  id,
  object,
  qc = shiny::reactive(nacho_qc(shiny::req(object())))
) {
  shiny::moduleServer(id, function(input, output, session) {
    overview <- shiny::reactive(
      app_overview(shiny::req(object()), shiny::req(qc()))
    )
    output$samples <- shiny::renderText(overview()$samples)
    output$cartridges <- shiny::renderText(overview()$cartridges)
    output$flagged <- shiny::renderText({
      reasons <- overview()$reasons
      paste0(
        overview()$flagged,
        if (length(reasons) > 0) {
          paste0(
            " (",
            paste(names(reasons), reasons, sep = ": ", collapse = ", "),
            ")"
          )
        }
      )
    })
    output$method <- shiny::renderText(
      paste0(overview()$method, ", ", overview()$preset, " preset")
    )
    overview
  })
}
