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
  cartridges <- length(unique(stats::na.omit(qc[["CartridgeID"]])))
  lanes <- x@rcc_type == "n8" && "lane" %in% names(qc)
  unit <- if (lanes) "Lanes" else "Cartridges"
  units <- if (lanes) {
    nrow(unique(stats::na.omit(qc[c("CartridgeID", "lane")])))
  } else {
    cartridges
  }
  list(
    samples = nrow(qc),
    cartridges = cartridges,
    unit = unit,
    units = units,
    flagged = sum(qc[["status"]] %in% "fail"),
    method = x@settings[["normalisation_method"]],
    preset = x@thresholds[["preset"]],
    reasons = reasons
  )
}

reasons_text <- function(reasons) {
  if (length(reasons) == 0) {
    "None"
  } else {
    paste(names(reasons), reasons, sep = ": ", collapse = ", ")
  }
}

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  icon <- function(name) shiny::icon(name, `aria-hidden` = "true")
  item <- function(icon_name, label, value, extra = NULL) {
    shiny::tags$div(
      class = "nacho-summary-item",
      icon(icon_name),
      shiny::tags$span(class = "nacho-summary-label", label),
      shiny::tags$span(class = "nacho-summary-value", value),
      extra
    )
  }
  shiny::tags$div(
    class = "nacho-summary",
    role = "group",
    `aria-label` = "Summary of the data",
    item("vial", "Samples", shiny::textOutput(ns("samples"), inline = TRUE)),
    item(
      "layer-group",
      shiny::textOutput(ns("unit"), inline = TRUE),
      shiny::textOutput(ns("units"), inline = TRUE)
    ),
    item(
      "triangle-exclamation",
      "Flagged",
      shiny::textOutput(ns("flagged_count"), inline = TRUE),
      shiny::tagList(
        bslib::tooltip(
          shiny::tags$button(
            type = "button",
            class = "btn btn-link btn-sm p-0 nacho-summary-info",
            `aria-label` = "Why samples are flagged",
            icon("circle-info")
          ),
          shiny::textOutput(ns("reasons_tip"), inline = TRUE)
        ),
        shiny::tags$span(
          class = "visually-hidden",
          shiny::textOutput(ns("reasons"), inline = TRUE)
        )
      )
    ),
    item(
      "scale-balanced",
      "Method",
      shiny::tagList(
        shiny::textOutput(ns("method"), inline = TRUE),
        " \u00b7 ",
        shiny::textOutput(ns("preset"), inline = TRUE)
      )
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
    output$unit <- shiny::renderText(overview()$unit)
    output$units <- shiny::renderText(overview()$units)
    output$reasons <- shiny::renderText(reasons_text(overview()$reasons))
    output$reasons_tip <- shiny::renderText(reasons_text(overview()$reasons))
    output$method <- shiny::renderText(overview()$method)
    output$preset <- shiny::renderText(paste(
      preset_label(overview()$preset),
      "preset"
    ))
    overview
  })
}
