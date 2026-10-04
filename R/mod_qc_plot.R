#' @include report.R
NULL

#' Plot types shown on each page of the app
#'
#' @noRd
app_plot_types <- list(
  qc_metrics = c("BD", "FoV", "PCL", "LoD"),
  controls = c("Positive", "Negative", "Housekeeping", "PN"),
  counts = c("ACBD", "ACMC", "PCA12", "PCAi", "PCA"),
  normalisation = c("PFNF", "HF", "NORM", "RLE", "Stability"),
  batch = c("BatchFactors", "PCBatch")
)

app_plot_titles <- c(
  BD = "Binding density",
  FoV = "Field of view",
  PCL = "Positive control linearity",
  LoD = "Limit of detection",
  Positive = "Positive controls",
  Negative = "Negative controls",
  Housekeeping = "Housekeeping genes",
  PN = "Control probe expression",
  ACBD = "Average count against binding density",
  ACMC = "Average count against median count",
  PCA12 = "First two principal components",
  PCAi = "Variance explained",
  PCA = "Planes of the first components",
  PFNF = "Positive against negative factor",
  HF = "Housekeeping factor",
  NORM = "Normalisation result",
  RLE = "Relative log expression",
  Stability = "Housekeeping gene stability",
  BatchFactors = "Normalisation factors by cartridge",
  PCBatch = "Principal components and batches"
)

#' Draw one plot for the app
#'
#' Unknown colour columns fall back to `CartridgeID`.
#' Warnings about unavailable metrics are muffled, because the app shows an
#' empty plot for them.
#'
#' @noRd
app_plot <- function(x, type, options, dark) {
  colour <- options[["colour"]]
  if (is.null(colour) || !colour %in% names(nacho_samples(x))) {
    colour <- "CartridgeID"
  }
  withCallingHandlers(
    autoplot(
      x,
      type = type,
      colour = colour,
      size = options[["size"]] %||% 1,
      show_legend = options[["show_legend"]] %||% TRUE,
      outliers_labels = options[["outliers_labels"]],
      dark = dark
    ),
    nacho_warning_metric_unavailable = function(cnd) {
      invokeRestart("muffleWarning")
    }
  )
}

mod_qc_plot_ui <- function(id, type = id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(app_plot_titles[[type]]),
    shiny::plotOutput(ns("plot"), height = "350px"),
    bslib::card_footer(
      shiny::selectInput(ns("colour"), "Colour by", choices = "CartridgeID")
    )
  )
}

mod_qc_plot_server <- function(id, object, type = id, dark) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(object(), {
      columns <- names(nacho_samples(object()))
      shiny::updateSelectInput(
        session,
        "colour",
        choices = columns,
        selected = if (isTRUE(input$colour %in% columns)) {
          input$colour
        } else {
          "CartridgeID"
        }
      )
    })
    plot <- shiny::reactive({
      app_plot(shiny::req(object()), type, list(colour = input$colour), dark())
    })
    output$plot <- shiny::renderPlot(plot(), alt = plot_alt_texts[[type]]) |>
      shiny::bindCache(object(), type, input$colour, dark())
    plot
  })
}
