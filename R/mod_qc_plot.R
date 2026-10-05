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

app_colourless <- c("Stability", "PCBatch")

app_flag_metrics <- c("BD", "FoV", "PCL", "LoD")

download_bounds <- c(min = 5, max = 50)

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
  title <- app_plot_titles[[type]]
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      title,
      bslib::popover(
        shiny::tags$button(
          type = "button",
          class = "btn btn-sm btn-outline-secondary",
          `aria-label` = paste("Display options for", title),
          shiny::icon("sliders", `aria-hidden` = "true")
        ),
        title = "Display options",
        if (!type %in% app_colourless) {
          shiny::tagList(
            shiny::selectInput(
              ns("colour"),
              "Colour by",
              choices = "CartridgeID"
            ),
            shiny::checkboxInput(
              ns("show_legend"),
              "Show the legend",
              value = TRUE
            ),
            shiny::selectInput(
              ns("labels"),
              "Label flagged samples with",
              choices = c(None = "")
            )
          )
        },
        shiny::sliderInput(
          ns("size"),
          "Point size",
          min = 0.5,
          max = 4,
          value = 1,
          step = 0.5
        ),
        shiny::numericInput(
          ns("width"),
          "Download width (cm)",
          value = 16,
          min = download_bounds[["min"]],
          max = download_bounds[["max"]]
        ),
        shiny::numericInput(
          ns("height"),
          "Download height (cm)",
          value = 12,
          min = download_bounds[["min"]],
          max = download_bounds[["max"]]
        ),
        shiny::downloadButton(ns("download"), "Download PNG")
      )
    ),
    bslib::card_body(shiny::plotOutput(ns("plot"), height = "350px")),
    bslib::card_footer(shiny::textOutput(ns("summary")))
  )
}

plot_summary <- function(x, qc, type = NULL) {
  ids <- qc[[x@settings[["id_colname"]]]][qc[["status"]] %in% "fail"]
  cartridges <- length(unique(stats::na.omit(qc[["CartridgeID"]])))
  shown <- utils::head(ids, 3)
  overall <- paste0(
    nrow(qc),
    " samples",
    if (cartridges > 0) {
      paste0(" on ", cartridges, " cartridge", if (cartridges != 1) "s")
    },
    "; ",
    if (length(ids) == 0) {
      "none flagged."
    } else {
      paste0(
        length(ids),
        " flagged: ",
        paste(shown, collapse = ", "),
        if (length(ids) > length(shown)) {
          paste0(" and ", length(ids) - length(shown), " more")
        },
        "."
      )
    }
  )
  if (!isTRUE(type %in% app_flag_metrics)) {
    return(overall)
  }
  statuses <- qc[[paste0(type, "_status")]]
  if (all(is.na(statuses))) {
    return(paste0(overall, " ", type, " is not assessed for these data."))
  }
  on_metric <- sum(statuses %in% "fail")
  paste0(
    overall,
    " ",
    if (on_metric == 0) "None" else on_metric,
    " flagged on ",
    type,
    "."
  )
}

download_size <- function(value, default) {
  if (length(value) == 1 && is.finite(value)) {
    min(max(value, download_bounds[["min"]]), download_bounds[["max"]])
  } else {
    default
  }
}

mod_qc_plot_server <- function(id, object, qc, type = id, dark) {
  force(type)
  force(dark)
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
      shiny::updateSelectInput(
        session,
        "labels",
        choices = c(None = "", columns),
        selected = if (isTRUE(input$labels %in% columns)) input$labels else ""
      )
    })
    options <- shiny::reactive(
      list(
        colour = input$colour,
        show_legend = input$show_legend,
        size = input$size,
        outliers_labels = if (nzchar(input$labels %||% "")) input$labels
      )
    )
    plot <- shiny::reactive({
      app_plot(shiny::req(object()), type, options(), dark())
    })
    output$plot <- shiny::renderPlot(plot(), alt = plot_alt_texts[[type]]) |>
      shiny::bindCache(object(), type, options(), dark())
    output$summary <- shiny::renderText(
      plot_summary(shiny::req(object()), shiny::req(qc()), type)
    )
    output$download <- shiny::downloadHandler(
      filename = function() paste0("nacho-", type, ".png"),
      content = function(file) {
        ggplot2::ggsave(
          file,
          plot(),
          width = download_size(input$width, 16),
          height = download_size(input$height, 12),
          units = "cm",
          dpi = 150,
          bg = plot_colours(dark())[["paper"]]
        )
      }
    )
    plot
  })
}
