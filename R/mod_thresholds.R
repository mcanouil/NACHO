#' @include report.R
NULL

#' One sentence of help below each threshold
#'
#' @noRd
threshold_help <- c(
  BD = "Optical features per square micron. Bruker flags 0.05 to 2.25 on MAX, FLEX and PRO, and 0.1 to 1.8 on SPRINT.",
  FoV = "Share of fields of view the scanner could count. Bruker flags below 75%.",
  PCL = "R squared of the positive controls against their concentrations. Bruker flags below 0.95.",
  LoD = "How far the 0.5 fM positive control sits above background, in standard deviations. Bruker flags below 2.",
  Positive_factor = "Scaling from the positive controls. Bruker flags outside 0.3 to 3.",
  House_factor = "Scaling from the housekeeping genes. Bruker flags outside 0.1 to 10.",
  Housekeeping_detected = "Housekeeping genes above background in the sample.",
  Ligation_order = "1 when the ligation positive controls are in order, else 0.",
  Ligation_R2 = "R squared of the ligation positive controls against their positions.",
  Ligation_NEG = "Largest ligation negative minus the detection limit.",
  Haemolysis = "log2 of miR-451a over miR-23a-3p; above 7 suggests haemolysis."
)

threshold_metrics <- function(x) {
  known <- qc_metrics[
    qc_metrics %in% intersect(names(x@samples), names(x@thresholds))
  ]
  known[vapply(
    known,
    function(metric) any(!is.na(x@samples[[metric]])),
    logical(1)
  )]
}

threshold_range <- function(values, limits) {
  finite <- c(values[is.finite(values)], limits[is.finite(limits)])
  if (!length(finite)) {
    return(c(0, 1))
  }
  range(pretty(c(0, finite)))
}

limits_to_slider <- function(limits, range) {
  pmin(pmax(limits, range[1]), range[2])
}

slider_to_limits <- function(value, range, original) {
  lower <- if (is.infinite(original[1]) && value[1] <= range[1]) {
    original[1]
  } else {
    value[1]
  }
  if (length(original) == 1) {
    return(lower)
  }
  upper <- if (is.infinite(original[2]) && value[2] >= range[2]) {
    original[2]
  } else {
    value[2]
  }
  c(lower, upper)
}

count_metrics <- c("Housekeeping_detected", "Ligation_order")

slider_step <- function(metric, range, limits) {
  if (metric %in% count_metrics) {
    return(1)
  }
  finite <- limits[is.finite(limits)]
  exponent <- floor(log10(diff(range) / 100))
  for (candidate in 10^seq(exponent, exponent - 3)) {
    ratio <- finite / candidate
    if (all(abs(ratio - round(ratio)) < 1e-8)) {
      return(candidate)
    }
  }
  10^exponent
}

threshold_inputs <- function(x, limits, ns, metrics = threshold_metrics(x)) {
  shiny::tagList(lapply(metrics, function(metric) {
    range <- threshold_range(x@samples[[metric]], limits[[metric]])
    shiny::tagList(
      shiny::sliderInput(
        ns(metric),
        label = paste0(qc_metric_labels[[metric]], " (", metric, ")"),
        min = range[1],
        max = range[2],
        value = limits_to_slider(limits[[metric]], range),
        step = slider_step(metric, range, limits[[metric]])
      ),
      shiny::helpText(threshold_help[[metric]])
    )
  }))
}

normalisation_choices <- function(x) {
  c(
    "GEO",
    "GLM",
    "RUVg",
    if (identical(x@settings[["panel"]], "mirna")) {
      c("stable_mirna", "total_mirna", "spike_in", "ligation")
    }
  )
}

mod_thresholds_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      ns("preset"),
      "Threshold preset",
      c(nSolver = "nsolver", "NACHO 2" = "legacy")
    ),
    shiny::selectInput(
      ns("instrument"),
      "Instrument",
      c(MAX = "max", FLEX = "flex", PRO = "pro", SPRINT = "sprint")
    ),
    shiny::selectInput(ns("method"), "Normalisation method", "GEO"),
    shiny::numericInput(
      ns("ruv_k"),
      "Unwanted factors (RUVg)",
      value = 1,
      min = 1,
      max = 10,
      step = 1
    ),
    shiny::selectInput(
      ns("background"),
      "Background",
      c("none", "mean", "mean_2sd", "median", "max", "geo")
    ),
    shiny::selectInput(
      ns("background_mode"),
      "Background mode",
      c("threshold", "subtract")
    ),
    shiny::uiOutput(ns("sliders"))
  )
}

mod_thresholds_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    current <- shiny::reactiveVal()

    shiny::observeEvent(data(), {
      x <- data()
      current(x@thresholds)
      instrument <- x@thresholds[["instrument"]]
      shiny::updateSelectInput(
        session,
        "preset",
        selected = x@thresholds[["preset"]]
      )
      shiny::updateSelectInput(
        session,
        "instrument",
        selected = if (is.na(instrument)) "max" else instrument
      )
      shiny::updateSelectInput(
        session,
        "method",
        choices = normalisation_choices(x),
        selected = x@settings[["normalisation_method"]]
      )
      shiny::updateSelectInput(
        session,
        "background",
        selected = x@settings[["background"]]
      )
      shiny::updateSelectInput(
        session,
        "background_mode",
        selected = x@settings[["background_mode"]]
      )
    })

    reset <- function() {
      limits <- shiny::req(current())
      current(nacho_thresholds(
        instrument = input$instrument,
        preset = input$preset,
        haemolysis = any(is.finite(limits[["Haemolysis"]]))
      ))
    }

    shiny::observeEvent(
      list(input$preset, input$instrument),
      {
        limits <- shiny::req(current(), input$preset, input$instrument)
        instrument <- limits[["instrument"]]
        if (is.na(instrument)) {
          instrument <- "max"
        }
        moved <- !identical(input$preset, limits[["preset"]]) ||
          !identical(input$instrument, instrument)
        if (moved) reset()
      },
      ignoreInit = TRUE
    )

    output$sliders <- shiny::renderUI({
      threshold_inputs(shiny::req(data()), shiny::req(current()), session$ns)
    })

    current_thresholds <- shiny::reactive({
      x <- shiny::req(data())
      limits <- shiny::req(current())
      for (metric in threshold_metrics(x)) {
        value <- input[[metric]]
        if (!is.null(value)) {
          range <- threshold_range(x@samples[[metric]], limits[[metric]])
          limits[[metric]] <- slider_to_limits(value, range, limits[[metric]])
        }
      }
      limits
    })
    thresholds <- shiny::debounce(current_thresholds, 500)

    settings <- shiny::reactive({
      x <- shiny::req(data())
      method <- input$method %||% x@settings[["normalisation_method"]]
      list(
        normalisation_method = method,
        ruv_k = if (method == "RUVg") as.integer(input$ruv_k %||% 1L),
        background = input$background %||% x@settings[["background"]],
        background_mode = input$background_mode %||%
          x@settings[["background_mode"]]
      )
    })

    list(
      thresholds = thresholds,
      current_thresholds = current_thresholds,
      settings = settings,
      reset = reset
    )
  })
}
