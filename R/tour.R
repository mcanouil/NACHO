#' The HTML dependency of the vendored driver.js
#'
#' The files are driver.js 1.9.0, unchanged below their header comments.
#' The header of `inst/tour/driver.js` says how to update them.
#'
#' @noRd
driver_dependency <- function() {
  htmltools::htmlDependency(
    name = "driver.js",
    version = "1.9.0",
    src = "tour",
    package = "NACHO",
    script = "driver.js",
    stylesheet = "driver.css"
  )
}

#' The HTML dependency of the NACHO tour, which drives driver.js
#'
#' @noRd
tour_dependency <- function() {
  htmltools::htmlDependency(
    name = "nacho-tour",
    version = as.character(utils::packageVersion("NACHO")),
    src = "tour",
    package = "NACHO",
    script = "nacho-tour.js",
    stylesheet = "nacho-tour.css"
  )
}

#' Both dependencies of the tour, driver.js first
#'
#' @noRd
tour_dependencies <- function() {
  list(driver_dependency(), tour_dependency())
}

#' The steps of the app tour
#'
#' Each step names a CSS selector of a stable element of the app, a title
#' and a description in plain text, and optionally the page to show, the
#' threshold panel to open and the side of the card.
#' `nacho-tour.js` escapes the text, so it must not hold HTML.
#'
#' @noRd
tour_steps <- function() {
  list(
    list(
      element = "#data-card",
      title = "Load your data",
      description = paste(
        "Choose RCC files, zip archives of RCC files and an optional sample",
        "sheet (CSV). Then click Import."
      ),
      page = "Data"
    ),
    list(
      element = ".nacho-summary",
      title = "Summary of the data",
      description = paste(
        "This strip shows the samples, the cartridges, the flagged samples",
        "and the method. It shows on every page and changes when you change a",
        "threshold."
      ),
      page = "Data",
      side = "bottom"
    ),
    list(
      element = "#thresholds-sections",
      title = "Thresholds",
      description = paste(
        "Choose a preset and your instrument. Then move a slider to change",
        "one threshold. Every page uses these thresholds."
      ),
      page = "Data",
      open = "Preset",
      side = "right"
    ),
    list(
      element = "#BD-card",
      title = "Quality-control plots",
      description = paste(
        "Each card shows one metric for each sample. Shaded bands show the",
        "thresholds, and flagged samples show as triangles."
      ),
      page = "QC metrics"
    ),
    list(
      element = "button[aria-label='Display options for Binding density']",
      title = "Display options",
      description = paste(
        "Use this button to change the colours, the legend, the labels and",
        "the point size, or to download the plot as a PNG file."
      ),
      page = "QC metrics",
      side = "left"
    ),
    list(
      element = "#outliers-highlight",
      title = "Samples",
      description = paste(
        "Choose a sample to mark it in the tables. Interactive plots outline",
        "the same sample."
      ),
      page = "Samples"
    ),
    list(
      element = "#batch-batch-design",
      title = "Batches and groups",
      description = paste(
        "Choose the column that holds the biological groups. The page then",
        "shows whether batches and groups are confounded."
      ),
      page = "Batch"
    ),
    list(
      element = "#export-cards",
      title = "Export",
      description = paste(
        "Download the quality-control table, the thresholds and the nacho",
        "object, or render a report."
      ),
      page = "Export"
    ),
    list(
      element = "a.dropdown-toggle[data-value='help']",
      title = "Help",
      description = paste(
        "Open this menu to take the tour again, to read about NACHO or to",
        "report a problem."
      ),
      side = "bottom"
    )
  )
}

#' Start the app tour in the browser
#'
#' @param session The Shiny session.
#'
#' @noRd
start_tour <- function(session) {
  session$sendCustomMessage("nacho-tour-start", list(steps = tour_steps()))
}
