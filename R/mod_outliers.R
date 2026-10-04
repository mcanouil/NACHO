#' @include report.R
NULL

mod_outliers_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Flagged samples"),
    shiny::tableOutput(ns("failures"))
  )
}

mod_outliers_server <- function(id, object, qc) {
  shiny::moduleServer(id, function(input, output, session) {
    failures <- shiny::reactive(
      qc_failures(shiny::req(object()), shiny::req(qc()))
    )
    output$failures <- shiny::renderTable(failures())
    failures
  })
}
