#' @include report.R
NULL

mod_outliers_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Flagged samples"),
    shiny::uiOutput(ns("body"))
  )
}

mod_outliers_server <- function(id, object, qc) {
  shiny::moduleServer(id, function(input, output, session) {
    failures <- shiny::reactive(
      qc_failures(shiny::req(object()), shiny::req(qc()))
    )
    output$failures <- shiny::renderTable(failures())
    output$body <- shiny::renderUI({
      if (nrow(failures()) == 0) {
        shiny::tags$p("No sample is flagged.")
      } else {
        shiny::tableOutput(session$ns("failures"))
      }
    })
    failures
  })
}
