#' @include report.R
NULL

mod_outliers_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Samples"),
    shiny::selectInput(
      ns("highlight"),
      "Highlight a sample",
      choices = c(None = ""),
      selectize = FALSE
    ),
    shiny::helpText("The chosen sample is outlined in every plot."),
    shiny::uiOutput(ns("body"))
  )
}

mod_outliers_server <- function(
  id,
  object,
  qc,
  selected = shiny::reactiveVal(character())
) {
  shiny::moduleServer(id, function(input, output, session) {
    failures <- shiny::reactive(
      qc_failures(shiny::req(object()), shiny::req(qc()))
    )
    shiny::observeEvent(object(), {
      shiny::updateSelectInput(
        session,
        "highlight",
        choices = c(None = "", colnames(object()@counts)),
        selected = selected()
      )
    })
    shiny::observeEvent(
      input$highlight,
      selected(if (nzchar(input$highlight)) input$highlight else character())
    )
    shiny::observeEvent(
      selected(),
      shiny::updateSelectInput(
        session,
        "highlight",
        selected = if (length(selected()) == 1) selected() else ""
      ),
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
    output$failures <- shiny::renderTable(failures())
    output$samples <- shiny::renderTable(
      {
        table <- shiny::req(qc())[, c(
          shiny::req(object())@settings[["id_colname"]],
          "CartridgeID",
          "status",
          "n_flags",
          "reason"
        )]
        id_column <- names(table)[[1]]
        table[[id_column]] <- ifelse(
          table[[id_column]] %in% selected(),
          paste0("<strong>", table[[id_column]], "</strong>"),
          table[[id_column]]
        )
        table
      },
      sanitize.text.function = function(x) x
    )
    output$body <- shiny::renderUI({
      shiny::tagList(
        if (nrow(failures()) == 0) {
          shiny::tags$p("No sample is flagged.")
        } else {
          shiny::tableOutput(session$ns("failures"))
        },
        shiny::tags$h3(class = "h6", "All samples"),
        shiny::tableOutput(session$ns("samples"))
      )
    })
    failures
  })
}
