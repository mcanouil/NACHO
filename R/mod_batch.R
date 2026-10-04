#' @include mod_qc_plot.R
NULL

app_batch_variables <- c("CartridgeID", "Date")

mod_batch_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      id = ns("batch-design"),
      bslib::card_header("Batches and biological groups"),
      shiny::selectInput(
        ns("group"),
        "Biological group column",
        choices = c(None = "")
      ),
      shiny::textOutput(ns("design_note")),
      shiny::tableOutput(ns("design")),
      lapply(app_batch_variables, function(batch) {
        shiny::tagList(
          shiny::tags$h3(class = "h6", paste("Groups by", batch)),
          shiny::tableOutput(ns(paste0("crosstab_", batch)))
        )
      })
    ),
    plot_page("batch")
  )
}

mod_batch_server <- function(id, object) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(object(), {
      shiny::updateSelectInput(
        session,
        "group",
        choices = c(None = "", names(nacho_samples(object()))),
        selected = input$group
      )
    })
    diagnostics <- shiny::reactive({
      group <- input$group
      shiny::req(nzchar(group %||% ""))
      batch_diagnostics(
        shiny::req(object()),
        group = group,
        batch = app_batch_variables
      )
    })
    design <- shiny::reactive(diagnostics()$design)
    output$design_note <- shiny::renderText({
      if (!nzchar(input$group %||% "")) {
        "Choose the column that holds the biological groups to check whether batches and groups are confounded."
      } else if (any(design()$confounded)) {
        paste(
          "At least one batch level holds a single group:",
          "batch and biology cannot be told apart,",
          "and no normalisation can fix it."
        )
      } else {
        "Every batch level holds more than one group."
      }
    })
    output$design <- shiny::renderTable(design(), digits = 2)
    lapply(app_batch_variables, function(batch) {
      output[[paste0("crosstab_", batch)]] <- shiny::renderTable(
        as.data.frame.matrix(diagnostics()$crosstabs[[batch]]),
        rownames = TRUE
      )
    })
    design
  })
}
