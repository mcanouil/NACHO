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
        shiny::uiOutput(ns(paste0("crosstab_", batch)))
      })
    ),
    plot_page("batch")
  )
}

mod_batch_server <- function(id, object) {
  shiny::moduleServer(id, function(input, output, session) {
    columns <- shiny::reactive(names(nacho_samples(shiny::req(object()))))
    batches <- shiny::reactive(intersect(app_batch_variables, columns()))
    shiny::observeEvent(object(), {
      shiny::updateSelectInput(
        session,
        "group",
        choices = c(None = "", columns()),
        selected = if (isTRUE(input$group %in% columns())) input$group else ""
      )
    })
    diagnostics <- shiny::reactive({
      group <- input$group
      shiny::req(nzchar(group %||% ""), group %in% columns(), batches())
      batch_diagnostics(
        shiny::req(object()),
        group = group,
        batch = batches()
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
      output[[paste0("crosstab_", batch)]] <- shiny::renderUI({
        if (!batch %in% columns()) {
          shiny::tags$p(
            class = "text-muted",
            paste(batch, "is not in these data.")
          )
        } else {
          shiny::tagList(
            shiny::tags$h3(class = "h6", paste("Groups by", batch)),
            shiny::tableOutput(session$ns(paste0("table_", batch)))
          )
        }
      })
      output[[paste0("table_", batch)]] <- shiny::renderTable(
        {
          shiny::req(batch %in% batches())
          as.data.frame.matrix(diagnostics()$crosstabs[[batch]])
        },
        rownames = TRUE
      )
    })
    design
  })
}
