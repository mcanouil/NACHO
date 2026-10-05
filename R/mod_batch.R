#' @include mod_qc_plot.R
NULL

app_batch_variables <- c("CartridgeID", "Date")

#' Columns that can hold biological groups
#'
#' Text or factor columns with at least two levels, and at most one level for
#' every two samples, so identifiers and free text are left out.
#' The batch columns, and any column that splits the samples the same way,
#' are left out, since the page crosses groups with them.
#'
#' @noRd
group_choices <- function(samples) {
  partition <- function(v) match(v, unique(v))
  batches <- lapply(
    samples[intersect(app_batch_variables, names(samples))],
    partition
  )
  levels <- vapply(samples, function(v) length(unique(v)), integer(1))
  copies <- vapply(
    samples,
    function(v) any(vapply(batches, identical, logical(1), partition(v))),
    logical(1)
  )
  keep <- !vapply(samples, is.numeric, logical(1)) &
    levels >= 2 &
    levels <= nrow(samples) / 2 &
    !copies
  names(samples)[keep]
}

mod_batch_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      id = ns("batch-design"),
      fill = FALSE,
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
    groups <- shiny::reactive(group_choices(nacho_samples(shiny::req(object()))))
    shiny::observeEvent(object(), {
      shiny::updateSelectInput(
        session,
        "group",
        choices = c(None = "", groups()),
        selected = if (isTRUE(input$group %in% groups())) input$group else ""
      )
    })
    diagnostics <- shiny::reactive({
      group <- input$group
      shiny::req(nzchar(group %||% ""), group %in% groups(), batches())
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
      } else if (length(batches()) == 0) {
        "These data have no CartridgeID or Date column, so there is no batch to compare with the groups."
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
