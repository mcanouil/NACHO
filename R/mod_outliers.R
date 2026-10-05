#' @include report.R
NULL

mod_outliers_ui <- function(id, interactive = FALSE) {
  ns <- shiny::NS(id)
  hint_id <- ns("highlight-hint")
  select <- shiny::selectInput(
    ns("highlight"),
    "Highlight a sample",
    choices = c(None = ""),
    selectize = FALSE
  )
  select <- htmltools::tagQuery(select)$find("select")$addAttrs(
    `aria-describedby` = hint_id
  )$allTags()
  bslib::card(
    bslib::card_header("Samples"),
    select,
    shiny::helpText(
      id = hint_id,
      "The chosen sample is marked in the table below.",
      if (interactive) "It is also outlined in every plot."
    ),
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
      ids <- colnames(object()@counts)
      if (!all(selected() %in% ids)) {
        selected(character())
      }
      shiny::updateSelectInput(
        session,
        "highlight",
        choices = c(None = "", ids),
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
        chosen <- table[[1]] %in% selected()
        table[] <- lapply(table, function(column) {
          htmltools::htmlEscape(as.character(column))
        })
        table[[1]] <- ifelse(
          chosen,
          paste0("<strong>", table[[1]], "</strong>"),
          table[[1]]
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
