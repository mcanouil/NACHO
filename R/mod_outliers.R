#' @include report.R
NULL

#' The rows of the samples table
#'
#' Flagged samples come first, the samples with the most flags at the top.
#'
#' @param x A `nacho` object.
#' @param qc The quality-control table of `x`.
#'
#' @noRd
samples_table_data <- function(x, qc) {
  columns <- intersect(
    c(
      x@settings[["id_colname"]],
      "lane",
      "lane_status",
      "CartridgeID",
      "status",
      "n_flags",
      "reason"
    ),
    names(qc)
  )
  data <- qc[, columns, drop = FALSE]
  data <- data[
    order(!data[["status"]] %in% "fail", -data[["n_flags"]]),
    ,
    drop = FALSE
  ]
  rownames(data) <- NULL
  data
}

#' Show a quality-control status as a coloured word
#'
#' @noRd
status_badge <- function(status) {
  if (is.na(status)) {
    return(htmltools::span(class = "badge text-bg-secondary", "not assessed"))
  }
  style <- c(fail = "text-bg-danger", pass = "text-bg-success")[[status]]
  htmltools::span(class = paste("badge", style), status)
}

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
      if (interactive) {
        "It is also outlined in the plots that show one point per sample."
      }
    ),
    shiny::div(
      bslib::input_switch(ns("flagged_only"), "Flagged only"),
      shiny::uiOutput(ns("body")),
      if (has_package("reactable")) {
        reactable::reactableOutput(ns("samples"))
      } else {
        shiny::tableOutput(ns("samples"))
      }
    )
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
    rows <- shiny::reactive({
      data <- samples_table_data(shiny::req(object()), shiny::req(qc()))
      if (isTRUE(input$flagged_only)) {
        data <- data[data[["status"]] %in% "fail", , drop = FALSE]
      }
      data
    })
    shiny::observeEvent(object(), {
      ids <- colnames(object()@counts)
      if (!all(selected() %in% ids)) {
        selected(character())
      }
      shiny::updateSelectInput(
        session,
        "highlight",
        choices = c(None = "", ids),
        selected = if (length(selected()) == 1) selected() else ""
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
    output$body <- shiny::renderUI({
      if (nrow(failures()) == 0) {
        shiny::tags$p("No sample is flagged.")
      }
    })
    if (has_package("reactable")) {
      output$samples <- reactable::renderReactable({
        data <- rows()
        chosen <- match(shiny::isolate(selected()), data[[1]])
        columns <- list(
          lane = reactable::colDef(name = "Lane", maxWidth = 80),
          lane_status = reactable::colDef(
            name = "Lane status",
            cell = status_badge
          ),
          CartridgeID = reactable::colDef(name = "Cartridge", minWidth = 150),
          status = reactable::colDef(name = "Status", cell = status_badge),
          n_flags = reactable::colDef(name = "Flags", maxWidth = 80),
          reason = reactable::colDef(name = "Reason", minWidth = 160)
        )
        columns[[names(data)[1]]] <- reactable::colDef(minWidth = 260)
        reactable::reactable(
          data,
          columns = columns[intersect(names(columns), names(data))],
          selection = "single",
          onClick = "select",
          defaultSelected = chosen[!is.na(chosen)],
          searchable = TRUE,
          highlight = TRUE,
          compact = TRUE,
          defaultPageSize = 20,
          theme = reactable::reactableTheme(
            color = "inherit",
            backgroundColor = "transparent",
            borderColor = "var(--bs-border-color)",
            highlightColor = "var(--bs-tertiary-bg)",
            rowSelectedStyle = list(
              backgroundColor = "var(--bs-secondary-bg)",
              boxShadow = "inset 3px 0 0 0 var(--bs-primary)"
            ),
            inputStyle = list(
              backgroundColor = "transparent",
              color = "inherit"
            ),
            paginationStyle = list(color = "inherit")
          )
        )
      })
      shiny::observeEvent(
        reactable::getReactableState("samples", "selected"),
        {
          id <- rows()[[1]][reactable::getReactableState("samples", "selected")]
          if (!identical(id, selected())) {
            selected(id)
          }
        }
      )
      shiny::observeEvent(
        selected(),
        {
          chosen <- match(selected(), rows()[[1]])
          reactable::updateReactable(
            "samples",
            selected = if (length(chosen) == 1 && !is.na(chosen)) chosen else NA
          )
        },
        ignoreNULL = FALSE,
        ignoreInit = TRUE
      )
    } else {
      output$samples <- shiny::renderTable(
        {
          table <- rows()
          chosen <- table[[1]] %in% selected()
          status <- vapply(
            table[["status"]],
            function(value) as.character(status_badge(value)),
            character(1)
          )
          table[] <- lapply(table, function(column) {
            htmltools::htmlEscape(as.character(column))
          })
          table[["status"]] <- status
          table[[1]] <- ifelse(
            chosen,
            paste0("<strong>", table[[1]], "</strong>"),
            table[[1]]
          )
          table
        },
        sanitize.text.function = function(x) x,
        sanitize.colnames.function = htmltools::htmlEscape
      )
    }
    failures
  })
}
