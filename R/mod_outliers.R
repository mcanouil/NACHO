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
  data <- qc[, qc_table_columns(x, qc), drop = FALSE]
  data <- data[
    order(!data[["status"]] %in% "fail", -data[["n_flags"]]),
    ,
    drop = FALSE
  ]
  rownames(data) <- NULL
  data
}

#' The labels of the samples table columns
#'
#' @noRd
samples_table_labels <- c(
  lane = "Lane",
  lane_status = "Lane status",
  CartridgeID = "Cartridge",
  status = "Status",
  n_flags = "Flags",
  reason = "Reason"
)

#' Show quality-control statuses as coloured words
#'
#' @param status A character vector of `"fail"`, `"pass"` or `NA`.
#'
#' @return HTML strings.
#'
#' @noRd
status_badge <- function(status) {
  style <- ifelse(
    is.na(status),
    "secondary",
    ifelse(status %in% "fail", "danger", "success")
  )
  label <- ifelse(is.na(status), "not assessed", htmltools::htmlEscape(status))
  paste0('<span class="badge text-bg-', style, '">', label, "</span>")
}

#' The look of the reactable samples table, from the Bootstrap colours
#'
#' @noRd
samples_table_theme <- function() {
  reactable::reactableTheme(
    color = "inherit",
    backgroundColor = "transparent",
    borderColor = "var(--bs-border-color)",
    highlightColor = "var(--bs-tertiary-bg)",
    rowSelectedStyle = list(
      backgroundColor = "var(--bs-secondary-bg)",
      boxShadow = "inset 3px 0 0 0 var(--bs-primary)"
    ),
    inputStyle = list(backgroundColor = "transparent", color = "inherit"),
    paginationStyle = list(color = "inherit")
  )
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
      shiny::textOutput(ns("message")),
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
    output$message <- shiny::renderText(
      if (nrow(failures()) == 0) "No sample is flagged." else ""
    )
    badged <- c("status", "lane_status")
    if (has_package("reactable")) {
      output$samples <- reactable::renderReactable({
        data <- rows()
        chosen <- match(shiny::isolate(selected()), data[[1]])
        shown <- intersect(names(samples_table_labels), names(data))
        min_width <- c(CartridgeID = 150, reason = 160)
        columns <- stats::setNames(
          lapply(shown, function(column) {
            reactable::colDef(
              name = samples_table_labels[[column]],
              cell = if (column %in% badged) status_badge,
              html = column %in% badged,
              minWidth = if (column %in% names(min_width)) min_width[[column]],
              maxWidth = if (column %in% c("lane", "n_flags")) 80
            )
          }),
          shown
        )
        columns[[names(data)[1]]] <- reactable::colDef(minWidth = 260)
        reactable::reactable(
          data,
          columns = columns,
          selection = "single",
          onClick = "select",
          defaultSelected = chosen[!is.na(chosen)],
          searchable = TRUE,
          highlight = TRUE,
          compact = TRUE,
          defaultPageSize = 20,
          theme = samples_table_theme()
        )
      })
      shiny::observeEvent(
        input$samples__reactable__selected,
        {
          index <- input$samples__reactable__selected
          if (length(index) > 0) {
            id <- rows()[[1]][index]
          } else if (any(selected() %in% rows()[[1]])) {
            id <- character()
          } else {
            return(invisible(NULL))
          }
          if (!identical(id, selected())) {
            selected(id)
          }
        },
        ignoreNULL = FALSE,
        ignoreInit = TRUE
      )
      shiny::observeEvent(
        selected(),
        {
          chosen <- match(selected(), rows()[[1]])
          wanted <- if (length(chosen) == 1 && !is.na(chosen)) chosen
          shown <- input$samples__reactable__selected
          if (!identical(as.integer(wanted), as.integer(shown))) {
            reactable::updateReactable("samples", selected = wanted %||% NA)
          }
        },
        ignoreNULL = FALSE,
        ignoreInit = TRUE
      )
    } else {
      output$samples <- shiny::renderTable(
        {
          table <- rows()
          chosen <- table[[1]] %in% selected()
          plain <- setdiff(names(table), badged)
          table[plain] <- lapply(table[plain], function(column) {
            htmltools::htmlEscape(as.character(column))
          })
          for (column in intersect(badged, names(table))) {
            table[[column]] <- status_badge(table[[column]])
          }
          table[[1]] <- ifelse(
            chosen,
            paste0("<strong>", table[[1]], "</strong>"),
            table[[1]]
          )
          labelled <- names(table) %in% names(samples_table_labels)
          names(table)[labelled] <- samples_table_labels[names(table)[labelled]]
          table
        },
        sanitize.text.function = function(x) x,
        sanitize.colnames.function = htmltools::htmlEscape
      )
    }
    failures
  })
}
