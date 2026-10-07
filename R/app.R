#' @include mod_data.R mod_thresholds.R mod_qc_plot.R mod_overview.R mod_outliers.R mod_batch.R mod_export.R
NULL

#' Run the NACHO app
#'
#' Opens the quality-control app on a `nacho` object, or on an empty upload
#' page.
#' Use [visualise()] to get the tuned object back in the R session.
#'
#' @param x A `nacho` object from [load_rcc()] or [normalise()], or `NULL` to
#'   start by uploading RCC files.
#' @param done If `TRUE`, the app shows a "Done" button that closes it and
#'   returns the tuned object.
#'   Keep `FALSE` for a deployed app, because closing it would stop the
#'   server for every user.
#'
#' @return A Shiny app object; print it or pass it to [shiny::runApp()].
#' @export
#'
#' @examples
#' if (interactive()) {
#'   shiny::runApp(nacho_app(GSE74821))
#' }
nacho_app <- function(x = NULL, done = FALSE) {
  if (!is.null(x)) {
    check_nacho(x)
  }
  shiny::addResourcePath("nacho-brand", brand_path())
  interactive <- plots_interactive()
  quarto <- quarto_available()
  shiny::shinyApp(
    ui = function(request) app_ui(done, interactive, quarto),
    server = app_server(x, done, interactive, quarto)
  )
}

help_page <- function(name) {
  shiny::markdown(help_text(name))
}

empty_state <- function() {
  bslib::card(
    bslib::card_body(
      class = "text-center",
      shiny::tags$p("No data yet."),
      shiny::tags$p(
        "Load RCC files on the Data page, or load the example data to explore the app."
      )
    )
  )
}

with_data <- function(...) {
  shiny::tagList(
    shiny::conditionalPanel("output.has_data === true", ...),
    shiny::conditionalPanel("output.has_data === false", empty_state())
  )
}

plot_page <- function(page, interactive = FALSE) {
  bslib::layout_columns(
    col_widths = bslib::breakpoints(sm = 12, lg = 6),
    !!!lapply(app_plot_types[[page]], function(type) {
      shiny::conditionalPanel(
        sprintf(
          "output.applicable && output.applicable.indexOf(',%s,') >= 0",
          type
        ),
        mod_qc_plot_ui(type, interactive = interactive)
      )
    })
  )
}

app_ui <- function(done = FALSE, interactive = FALSE, quarto = FALSE) {
  bslib::page_navbar(
    title = shiny::tags$span(
      shiny::tags$img(src = "nacho-brand/nacho_hex.png", height = 24, alt = ""),
      "NACHO"
    ),
    window_title = "NACHO",
    fillable = FALSE,
    id = "page",
    header = shiny::tagList(
      shiny::useBusyIndicators(),
      brand_font_dependency(),
      shiny::conditionalPanel(
        "output.has_data === true",
        mod_overview_ui("overview")
      )
    ),
    theme = nacho_theme(),
    navbar_options = bslib::navbar_options(
      bg = nacho_palette[["navy"]],
      theme = "dark"
    ),
    sidebar = bslib::sidebar(
      title = "Thresholds",
      width = 320,
      mod_thresholds_ui("thresholds"),
      if (done) shiny::actionButton("done", "Done", class = "btn-primary")
    ),
    bslib::nav_panel("Data", mod_data_ui("data")),
    bslib::nav_panel(
      "QC metrics",
      with_data(plot_page("qc_metrics", interactive))
    ),
    bslib::nav_panel("Controls", with_data(plot_page("controls", interactive))),
    bslib::nav_panel("Counts", with_data(plot_page("counts", interactive))),
    bslib::nav_panel(
      "Normalisation",
      with_data(plot_page("normalisation", interactive))
    ),
    bslib::nav_panel("Batch", with_data(mod_batch_ui("batch", interactive))),
    bslib::nav_panel(
      "Samples",
      with_data(mod_outliers_ui("outliers", interactive))
    ),
    bslib::nav_panel("Export", with_data(mod_export_ui("export", quarto))),
    help_menu(),
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode(id = "dark_mode"))
  )
}

help_links <- function() {
  description <- utils::packageDescription("NACHO")
  urls <- trimws(strsplit(description[["URL"]], ",")[[1]])
  github <- urls[grepl("^https://github.com/", urls)][1]
  site <- urls[!grepl("^https://github.com/", urls)][1]
  c(
    documentation = site,
    discussions = paste0(sub("/$", "", github), "/discussions"),
    issues = description[["BugReports"]]
  )
}

external_link <- function(label, href) {
  shiny::tags$a(
    class = "dropdown-item",
    href = href,
    target = "_blank",
    rel = "noopener",
    label,
    shiny::icon("arrow-up-right-from-square", `aria-hidden` = "true"),
    shiny::tags$span(class = "visually-hidden", "(opens in a new tab)")
  )
}

help_menu <- function() {
  links <- help_links()
  bslib::nav_menu(
    "Help",
    align = "right",
    bslib::nav_panel("About NACHO", help_page("nacho")),
    bslib::nav_item(external_link("Documentation", links[["documentation"]])),
    bslib::nav_item(external_link("Ask a question", links[["discussions"]])),
    bslib::nav_item(external_link("Report a problem", links[["issues"]])),
    bslib::nav_item(
      shiny::actionLink("cite", "Cite NACHO", class = "dropdown-item")
    )
  )
}

citation_text <- function(bibtex = FALSE) {
  entry <- utils::citation("NACHO")
  if (bibtex) {
    paste(format(entry, style = "bibtex"), collapse = "\n")
  } else {
    paste(format(entry, style = "text"), collapse = "\n\n")
  }
}

tune_object <- function(object, chosen, thresholds) {
  tryCatch(
    rlang::with_options(
      normalise(
        object,
        normalisation_method = chosen$normalisation_method,
        ruv_k = chosen$ruv_k,
        background = chosen$background,
        background_mode = chosen$background_mode,
        outliers_thresholds = thresholds
      ),
      nacho.quiet = TRUE
    ),
    nacho_error = function(cnd) {
      shiny::validate(
        shiny::need(FALSE, cli::ansi_strip(rlang::cnd_message(cnd)))
      )
    }
  )
}

tune_with_toasts <- function(object, chosen, thresholds, announced) {
  key <- list(object@provenance, dim(object@counts), chosen)
  if (!identical(announced$key, key)) {
    announced$key <- key
    announced$messages <- character()
  }
  withCallingHandlers(
    tune_object(object, chosen, thresholds),
    nacho_warning_metric_unavailable = function(cnd) {
      invokeRestart("muffleWarning")
    },
    nacho_warning = function(cnd) {
      message <- cli::ansi_strip(rlang::cnd_message(cnd))
      if (!message %in% announced$messages) {
        announced$messages <- c(announced$messages, message)
        notify_user(message, "warning")
      }
      invokeRestart("muffleWarning")
    }
  )
}

app_server <- function(
  x,
  done = FALSE,
  interactive = FALSE,
  quarto = FALSE
) {
  function(input, output, session) {
    data <- mod_data_server("data", initial = x)
    settings <- mod_thresholds_server("thresholds", data = data)
    # The value must stay logical: text such as "FALSE" is truthy in JavaScript.
    output$has_data <- shiny::markRenderFunction(
      uiFunc = shiny::textOutput,
      renderFunc = function(shinysession, name, ...) !is.null(data())
    )
    shiny::outputOptions(output, "has_data", suspendWhenHidden = FALSE)
    dark <- shiny::reactive(identical(input$dark_mode, "dark"))
    announced <- new.env()
    tuned <- shiny::reactive(
      tune_with_toasts(
        shiny::req(data()),
        settings$settings(),
        settings$thresholds(),
        announced
      )
    )
    qc <- shiny::reactive(nacho_qc(tuned()))
    output$applicable <- shiny::renderText(
      paste0(
        ",",
        paste(applicable_plots(shiny::req(tuned())), collapse = ","),
        ","
      )
    )
    shiny::outputOptions(output, "applicable", suspendWhenHidden = FALSE)
    mod_overview_server("overview", tuned, qc)
    selected <- shiny::reactiveVal(character())
    mod_outliers_server("outliers", tuned, qc, selected)
    mod_batch_server("batch", tuned)
    mod_export_server("export", tuned, quarto)
    shiny::observeEvent(input$cite, {
      shiny::showModal(shiny::modalDialog(
        title = "Cite NACHO",
        shiny::p(citation_text()),
        shiny::tags$pre(citation_text(bibtex = TRUE)),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })
    lapply(unlist(app_plot_types, use.names = FALSE), function(type) {
      mod_qc_plot_server(
        type,
        object = tuned,
        qc = qc,
        type = type,
        dark = dark,
        selected = selected,
        interactive = interactive
      )
    })
    if (done) {
      finished <- new.env()
      observe_done(input, data, settings, announced, finished)
      session$onSessionEnded(function() {
        if (!isTRUE(finished$done)) shiny::stopApp(NULL)
      })
    }
  }
}

observe_done <- function(
  input,
  data,
  settings,
  announced = new.env(),
  finished = new.env()
) {
  shiny::observeEvent(input$done, {
    if (is.null(data())) {
      finished$done <- TRUE
      return(shiny::stopApp(NULL))
    }
    problem <- NULL
    result <- tryCatch(
      tune_with_toasts(
        data(),
        settings$settings(),
        settings$current_thresholds(),
        announced
      ),
      shiny.silent.error = function(cnd) {
        problem <<- conditionMessage(cnd)
        NULL
      }
    )
    if (is.null(result)) {
      notify_user(
        paste(
          problem,
          "Choose a normalisation method these data support before clicking Done."
        ),
        "warning"
      )
    } else {
      finished$done <- TRUE
      shiny::stopApp(result)
    }
  })
}
