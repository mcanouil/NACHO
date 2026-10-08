#' @include report.R
NULL

#' Plot types shown on each page of the app
#'
#' @noRd
app_plot_types <- list(
  qc_metrics = c("BD", "FoV", "PCL", "LoD"),
  controls = c("Positive", "Negative", "Housekeeping", "PN"),
  counts = c("ACBD", "ACMC", "PCA12", "PCAi", "PCA"),
  normalisation = c("PFNF", "HF", "NORM", "RLE", "Stability"),
  batch = c("BatchFactors", "PCBatch")
)

app_colourless <- c("Stability", "PCBatch")

app_flag_metrics <- c("BD", "FoV", "PCL", "LoD")

download_bounds <- c(min = 5, max = 50)

app_plot_titles <- c(
  BD = "Binding density",
  FoV = "Field of view",
  PCL = "Positive control linearity",
  LoD = "Limit of detection",
  Positive = "Positive controls",
  Negative = "Negative controls",
  Housekeeping = "Housekeeping genes",
  PN = "Control probe expression",
  ACBD = "Average count against binding density",
  ACMC = "Average count against median count",
  PCA12 = "First two principal components",
  PCAi = "Variance explained",
  PCA = "Planes of the first components",
  PFNF = "Positive against negative factor",
  HF = "Housekeeping factor",
  NORM = "Normalisation result",
  RLE = "Relative log expression",
  Stability = "Housekeeping gene stability",
  BatchFactors = "Normalisation factors by cartridge",
  PCBatch = "Principal components and batches"
)

#' Draw one plot for the app
#'
#' Unknown colour columns fall back to `CartridgeID`, and unknown label
#' columns to no labels.
#' Warnings about unavailable metrics are muffled, because the app shows an
#' empty plot for them.
#' Interactive plots add tooltips and selection with ggiraph, and only the app
#' asks for them: [autoplot()] always returns static plots.
#'
#' @noRd
app_plot <- function(x, type, options, dark, interactive = FALSE) {
  colour <- options[["colour"]]
  if (is.null(colour) || !colour %in% names(nacho_samples(x))) {
    colour <- "CartridgeID"
  }
  outliers_labels <- options[["outliers_labels"]]
  if (!isTRUE(outliers_labels %in% names(nacho_samples(x)))) {
    outliers_labels <- NULL
  }
  show_legend <- !isFALSE(options[["show_legend"]])
  withCallingHandlers(
    if (interactive) {
      nacho_plot_registry[[type]](
        object = x,
        type = type,
        colour = colour,
        size = options[["size"]] %||% 1,
        show_legend = show_legend,
        show_outliers = TRUE,
        outliers_factor = 1,
        outliers_labels = outliers_labels,
        dark = dark,
        interactive = TRUE
      )
    } else {
      autoplot(
        x,
        type = type,
        colour = colour,
        size = options[["size"]] %||% 1,
        show_legend = show_legend,
        outliers_labels = outliers_labels,
        dark = dark
      )
    },
    nacho_warning_metric_unavailable = function(cnd) {
      invokeRestart("muffleWarning")
    }
  )
}

girafe_selection <- function(selected = NULL) {
  ggiraph::opts_selection(
    type = "single",
    only_shiny = TRUE,
    css = "stroke:currentColor;stroke-width:3px;r:6px;",
    selected = selected
  )
}

#' Convert the pixel size of a card to the size of a plot in inches
#'
#' Sizes round to 50 pixels, so small changes in the layout do not redraw the
#' plot, and a return from full screen finds the plot in the cache.
#' In the grid the plot output has a fixed height of 350 pixels, so only the
#' width follows the card; in full screen the output fills the card.
#' Without a usable size, the default size is used.
#'
#' @noRd
card_size <- function(width, height) {
  default <- c(width = 7, height = 4.5)
  if (
    !is.numeric(width) ||
      !is.numeric(height) ||
      length(width) != 1 ||
      length(height) != 1 ||
      !is.finite(width) ||
      !is.finite(height) ||
      width <= 0 ||
      height <= 0
  ) {
    return(default)
  }
  rounded <- function(pixels) max(50, round(pixels / 50) * 50)
  c(width = rounded(width) / 96, height = rounded(height) / 96)
}

app_girafe <- function(plot, width = 7, height = 4.5) {
  ggiraph::girafe(
    ggobj = plot,
    width_svg = width,
    height_svg = height,
    options = list(
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_hover(css = "stroke:currentColor;stroke-width:2px;"),
      ggiraph::opts_tooltip(use_fill = FALSE),
      ggiraph::opts_toolbar(
        hidden = c(
          "lasso_select",
          "lasso_deselect",
          "zoom_onoff",
          "zoom_rect",
          "zoom_reset",
          "saveaspng"
        )
      )
    )
  )
}

#' Decide once whether the app draws interactive plots
#'
#' @noRd
plots_interactive <- function() {
  tryCatch(
    {
      rlang::check_installed(
        "ggiraph",
        reason = "for interactive plots in the NACHO app."
      )
      TRUE
    },
    rlib_error_package_not_found = function(cnd) {
      nacho_inform("ggiraph is not installed, so the app shows static plots.")
      FALSE
    }
  )
}

mod_qc_plot_ui <- function(id, type = id, interactive = FALSE) {
  ns <- shiny::NS(id)
  title <- app_plot_titles[[type]]
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      title,
      bslib::popover(
        shiny::tags$button(
          type = "button",
          class = "btn btn-sm btn-outline-secondary",
          `aria-label` = paste("Display options for", title),
          shiny::icon("sliders", `aria-hidden` = "true")
        ),
        title = "Display options",
        if (!type %in% app_colourless) {
          shiny::tagList(
            shiny::selectInput(
              ns("colour"),
              "Colour by",
              choices = "CartridgeID"
            ),
            shiny::checkboxInput(
              ns("show_legend"),
              "Show the legend",
              value = TRUE
            ),
            shiny::selectInput(
              ns("labels"),
              "Label flagged samples with",
              choices = c(None = "")
            )
          )
        },
        shiny::sliderInput(
          ns("size"),
          "Point size",
          min = 0.5,
          max = 4,
          value = 1,
          step = 0.5
        ),
        shiny::numericInput(
          ns("width"),
          "Download width (cm)",
          value = 16,
          min = download_bounds[["min"]],
          max = download_bounds[["max"]]
        ),
        shiny::numericInput(
          ns("height"),
          "Download height (cm)",
          value = 12,
          min = download_bounds[["min"]],
          max = download_bounds[["max"]]
        ),
        shiny::downloadButton(ns("download"), "Download PNG")
      )
    ),
    bslib::card_body(
      fillable = TRUE,
      if (interactive) {
        shiny::tags$div(
          class = "nacho-girafe",
          role = "img",
          `aria-label` = plot_alt_texts[[type]],
          ggiraph::girafeOutput(ns("girafe"), height = "100%")
        )
      } else {
        shiny::plotOutput(ns("plot"), height = "350px")
      }
    ),
    bslib::card_footer(shiny::textOutput(ns("summary")))
  )
}

plot_summary <- function(x, qc, type = NULL) {
  ids <- qc[[x@settings[["id_colname"]]]][qc[["status"]] %in% "fail"]
  cartridges <- length(unique(stats::na.omit(qc[["CartridgeID"]])))
  shown <- utils::head(ids, 3)
  overall <- paste0(
    nrow(qc),
    " samples",
    if (cartridges > 0) {
      paste0(" on ", cartridges, " cartridge", if (cartridges != 1) "s")
    },
    "; ",
    if (length(ids) == 0) {
      "none flagged."
    } else {
      paste0(
        length(ids),
        " flagged: ",
        paste(shown, collapse = ", "),
        if (length(ids) > length(shown)) {
          paste0(" and ", length(ids) - length(shown), " more")
        },
        "."
      )
    }
  )
  if (!isTRUE(type %in% app_flag_metrics)) {
    return(overall)
  }
  statuses <- qc[[paste0(type, "_status")]]
  if (all(is.na(statuses))) {
    return(paste0(overall, " ", type, " is not assessed for these data."))
  }
  on_metric <- sum(statuses %in% "fail")
  paste0(
    overall,
    " ",
    if (on_metric == 0) "None" else on_metric,
    " flagged on ",
    type,
    "."
  )
}

download_size <- function(value, default) {
  if (length(value) == 1 && is.finite(value)) {
    min(max(value, download_bounds[["min"]]), download_bounds[["max"]])
  } else {
    default
  }
}

# The server sets a selection on the plots and on the highlight menu.
# Each of them answers with an input event that repeats the value it just got.
# A plot that the server draws again also sends the selection it was built
# with, which can be older than the selection of the user.
# When two selections are in flight, the stale answers bounce between the
# server, the plots and the menu without end.
# Shiny handles one server message at a time, and a plot or the menu answers
# while Shiny handles the message that sets it.
# The guard keeps the values of the current message only, and drops an input
# event that repeats one of them.
# A drawn plot counts as a message that sets the selection it was built with.
# The server then sends the current selection to that plot again, in case the
# selection changed while the plot was built.
# The next message removes the values that got no answer, because a plot that
# is not drawn, or that already shows the value, does not answer.
# After a drop, Shiny forgets the last value it sent for that input, so the
# next user action with that value still reaches the server.
selection_echo_guard <- shiny::tags$script(shiny::HTML(
  "(function() {
    var expected = {};
    var normalise = function(value) {
      var empty = value === null || value === undefined || value === '';
      return JSON.stringify([].concat(empty ? [] : value).sort());
    };
    var expect = function(name, value) {
      expected[name] = (expected[name] || []).concat(normalise(value));
    };
    $(document).on('shiny:message', function(e) {
      var message = e.message || {};
      expected = {};
      Object.keys(message.values || {}).forEach(function(key) {
        var widget = message.values[key];
        var select = widget && widget.x && widget.x.settings &&
          widget.x.settings.select;
        if (/-girafe$/.test(key) && select && select.selected) {
          expect(key + '_selected', select.selected);
        }
      });
      Object.keys(message.custom || {}).forEach(function(key) {
        if (/-girafe_set$/.test(key)) {
          expect(key.replace(/_set$/, '_selected'), message.custom[key]);
        }
      });
      (message.inputMessages || []).forEach(function(input) {
        if (input.id === 'outliers-highlight' && 'value' in input.message) {
          expect(input.id, input.message.value);
        }
      });
    });
    $(document).on('shiny:inputchanged', function(e) {
      var values = expected[e.name];
      var at = values ? values.indexOf(normalise(e.value)) : -1;
      if (at < 0) return;
      values.splice(at, 1);
      e.preventDefault();
      Shiny.forgetLastInputValue(e.name);
    });
  })();"
))

send_selection <- function(session, output_id, value) {
  session$sendCustomMessage(paste0(session$ns(output_id), "_set"), value)
}

mod_qc_plot_server <- function(
  id,
  object,
  qc,
  type = id,
  dark,
  selected = shiny::reactiveVal(character()),
  interactive = FALSE
) {
  force(type)
  force(dark)
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(object(), {
      columns <- names(nacho_samples(object()))
      shiny::updateSelectInput(
        session,
        "colour",
        choices = columns,
        selected = if (isTRUE(input$colour %in% columns)) {
          input$colour
        } else {
          "CartridgeID"
        }
      )
      shiny::updateSelectInput(
        session,
        "labels",
        choices = c(None = "", columns),
        selected = if (isTRUE(input$labels %in% columns)) input$labels else ""
      )
    })
    options <- shiny::reactive(
      list(
        colour = input$colour,
        show_legend = input$show_legend,
        size = input$size,
        outliers_labels = if (nzchar(input$labels %||% "")) input$labels
      )
    )
    plot <- shiny::reactive({
      app_plot(shiny::req(object()), type, options(), dark(), interactive)
    })
    if (interactive) {
      shiny::observeEvent(
        input$girafe_selected,
        selected(as.character(input$girafe_selected %||% character())),
        ignoreNULL = FALSE
      )
      shiny::observeEvent(
        selected(),
        send_selection(session, "girafe", selected()),
        ignoreNULL = FALSE,
        ignoreInit = TRUE
      )
      card_pixels <- shiny::reactive({
        output_id <- paste0("output_", session$ns("girafe"))
        width <- session$clientData[[paste0(output_id, "_width")]]
        height <- session$clientData[[paste0(output_id, "_height")]]
        shiny::req(
          is.numeric(width) && length(width) == 1 && isTRUE(width > 0),
          is.numeric(height) && length(height) == 1 && isTRUE(height > 0)
        )
        c(width = width, height = height)
      }) |>
        shiny::debounce(250)
      girafe_size <- shiny::reactiveVal()
      shiny::observe({
        pixels <- card_pixels()
        size <- card_size(pixels[["width"]], pixels[["height"]])
        if (!identical(size, shiny::isolate(girafe_size()))) {
          girafe_size(size)
        }
      })
      widget <- shiny::reactive({
        size <- girafe_size()
        app_girafe(plot(), size[["width"]], size[["height"]])
      }) |>
        shiny::bindCache(
          object(),
          type,
          options(),
          dark(),
          shiny::req(girafe_size())
        )
      output$girafe <- ggiraph::renderGirafe({
        session$onFlushed(function() {
          send_selection(session, "girafe", shiny::isolate(selected()))
        })
        ggiraph::girafe_options(
          widget(),
          girafe_selection(
            shiny::isolate(if (length(selected()) > 0) selected())
          )
        )
      })
    } else {
      output$plot <- shiny::renderPlot(plot(), alt = plot_alt_texts[[type]]) |>
        shiny::bindCache(object(), type, options(), dark())
    }
    output$summary <- shiny::renderText(
      plot_summary(shiny::req(object()), shiny::req(qc()), type)
    )
    output$download <- shiny::downloadHandler(
      filename = function() paste0("nacho-", type, ".png"),
      content = function(file) {
        ggplot2::ggsave(
          file,
          plot(),
          width = download_size(input$width, 16),
          height = download_size(input$height, 12),
          units = "cm",
          dpi = 150,
          bg = plot_colours(dark())[["paper"]]
        )
      }
    )
    plot
  })
}
