#' @include nacho-class.R
NULL

#' Plot the quality control of a nacho object
#'
#' Draws any of the quality-control figures of the Shiny app
#' ([visualise()]) and of the HTML or PDF report ([render()]).
#'
#' @section Usage:
#' ```r
#' autoplot(
#'   object,
#'   type,
#'   colour = "CartridgeID",
#'   size = 0.5,
#'   show_legend = TRUE,
#'   show_outliers = TRUE,
#'   outliers_factor = 1,
#'   outliers_labels = NULL,
#'   dark = FALSE
#' )
#' ```
#'
#' @section Arguments:
#' * `object`: A `nacho` object from [load_rcc()] or [normalise()].
#' * `type`: The plot to draw, one of:
#'   * `"BD"`: Binding Density.
#'   * `"FoV"`: Field of View (imaging).
#'   * `"PCL"`: Positive Control Linearity.
#'   * `"LoD"`: Limit of Detection.
#'   * `"Positive"`: Positive controls.
#'   * `"Negative"`: Negative controls.
#'   * `"Housekeeping"`: Housekeeping genes.
#'   * `"PN"`: Positive controls against negative controls.
#'   * `"ACBD"`: Average counts against binding density.
#'   * `"ACMC"`: Average counts against median counts.
#'   * `"PCA12"`: Principal component 1 against 2.
#'   * `"PCAi"`: Scree plot of the principal components.
#'   * `"PCA"`: Planes of the first principal components.
#'   * `"PFNF"`: Positive factor against negative factor.
#'   * `"HF"`: Housekeeping factor.
#'   * `"NORM"`: Normalisation factor.
#'   * `"Stability"`: geNorm stability of the housekeeping genes, see
#'     [housekeeping_stability()].
#'     The dashed line marks M = 1.5, the limit geNorm suggests for
#'     homogeneous samples.
#'     This plot ignores `colour`, `show_legend`, `show_outliers` and
#'     `outliers_labels`.
#'   * `"RLE"`: Relative log expression of the normalised endogenous genes.
#'     This plot ignores `size`, `show_outliers`, `outliers_factor` and
#'     `outliers_labels`.
#'   * `"BatchFactors"`: Normalisation factors by cartridge.
#'     This plot ignores `show_outliers`, `outliers_factor` and
#'     `outliers_labels`.
#'   * `"PCBatch"`: Share of each principal component explained by cartridge
#'     and date; see [batch_diagnostics()].
#'     Tiles with no value stay grey and unlabelled.
#'     This plot ignores `colour`, `size`, `show_legend`, `show_outliers`,
#'     `outliers_factor` and `outliers_labels`.
#' * `colour`: The column of `nacho_samples(object)` that colours the points.
#' * `size`: The point size, and the line width in the `"NORM"` plot.
#' * `show_legend`: If `FALSE`, hide the colour legend.
#' * `show_outliers`: If `TRUE`, draw the flagged samples as triangles in the
#'   accent colour: rust on a light background, amber on a dark one.
#' * `outliers_factor`: The size of the flagged samples, relative to `size`.
#' * `outliers_labels`: The column of `nacho_samples(object)` that labels the
#'   flagged samples, or `NULL` for no labels.
#'   Labels imply `show_outliers = TRUE`.
#' * `dark`: If `TRUE`, draw the plot for a dark background.
#'
#' @return A `ggplot` object.
#'
#' @name autoplot.nacho
#' @usage NULL
#' @importFrom ggplot2 .data
#'
#' @examples
#' autoplot(GSE74821, type = "BD")
NULL

metric_labels <- c(
  "BD" = "Binding Density",
  "FoV" = "Field of View",
  "PCL" = "Positive Control Linearity",
  "LoD" = "Limit of Detection"
)

autoplot_nacho <- function(
  object,
  type,
  colour = "CartridgeID",
  size = 0.5,
  show_legend = TRUE,
  show_outliers = TRUE,
  outliers_factor = 1,
  outliers_labels = NULL,
  dark = FALSE,
  ...
) {
  check_nacho(object)
  dots <- list(...)
  if ("x" %in% names(dots)) {
    nacho_abort(
      c(
        "{.arg x} was renamed {.arg type} in NACHO 3.0.0.",
        i = "Use {.code autoplot(object, type = {encodeString(dots[['x']], quote = '\"')})}."
      ),
      class = "bad_argument"
    )
  }
  if (missing(type)) {
    nacho_abort(
      c(
        "{.arg type} is missing.",
        i = "Choose one of {.val {names(nacho_plot_registry)}}."
      ),
      class = "bad_argument"
    )
  }
  type <- check_choice(type, names(nacho_plot_registry))
  check_string(colour)
  check_column(
    colour,
    nacho_samples(object),
    data_arg = "nacho_samples(object)"
  )
  check_string(outliers_labels, allow_null = TRUE)
  if (!is.null(outliers_labels)) {
    check_column(
      outliers_labels,
      nacho_samples(object),
      data_arg = "nacho_samples(object)"
    )
    show_outliers <- TRUE
  }
  check_bool(show_legend)
  check_bool(show_outliers)
  check_bool(dark)
  nacho_plot_registry[[type]](
    object = object,
    type = type,
    colour = colour,
    size = size,
    show_legend = show_legend,
    show_outliers = show_outliers,
    outliers_factor = outliers_factor,
    outliers_labels = outliers_labels,
    dark = dark
  )
}

plot_samples <- function(object, colour) {
  samples <- data.table::as.data.table(nacho_samples(object))
  if (is.numeric(samples[[colour]])) {
    samples[[colour]] <- as.character(samples[[colour]])
  }
  samples[["flagged"]] <- flagged_samples(object)
  samples
}

plot_probes <- function(object, rows, colour) {
  long <- long_table(object, rows = rows)
  if (is.numeric(long[[colour]])) {
    long[[colour]] <- as.character(long[[colour]])
  }
  long[["flagged"]] <- flagged_samples(object)[match(
    long[[object@settings[["id_colname"]]]],
    colnames(object@counts)
  )]
  long
}

lane_samples <- function(object, data, id) {
  if (object@rcc_type == "n8") strip_plexset_suffix(data, id) else data
}

tooltip_labels <- c(
  qc_metric_labels,
  MC = "Average counts",
  MedC = "Median counts",
  Count = "Count",
  Negative_factor = "Negative Factor"
)
tooltip_labels[c("Positive_factor", "House_factor")] <- c(
  "Positive Factor",
  "Housekeeping Factor"
)

hover_mapping <- function(id, y, label_column = NULL, selectable = TRUE) {
  esc <- htmltools::htmlEscape
  value <- function(d) trimws(formatC(d, digits = 4, format = "fg"))
  parts <- unlist(
    lapply(y, function(column) {
      label <- if (is.null(label_column)) {
        tooltip_label <- unname(tooltip_labels[column])
        esc(if (is.na(tooltip_label)) column else tooltip_label)
      } else {
        rlang::expr(esc(.data[[!!label_column]]))
      }
      list("\n", label, ": ", rlang::expr(value(.data[[!!column]])))
    }),
    recursive = FALSE
  )
  mapping <- ggplot2::aes(
    tooltip = paste0(esc(.data[[!!id]]), !!!parts)
  )
  if (selectable) {
    mapping <- utils::modifyList(mapping, ggplot2::aes(data_id = .data[[!!id]]))
  }
  mapping
}

point_layer <- function(
  mapping = NULL,
  interactive = FALSE,
  id,
  y,
  label_column = NULL,
  selectable = TRUE,
  ...
) {
  if (!interactive) {
    return(ggplot2::geom_point(mapping = mapping, ...))
  }
  hover <- hover_mapping(id, y, label_column, selectable)
  ggiraph::geom_point_interactive(
    mapping = if (is.null(mapping)) {
      hover
    } else {
      utils::modifyList(mapping, hover)
    },
    ...
  )
}

outlier_layers <- function(
  show_outliers,
  colour,
  size,
  outliers_factor,
  outliers_labels,
  jitter,
  dark,
  interactive = FALSE,
  id = NULL,
  y = NULL,
  selectable = TRUE
) {
  position <- if (jitter) {
    ggplot2::position_jitter(width = 0.25, height = 0)
  } else {
    "identity"
  }
  inliers <- point_layer(
    data = if (show_outliers) function(d) d[!d[["flagged"]] %in% TRUE, ],
    mapping = ggplot2::aes(colour = .data[[colour]]),
    interactive = interactive,
    id = id,
    y = y,
    selectable = selectable,
    size = size,
    na.rm = TRUE,
    position = position
  )
  if (!show_outliers) {
    return(list(inliers))
  }
  colours <- plot_colours(dark)
  accent <- colours[["accent"]]
  list(
    inliers,
    point_layer(
      data = function(d) d[d[["flagged"]] %in% TRUE, ],
      interactive = interactive,
      id = id,
      y = y,
      selectable = selectable,
      size = size * outliers_factor,
      shape = 17,
      colour = accent,
      na.rm = TRUE,
      position = position
    ),
    if (!is.null(outliers_labels)) {
      ggrepel::geom_label_repel(
        data = function(d) d[d[["flagged"]] %in% TRUE, ],
        mapping = ggplot2::aes(label = .data[[outliers_labels]]),
        colour = accent,
        fill = colours[["paper"]],
        na.rm = TRUE
      )
    }
  )
}

finite_values <- function(x) {
  x[is.finite(x)]
}

threshold_layers <- function(limits, dark) {
  accent <- plot_colours(dark)[["accent"]]
  list(
    ggplot2::geom_rect(
      data = data.frame(
        ymin = limits,
        ymax = c(-Inf, Inf)[seq_along(limits)]
      ),
      mapping = ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]]
      ),
      fill = accent,
      alpha = 0.2,
      colour = "transparent",
      inherit.aes = FALSE
    ),
    ggplot2::geom_hline(
      data = data.frame(value = finite_values(limits)),
      mapping = ggplot2::aes(yintercept = .data[["value"]]),
      colour = accent,
      linetype = "longdash"
    )
  )
}

not_available_plot <- function(x_label, y_label, dark) {
  ggplot2::ggplot() +
    theme_nacho(dark) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = "Not available!",
      angle = 30,
      size = 24,
      colour = plot_colours(dark)[["accent"]],
      alpha = 0.25
    ) +
    ggplot2::theme(axis.text = ggplot2::element_blank())
}

warn_too_few_components <- function(type) {
  nacho_warn(
    c(
      "{.val {type}} needs at least two principal components.",
      i = "Keep more probes or samples to compute them."
    ),
    class = "metric_unavailable"
  )
}

plot_metrics <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  id <- object@settings[["id_colname"]]
  units <- c(
    "BD" = '"(Optical features / ", mu, m^2, ")"',
    "FoV" = '"(% Counted)"',
    "PCL" = '"(R^2)"',
    "LoD" = '"(Z)"'
  )
  y_label <- parse(
    text = paste0(
      "atop(\"",
      metric_labels[type],
      "\", paste(",
      units[type],
      "))"
    )
  )

  if (object@rcc_type == "n8" && type %in% c("PCL", "LoD")) {
    nacho_warn(
      "{.val {type}} is not available for PlexSet (n8) RCC files.",
      class = "metric_unavailable"
    )
    return(not_available_plot("CartridgeID", y_label, dark))
  }

  ggplot2::ggplot(
    data = lane_samples(object, plot_samples(object, colour), id)[
      j = unique(.SD),
      .SDcols = unique(c(
        "CartridgeID",
        colour,
        id,
        type,
        "flagged",
        outliers_labels
      ))
    ]
  ) +
    theme_nacho(dark) +
    ggplot2::aes(
      x = .data[["CartridgeID"]],
      y = .data[[type]]
    ) +
    ggplot2::geom_boxplot(
      mapping = ggplot2::aes(group = .data[["CartridgeID"]]),
      fill = NA,
      outliers = FALSE,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    outlier_layers(
      show_outliers,
      colour,
      size,
      outliers_factor,
      outliers_labels,
      jitter = TRUE,
      dark = dark,
      interactive = interactive,
      id = id,
      y = type,
      selectable = object@rcc_type != "n8"
    ) +
    ggplot2::labs(
      x = "CartridgeID",
      y = y_label,
      colour = colour
    ) +
    threshold_layers(object@thresholds[[type]], dark) +
    (if (!show_legend) ggplot2::guides(colour = "none")) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1)
    )
}

plot_cg <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  id <- object@settings[["id_colname"]]
  housekeeping_genes <- object@probes[["Name"]][object@probes[[
    "is_housekeeping"
  ]]]
  if (length(housekeeping_genes) == 0 && type %in% "Housekeeping") {
    nacho_warn(
      "No housekeeping genes are available.",
      class = "metric_unavailable"
    )
    return(not_available_plot("Gene Name", "Counts + 1", dark))
  }

  ggplot2::ggplot(
    data = lane_samples(
      object,
      plot_probes(
        object,
        which(object@probes[["CodeClass"]] %in% type),
        colour
      ),
      id
    )[
      j = unique(.SD),
      .SDcols = unique(c(
        "CartridgeID",
        colour,
        id,
        "Name",
        "Count",
        "flagged",
        outliers_labels
      ))
    ]
  ) +
    theme_nacho(dark) +
    ggplot2::aes(
      x = .data[["Name"]],
      y = .data[["Count"]] + 1
    ) +
    ggplot2::geom_boxplot(
      mapping = ggplot2::aes(group = .data[["Name"]]),
      fill = NA,
      outliers = FALSE,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    outlier_layers(
      show_outliers,
      colour,
      size,
      outliers_factor,
      outliers_labels,
      jitter = TRUE,
      dark = dark,
      interactive = interactive,
      id = id,
      y = "Count",
      selectable = object@rcc_type != "n8"
    ) +
    ggplot2::scale_y_log10(
      labels = function(x) format(x, big.mark = ",")
    ) +
    ggplot2::labs(
      x = if (type %in% c("Negative", "Positive")) {
        "Control Name"
      } else {
        "Gene Name"
      },
      y = "Counts + 1",
      colour = colour
    ) +
    (if (!show_legend) ggplot2::guides(colour = "none")) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        face = "italic",
        angle = 30,
        hjust = 1,
        vjust = 1
      )
    )
}

plot_pn <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  id <- object@settings[["id_colname"]]
  data <- lane_samples(
    object,
    plot_probes(
      object,
      which(object@probes[["CodeClass"]] %in% c("Positive", "Negative")),
      colour
    ),
    id
  )[
    j = unique(.SD),
    .SDcols = unique(c(
      "CartridgeID",
      colour,
      id,
      "CodeClass",
      "Name",
      "Count",
      "flagged"
    ))
  ]

  ggplot2::ggplot(data = data) +
    theme_nacho(dark) +
    ggplot2::aes(
      x = .data[[id]],
      y = .data[["Count"]] + 1,
      colour = .data[["Name"]],
      group = .data[["Name"]]
    ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(facets = "CodeClass", scales = "free_y", ncol = 2) +
    ggplot2::scale_y_log10(
      labels = function(x) format(x, big.mark = ",")
    ) +
    ggplot2::scale_x_discrete(labels = NULL) +
    ggplot2::labs(
      x = "Sample Index",
      y = "Counts + 1",
      colour = "Control",
      linetype = "Smooth"
    ) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::geom_smooth(
      data = sample_trend(
        data.frame(
          x = as.numeric(as.factor(data[[id]])),
          y = data[["Count"]] + 1,
          CodeClass = data[["CodeClass"]]
        ),
        by = "CodeClass"
      ),
      mapping = ggplot2::aes(
        x = .data[["x"]],
        y = .data[["y"]],
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]],
        linetype = "Loess"
      ),
      stat = "identity",
      colour = plot_colours(dark)[["ink"]],
      inherit.aes = FALSE
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2)) +
    (if (!show_legend) ggplot2::guides(colour = "none"))
}

plot_acbd <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  id <- object@settings[["id_colname"]]
  ggplot2::ggplot(
    data = plot_samples(object, colour)[
      j = unique(.SD),
      .SDcols = unique(c(
        "CartridgeID",
        colour,
        id,
        "MC",
        "BD",
        "flagged",
        outliers_labels
      ))
    ]
  ) +
    theme_nacho(dark) +
    ggplot2::aes(
      x = .data[["MC"]],
      y = .data[["BD"]],
      colour = .data[[colour]]
    ) +
    outlier_layers(
      show_outliers,
      colour,
      size,
      outliers_factor,
      outliers_labels,
      jitter = FALSE,
      dark = dark,
      interactive = interactive,
      id = id,
      y = c("MC", "BD")
    ) +
    ggplot2::scale_x_continuous(labels = function(x) {
      format(x, big.mark = ",")
    }) +
    ggplot2::labs(
      x = "Average Counts",
      y = parse(
        text = 'atop("Binding Density", paste("(Optical features / ", mu, m^2, ")"))'
      ),
      colour = colour
    ) +
    threshold_layers(object@thresholds[["BD"]], dark) +
    (if (!show_legend) ggplot2::guides(colour = "none"))
}

plot_acmc <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  id <- object@settings[["id_colname"]]
  ggplot2::ggplot(
    data = plot_samples(object, colour)[
      j = unique(.SD),
      .SDcols = unique(c(
        "CartridgeID",
        colour,
        id,
        "MC",
        "MedC"
      ))
    ]
  ) +
    theme_nacho(dark) +
    ggplot2::aes(
      x = .data[["MC"]],
      y = .data[["MedC"]],
      colour = .data[[colour]]
    ) +
    point_layer(
      interactive = interactive,
      id = id,
      y = c("MC", "MedC"),
      size = size,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(labels = function(x) {
      format(x, big.mark = ",")
    }) +
    ggplot2::scale_y_continuous(labels = function(x) {
      format(x, big.mark = ",")
    }) +
    ggplot2::labs(
      x = "Average Counts",
      y = "Median Counts",
      colour = colour
    ) +
    (if (!show_legend) ggplot2::guides(colour = "none"))
}

plot_pca12 <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  if (ncol(object@pca[["scores"]]) < 2) {
    warn_too_few_components(type)
    return(not_available_plot("PC01", "PC02", dark))
  }
  id <- object@settings[["id_colname"]]
  ggplot2::ggplot(
    data = plot_samples(object, colour)[
      j = unique(.SD),
      .SDcols = unique(c(
        "CartridgeID",
        colour,
        id,
        "PC01",
        "PC02"
      ))
    ]
  ) +
    theme_nacho(dark) +
    ggplot2::aes(
      x = .data[["PC01"]],
      y = .data[["PC02"]],
      colour = .data[[colour]]
    ) +
    ggforce::geom_mark_ellipse(na.rm = TRUE, alpha = 0.1) +
    point_layer(
      interactive = interactive,
      id = id,
      y = c("PC01", "PC02"),
      size = size,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0.25)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.25)) +
    ggplot2::labs(x = "PC01", y = "PC02", colour = colour) +
    (if (!show_legend) ggplot2::guides(colour = "none"))
}

plot_pca <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  X.PC <- Y.PC <- NULL
  if (ncol(object@pca[["scores"]]) < 2) {
    warn_too_few_components(type)
    return(not_available_plot(NULL, NULL, dark))
  }
  id <- object@settings[["id_colname"]]
  components <- sprintf("PC%02d", seq_len(min(ncol(object@pca[["scores"]]), 5)))
  keys <- unique(c("CartridgeID", id, colour))
  ggplot2::ggplot(
    data = plot_samples(object, colour)[
      j = merge(
        x = data.table::melt(
          data = unique(.SD),
          id.vars = keys,
          measure.vars = components,
          variable.name = "X.PC",
          value.name = "X"
        ),
        y = data.table::melt(
          data = unique(.SD),
          id.vars = keys,
          measure.vars = components,
          variable.name = "Y.PC",
          value.name = "Y"
        ),
        by = keys,
        allow.cartesian = TRUE
      ),
      .SDcols = unique(c("CartridgeID", colour, id, components))
    ][
      as.numeric(sub("PC", "", X.PC)) < as.numeric(sub("PC", "", Y.PC))
    ]
  ) +
    theme_nacho(dark) +
    ggplot2::aes(
      x = .data[["X"]],
      y = .data[["Y"]],
      colour = .data[[colour]],
      fill = .data[[colour]]
    ) +
    ggforce::geom_mark_ellipse(na.rm = TRUE, alpha = 0.1) +
    point_layer(
      interactive = interactive,
      id = id,
      y = "Y",
      label_column = "Y.PC",
      size = size,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0.25)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.25)) +
    ggplot2::labs(x = NULL, y = NULL, colour = colour, fill = colour) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["Y.PC"]]),
      cols = ggplot2::vars(.data[["X.PC"]]),
      scales = "free"
    ) +
    (if (!show_legend) ggplot2::guides(colour = "none"))
}

plot_pcai <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  PoV <- `Proportion of Variance` <- NULL
  ggplot2::ggplot(
    data = data.table::as.data.table(
      object@pca[["importance"]]
    )[
      j = PoV := sprintf("%0.2f%%", `Proportion of Variance` * 100)
    ]
  ) +
    theme_nacho(dark) +
    ggplot2::aes(x = .data[["PC"]], y = .data[["Proportion of Variance"]]) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = .data[["PoV"]]),
      vjust = -1,
      show.legend = FALSE
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) sprintf("%0.2f%%", x * 100),
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(x = "Principal Components", y = "Proportion of Variance")
}

plot_pfnf <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  id <- object@settings[["id_colname"]]
  ggplot2::ggplot(
    data = plot_samples(object, colour)[
      j = unique(.SD),
      .SDcols = unique(c(
        "CartridgeID",
        colour,
        id,
        "Negative_factor",
        "Positive_factor",
        "flagged",
        outliers_labels
      ))
    ]
  ) +
    theme_nacho(dark) +
    ggplot2::aes(
      x = .data[["Negative_factor"]],
      y = .data[["Positive_factor"]],
      colour = .data[[colour]]
    ) +
    outlier_layers(
      show_outliers,
      colour,
      size,
      outliers_factor,
      outliers_labels,
      jitter = FALSE,
      dark = dark,
      interactive = interactive,
      id = id,
      y = c("Negative_factor", "Positive_factor")
    ) +
    ggplot2::labs(
      x = tooltip_labels[["Negative_factor"]],
      y = tooltip_labels[["Positive_factor"]],
      colour = colour
    ) +
    ggplot2::scale_y_continuous(transform = transform_log10_infinite()) +
    threshold_layers(object@thresholds[["Positive_factor"]], dark) +
    (if (!show_legend) ggplot2::guides(colour = "none"))
}

plot_hf <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  id <- object@settings[["id_colname"]]
  if (!"House_factor" %in% names(object@samples)) {
    nacho_warn(
      "The housekeeping factor was not computed.",
      class = "metric_unavailable"
    )
    return(not_available_plot(
      tooltip_labels[["Positive_factor"]],
      tooltip_labels[["House_factor"]],
      dark
    ))
  }

  ggplot2::ggplot(
    data = plot_samples(object, colour)[
      j = unique(.SD),
      .SDcols = unique(c(
        "CartridgeID",
        colour,
        id,
        "House_factor",
        "Positive_factor",
        "flagged",
        outliers_labels
      ))
    ]
  ) +
    theme_nacho(dark) +
    ggplot2::aes(
      x = .data[["Positive_factor"]],
      y = .data[["House_factor"]],
      colour = .data[[colour]]
    ) +
    outlier_layers(
      show_outliers,
      colour,
      size,
      outliers_factor,
      outliers_labels,
      jitter = FALSE,
      dark = dark,
      interactive = interactive,
      id = id,
      y = c("Positive_factor", "House_factor")
    ) +
    ggplot2::labs(
      x = tooltip_labels[["Positive_factor"]],
      y = tooltip_labels[["House_factor"]],
      colour = colour
    ) +
    ggplot2::scale_x_continuous(transform = transform_log10_infinite()) +
    ggplot2::scale_y_continuous(transform = transform_log10_infinite()) +
    ggplot2::geom_rect(
      data = data.frame(
        xmin = c(-Inf, -Inf, object@thresholds[["Positive_factor"]]),
        xmax = c(Inf, Inf, -Inf, Inf),
        ymin = c(object@thresholds[["House_factor"]], -Inf, -Inf),
        ymax = c(-Inf, Inf, Inf, Inf)
      ),
      mapping = ggplot2::aes(
        xmin = .data[["xmin"]],
        xmax = .data[["xmax"]],
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]]
      ),
      fill = plot_colours(dark)[["accent"]],
      alpha = 0.2,
      colour = "transparent",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      data = data.frame(
        value = finite_values(object@thresholds[["House_factor"]])
      ),
      mapping = ggplot2::aes(yintercept = .data[["value"]]),
      colour = plot_colours(dark)[["accent"]],
      linetype = "longdash"
    ) +
    ggplot2::geom_vline(
      data = data.frame(
        value = finite_values(object@thresholds[["Positive_factor"]])
      ),
      mapping = ggplot2::aes(xintercept = .data[["value"]]),
      colour = plot_colours(dark)[["accent"]],
      linetype = "longdash"
    ) +
    (if (!show_legend) ggplot2::guides(colour = "none"))
}

plot_norm <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  Status <- Count <- NULL
  id <- object@settings[["id_colname"]]
  housekeeping_genes <- object@probes[["Name"]][object@probes[[
    "is_housekeeping"
  ]]]
  rows <- if (length(housekeeping_genes) == 0) {
    which(object@probes[["CodeClass"]] == "Positive")
  } else {
    which(object@probes[["is_housekeeping"]])
  }

  data <- plot_probes(object, rows, "CartridgeID")[
    j = c("Count", "Count_Norm") := lapply(.SD, as.double),
    .SDcols = c("Count", "Count_Norm")
  ][
    j = data.table::melt(
      data = unique(.SD),
      id.vars = unique(c(
        "CartridgeID",
        id,
        "Name",
        "CodeClass",
        "flagged"
      )),
      measure.vars = c("Count", "Count_Norm"),
      variable.name = "Status",
      value.name = "Count"
    ),
    .SDcols = unique(c(
      "CartridgeID",
      id,
      "Count",
      "Count_Norm",
      "Name",
      "CodeClass",
      "flagged"
    ))
  ][
    j = `:=`(
      Status = factor(
        x = c("Count" = "Raw", "Count_Norm" = "Normalized")[Status],
        levels = c("Count" = "Raw", "Count_Norm" = "Normalized")
      ),
      Count = Count + 1
    )
  ]
  ggplot2::ggplot(data = data) +
    theme_nacho(dark) +
    ggplot2::aes(
      x = .data[[id]],
      y = .data[["Count"]]
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(colour = .data[["Name"]], group = .data[["Name"]]),
      linewidth = size,
      na.rm = TRUE
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data[["Status"]])) +
    ggplot2::scale_x_discrete(label = NULL) +
    ggplot2::scale_y_log10(
      labels = function(x) format(x, big.mark = ",")
    ) +
    ggplot2::labs(
      x = "Sample Index",
      y = "Counts + 1",
      colour = if (length(housekeeping_genes) == 0) {
        "Positive Control"
      } else {
        "Housekeeping Genes"
      },
      linetype = "Smooth"
    ) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::geom_smooth(
      data = sample_trend(
        data.frame(
          x = as.numeric(as.factor(data[[id]])),
          y = data[["Count"]],
          Status = data[["Status"]]
        ),
        by = "Status"
      ),
      mapping = ggplot2::aes(
        x = .data[["x"]],
        y = .data[["y"]],
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]],
        linetype = "Loess"
      ),
      stat = "identity",
      colour = plot_colours(dark)[["ink"]],
      inherit.aes = FALSE
    ) +
    (if (!(show_legend && length(housekeeping_genes) <= 10)) {
      ggplot2::guides(colour = "none")
    })
}

plot_stability <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  explain <- function(cnd) rlang::cnd_message(cnd)
  ranking <- rlang::try_fetch(
    housekeeping_stability(object)[["ranking"]],
    nacho_error_bad_argument = explain,
    nacho_error_no_detection_rate = explain
  )
  if (is.character(ranking)) {
    nacho_warn(
      c(
        "Stability cannot be drawn.",
        x = "{ranking}"
      ),
      class = "metric_unavailable"
    )
    return(not_available_plot("Housekeeping gene", "geNorm M", dark))
  }
  ranking[["Name"]] <- factor(ranking[["Name"]], levels = ranking[["Name"]])
  ggplot2::ggplot(ranking) +
    theme_nacho(dark) +
    ggplot2::aes(x = .data[["Name"]], y = .data[["geNorm_M"]]) +
    ggplot2::geom_point(size = size * 4) +
    ggplot2::geom_hline(
      yintercept = 1.5,
      colour = plot_colours(dark)[["accent"]],
      linetype = "longdash"
    ) +
    ggplot2::labs(x = "Housekeeping gene, most stable first", y = "geNorm M") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1)
    )
}

#' Box statistics of each column, with the rule of `stat_boxplot()`
#'
#' The whiskers reach the most extreme value within 1.5 times the
#' interquartile range of the box.
#' A plot of these boxes draws hundreds of samples much faster than
#' `geom_boxplot()`, which summarises every value.
#'
#' @param values A numeric matrix with column names, one column per box.
#'
#' @return A data frame with one row per column: `sample`, `ymin`, `lower`,
#'   `middle`, `upper` and `ymax`.
#'
#' @noRd
box_stats <- function(values) {
  stats <- apply(values, 2, function(v) {
    v <- v[!is.na(v)]
    if (length(v) == 0) {
      return(rep(NA_real_, 5))
    }
    q <- stats::quantile(v, c(0.25, 0.5, 0.75), names = FALSE)
    iqr <- q[3] - q[1]
    inside <- v[v >= q[1] - 1.5 * iqr & v <= q[3] + 1.5 * iqr]
    c(min(c(q, inside)), q, max(c(q, inside)))
  })
  data.frame(
    sample = colnames(values),
    ymin = stats[1, ],
    lower = stats[2, ],
    middle = stats[3, ],
    upper = stats[4, ],
    ymax = stats[5, ],
    row.names = NULL
  )
}

#' Loess trend of the sample means
#'
#' `geom_smooth()` fits a loess on every point, which takes seconds when a
#' study has hundreds of samples.
#' This fit uses the mean of each sample on the log10 scale, weighted by the
#' number of points of the sample.
#' When every sample has the same number of points, the curve is the same as
#' the curve of `geom_smooth()`.
#' The band is the 95% confidence interval of the trend of the sample means.
#' A loess needs six samples, so a panel with fewer has no trend.
#'
#' @param data A data frame with a numeric `x` (the position of the sample),
#'   a positive `y` (the count) and the columns named in `by`.
#' @param by The names of the columns that split the data into panels.
#' @param n The number of points of each curve.
#'
#' @return A data frame with the columns in `by`, then `x`, `y`, `ymin` and
#'   `ymax`, with `y`, `ymin` and `ymax` on the count scale.
#'
#' @noRd
sample_trend <- function(data, by, n = 80L) {
  keep <- is.finite(data[["x"]]) & is.finite(data[["y"]]) & data[["y"]] > 0
  data <- data[keep, , drop = FALSE]
  empty <- cbind(
    data[0, by, drop = FALSE],
    data.frame(x = numeric(), y = numeric(), ymin = numeric(), ymax = numeric())
  )
  panels <- split(data, data[by], drop = TRUE)
  trends <- lapply(panels, function(panel) {
    log_y <- log10(panel[["y"]])
    means <- tapply(log_y, panel[["x"]], mean)
    if (length(means) < 6L) {
      return(NULL)
    }
    points <- data.frame(
      x = as.numeric(names(means)),
      y = as.vector(means),
      w = as.vector(tapply(log_y, panel[["x"]], length))
    )
    fit <- stats::loess(y ~ x, data = points, weights = points[["w"]])
    grid <- data.frame(
      x = seq(min(points[["x"]]), max(points[["x"]]), length.out = n)
    )
    predicted <- stats::predict(fit, grid, se = TRUE)
    half <- predicted[["se.fit"]] * stats::qt(0.975, predicted[["df"]])
    trend <- data.frame(
      x = grid[["x"]],
      y = 10^predicted[["fit"]],
      ymin = 10^(predicted[["fit"]] - half),
      ymax = 10^(predicted[["fit"]] + half)
    )
    cbind(panel[rep(1L, n), by, drop = FALSE], trend, row.names = NULL)
  })
  trends <- trends[!vapply(trends, is.null, logical(1))]
  if (length(trends) == 0) {
    return(empty)
  }
  do.call(rbind, trends)
}

plot_rle <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  id <- object@settings[["id_colname"]]
  rows <- grepl("Endogenous", object@probes[["CodeClass"]])
  values <- log2(object@normalised[rows, , drop = FALSE] + 1)
  rle <- rle_centre(values, 1)
  samples <- plot_samples(object, colour)
  boxes <- box_stats(rle)
  boxes[[colour]] <- samples[[colour]][match(boxes[["sample"]], samples[[id]])]
  boxes[["sample"]] <- factor(
    boxes[["sample"]],
    levels = samples[[id]][order(nacho_samples(object)[[colour]])]
  )
  ggplot2::ggplot(boxes) +
    theme_nacho(dark) +
    ggplot2::aes(x = .data[["sample"]], colour = .data[[colour]]) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = plot_colours(dark)[["accent"]],
      linetype = "longdash"
    ) +
    ggplot2::geom_linerange(
      mapping = ggplot2::aes(
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]]
      ),
      na.rm = TRUE
    ) +
    ggplot2::geom_crossbar(
      mapping = ggplot2::aes(
        y = .data[["middle"]],
        ymin = .data[["lower"]],
        ymax = .data[["upper"]]
      ),
      fill = plot_colours(dark)[["paper"]],
      width = 0.9,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      x = "Sample",
      y = "Relative log expression",
      colour = colour
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    (if (!show_legend) ggplot2::guides(colour = "none"))
}

plot_batch_factors <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  samples <- plot_samples(object, colour)
  labels <- c(
    Positive_factor = "Positive",
    Negative_factor = "Negative",
    House_factor = "Housekeeping"
  )
  factors <- intersect(names(labels), names(samples))
  data <- do.call(
    rbind,
    lapply(factors, function(f) {
      data.frame(
        CartridgeID = samples[["CartridgeID"]],
        colour = samples[[colour]],
        factor = labels[[f]],
        value = samples[[f]]
      )
    })
  )
  data[["factor"]] <- factor(data[["factor"]], levels = labels[factors])
  ggplot2::ggplot(data) +
    theme_nacho(dark) +
    ggplot2::aes(x = .data[["CartridgeID"]], y = .data[["value"]]) +
    ggplot2::geom_boxplot(outliers = FALSE, na.rm = TRUE) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(colour = .data[["colour"]]),
      size = size,
      position = ggplot2::position_jitter(width = 0.25, height = 0),
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["factor"]]), scales = "free_y") +
    ggplot2::labs(x = "CartridgeID", y = "Factor", colour = colour) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1),
      legend.position = "bottom"
    ) +
    (if (!show_legend) ggplot2::guides(colour = "none"))
}

plot_pc_batch <- function(
  object,
  type,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels,
  dark,
  interactive = FALSE
) {
  batch <- intersect(c("CartridgeID", "Date"), names(nacho_samples(object)))
  data <- if (length(batch) > 0) {
    batch_diagnostics(object, batch = batch)[["pc_batch"]]
  } else {
    data.frame(PC = character(), batch = character(), r_squared = numeric())
  }
  if (nrow(data) == 0) {
    warn_too_few_components(type)
    return(not_available_plot("Batch", "Principal component", dark))
  }
  ggplot2::ggplot(data) +
    theme_nacho(dark) +
    ggplot2::aes(
      x = .data[["batch"]],
      y = .data[["PC"]],
      fill = .data[["r_squared"]]
    ) +
    ggplot2::geom_tile(colour = plot_colours(dark)[["paper"]]) +
    ggplot2::geom_label(
      data = function(d) d[!is.na(d[["r_squared"]]), ],
      mapping = ggplot2::aes(label = sprintf("%.2f", .data[["r_squared"]])),
      fill = plot_colours(dark)[["paper"]],
      colour = plot_colours(dark)[["ink"]],
      linewidth = 0,
      label.padding = ggplot2::unit(0.15, "lines")
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "plasma",
      limits = c(0, 1),
      na.value = grDevices::adjustcolor(
        plot_colours(dark)[["ink"]],
        alpha.f = 0.15
      )
    ) +
    ggplot2::labs(x = "Batch", y = "Principal component", fill = "R\u00b2")
}

nacho_plot_registry <- list(
  BD = plot_metrics,
  FoV = plot_metrics,
  PCL = plot_metrics,
  LoD = plot_metrics,
  Positive = plot_cg,
  Negative = plot_cg,
  Housekeeping = plot_cg,
  PN = plot_pn,
  ACBD = plot_acbd,
  ACMC = plot_acmc,
  PCA12 = plot_pca12,
  PCAi = plot_pcai,
  PCA = plot_pca,
  PFNF = plot_pfnf,
  HF = plot_hf,
  NORM = plot_norm,
  Stability = plot_stability,
  RLE = plot_rle,
  BatchFactors = plot_batch_factors,
  PCBatch = plot_pc_batch
)

S7::method(autoplot, nacho) <- autoplot_nacho

#' Point NACHO 2 objects to the converters
#'
#' NACHO 2 objects are S3 lists of class `nacho`, so without this method
#' ggplot2 would answer them with its generic error.
#'
#' @param object A NACHO 2 object.
#' @param ... Ignored.
#'
#' @noRd
#' @exportS3Method ggplot2::autoplot
autoplot.nacho <- function(object, ...) {
  abort_nacho_v2(object, arg = rlang::caller_arg(object))
}
