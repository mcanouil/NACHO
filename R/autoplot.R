#' @include nacho-class.R
NULL

#' Plot the quality control of a nacho object
#'
#' Draws any of the quality-control figures of the Shiny app
#' ([visualise()]) and of the HTML report ([render()]).
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
#'   outliers_labels = NULL
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
#' * `colour`: The column of `nacho_samples(object)` that colours the points.
#' * `size`: The point size.
#' * `show_legend`: If `FALSE`, hide the colour legend.
#' * `show_outliers`: If `TRUE`, draw the flagged samples in red.
#' * `outliers_factor`: The size of the flagged samples, relative to `size`.
#' * `outliers_labels`: The column of `nacho_samples(object)` that labels the
#'   flagged samples, or `NULL` for no labels.
#'   Labels imply `show_outliers = TRUE`.
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
  nacho_plot_registry[[type]](
    object = object,
    type = type,
    colour = colour,
    size = size,
    show_legend = show_legend,
    show_outliers = show_outliers,
    outliers_factor = outliers_factor,
    outliers_labels = outliers_labels
  )
}

plot_samples <- function(object, colour) {
  samples <- data.table::as.data.table(nacho_samples(object))
  if (is.numeric(samples[[colour]])) {
    samples[[colour]] <- as.character(samples[[colour]])
  }
  samples
}

plot_probes <- function(object, rows, colour) {
  long <- long_table(object, rows = rows)
  if (is.numeric(long[[colour]])) {
    long[[colour]] <- as.character(long[[colour]])
  }
  long
}

outlier_layers <- function(
  show_outliers,
  colour,
  size,
  outliers_factor,
  outliers_labels,
  jitter
) {
  position <- if (jitter) {
    ggplot2::position_jitter(width = 0.25, height = 0)
  } else {
    "identity"
  }
  inliers <- ggplot2::geom_point(
    data = if (show_outliers) function(d) d[!d[["is_outlier"]] %in% TRUE, ],
    mapping = ggplot2::aes(colour = .data[[colour]]),
    size = size,
    na.rm = TRUE,
    position = position
  )
  if (!show_outliers) {
    return(list(inliers))
  }
  list(
    inliers,
    ggplot2::geom_point(
      data = function(d) d[d[["is_outlier"]] %in% TRUE, ],
      size = size * outliers_factor,
      colour = "#b22222",
      na.rm = TRUE,
      position = position
    ),
    if (!is.null(outliers_labels)) {
      ggrepel::geom_label_repel(
        data = function(d) d[d[["is_outlier"]] %in% TRUE, ],
        mapping = ggplot2::aes(label = .data[[outliers_labels]]),
        colour = "#b22222",
        na.rm = TRUE
      )
    }
  )
}

threshold_layers <- function(limits) {
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
      fill = "#b22222",
      alpha = 0.2,
      colour = "transparent",
      inherit.aes = FALSE
    ),
    ggplot2::geom_hline(
      data = data.frame(value = limits),
      mapping = ggplot2::aes(yintercept = .data[["value"]]),
      colour = "#b22222",
      linetype = "longdash"
    )
  )
}

not_available_plot <- function(x_label, y_label) {
  ggplot2::ggplot() +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = "Not available!",
      angle = 30,
      size = 24,
      colour = "#b22222",
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
  outliers_labels
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
    return(not_available_plot("CartridgeID", y_label))
  }

  ggplot2::ggplot(
    data = strip_plexset_suffix(plot_samples(object, colour), id)[
      j = unique(.SD),
      .SDcols = unique(c(
        "CartridgeID",
        colour,
        id,
        type,
        "is_outlier",
        outliers_labels
      ))
    ]
  ) +
    ggplot2::aes(
      x = .data[["CartridgeID"]],
      y = .data[[type]]
    ) +
    ggplot2::scale_colour_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
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
      jitter = TRUE
    ) +
    ggplot2::labs(
      x = "CartridgeID",
      y = y_label,
      colour = colour
    ) +
    threshold_layers(object@thresholds[[type]]) +
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
  outliers_labels
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
    return(not_available_plot("Gene Name", "Counts + 1"))
  }

  ggplot2::ggplot(
    data = strip_plexset_suffix(
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
        "is_outlier",
        outliers_labels
      ))
    ]
  ) +
    ggplot2::aes(
      x = .data[["Name"]],
      y = .data[["Count"]] + 1
    ) +
    ggplot2::scale_colour_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
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
      jitter = TRUE
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
  outliers_labels
) {
  id <- object@settings[["id_colname"]]
  ggplot2::ggplot(
    data = strip_plexset_suffix(
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
        "is_outlier"
      ))
    ]
  ) +
    ggplot2::aes(
      x = .data[[id]],
      y = .data[["Count"]] + 1,
      colour = .data[["Name"]],
      group = .data[["Name"]]
    ) +
    ggplot2::scale_colour_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
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
      mapping = ggplot2::aes(
        x = as.numeric(as.factor(.data[[id]])),
        linetype = "Loess",
        group = "CodeClass"
      ),
      colour = "black",
      se = TRUE,
      method = "loess",
      formula = y ~ x
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
  outliers_labels
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
        "is_outlier",
        outliers_labels
      ))
    ]
  ) +
    ggplot2::aes(
      x = .data[["MC"]],
      y = .data[["BD"]],
      colour = .data[[colour]]
    ) +
    ggplot2::scale_colour_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
    ) +
    outlier_layers(
      show_outliers,
      colour,
      size,
      outliers_factor,
      outliers_labels,
      jitter = FALSE
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
    threshold_layers(object@thresholds[["BD"]]) +
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
  outliers_labels
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
    ggplot2::aes(
      x = .data[["MC"]],
      y = .data[["MedC"]],
      colour = .data[[colour]]
    ) +
    ggplot2::scale_colour_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
    ) +
    ggplot2::geom_point(size = size, na.rm = TRUE) +
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
  outliers_labels
) {
  if (ncol(object@pca[["scores"]]) < 2) {
    warn_too_few_components(type)
    return(not_available_plot("PC01", "PC02"))
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
    ggplot2::aes(
      x = .data[["PC01"]],
      y = .data[["PC02"]],
      colour = .data[[colour]]
    ) +
    ggforce::geom_mark_ellipse(na.rm = TRUE, alpha = 0.1) +
    ggplot2::geom_point(size = size, na.rm = TRUE) +
    ggplot2::scale_colour_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
    ) +
    ggplot2::scale_fill_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
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
  outliers_labels
) {
  X.PC <- Y.PC <- NULL
  if (ncol(object@pca[["scores"]]) < 2) {
    warn_too_few_components(type)
    return(not_available_plot(NULL, NULL))
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
    ggplot2::aes(
      x = .data[["X"]],
      y = .data[["Y"]],
      colour = .data[[colour]],
      fill = .data[[colour]]
    ) +
    ggforce::geom_mark_ellipse(na.rm = TRUE, alpha = 0.1) +
    ggplot2::geom_point(size = size, na.rm = TRUE) +
    ggplot2::scale_colour_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
    ) +
    ggplot2::scale_fill_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
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
  outliers_labels
) {
  PoV <- `Proportion of Variance` <- NULL
  ggplot2::ggplot(
    data = data.table::as.data.table(
      object@pca[["importance"]]
    )[
      j = PoV := sprintf("%0.2f%%", `Proportion of Variance` * 100)
    ]
  ) +
    ggplot2::aes(x = .data[["PC"]], y = .data[["Proportion of Variance"]]) +
    ggplot2::scale_colour_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
    ) +
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
  outliers_labels
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
        "is_outlier",
        outliers_labels
      ))
    ]
  ) +
    ggplot2::aes(
      x = .data[["Negative_factor"]],
      y = .data[["Positive_factor"]],
      colour = .data[[colour]]
    ) +
    ggplot2::scale_colour_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
    ) +
    outlier_layers(
      show_outliers,
      colour,
      size,
      outliers_factor,
      outliers_labels,
      jitter = FALSE
    ) +
    ggplot2::labs(
      x = "Negative Factor",
      y = "Positive Factor",
      colour = colour
    ) +
    ggplot2::scale_y_continuous(transform = transform_log10_infinite()) +
    threshold_layers(object@thresholds[["Positive_factor"]]) +
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
  outliers_labels
) {
  id <- object@settings[["id_colname"]]
  if (!"House_factor" %in% names(object@samples)) {
    nacho_warn(
      "The housekeeping factor was not computed.",
      class = "metric_unavailable"
    )
    return(not_available_plot("Positive Factor", "Housekeeping Factor"))
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
        "is_outlier",
        outliers_labels
      ))
    ]
  ) +
    ggplot2::aes(
      x = .data[["Positive_factor"]],
      y = .data[["House_factor"]],
      colour = .data[[colour]]
    ) +
    ggplot2::scale_colour_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
    ) +
    outlier_layers(
      show_outliers,
      colour,
      size,
      outliers_factor,
      outliers_labels,
      jitter = FALSE
    ) +
    ggplot2::labs(
      x = "Positive Factor",
      y = "Housekeeping Factor",
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
      fill = "#b22222",
      alpha = 0.2,
      colour = "transparent",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      data = data.frame(value = object@thresholds[["House_factor"]]),
      mapping = ggplot2::aes(yintercept = .data[["value"]]),
      colour = "#b22222",
      linetype = "longdash"
    ) +
    ggplot2::geom_vline(
      data = data.frame(value = object@thresholds[["Positive_factor"]]),
      mapping = ggplot2::aes(xintercept = .data[["value"]]),
      colour = "#b22222",
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
  outliers_labels
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

  ggplot2::ggplot(
    data = plot_probes(object, rows, "CartridgeID")[
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
          "is_outlier"
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
        "is_outlier"
      ))
    ][
      j = `:=`(
        Status = factor(
          x = c("Count" = "Raw", "Count_Norm" = "Normalised")[Status],
          levels = c("Count" = "Raw", "Count_Norm" = "Normalised")
        ),
        Count = Count + 1
      )
    ]
  ) +
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
    ggplot2::scale_colour_viridis_d(
      option = "plasma",
      direction = 1,
      end = 0.85
    ) +
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
      mapping = ggplot2::aes(
        x = as.numeric(as.factor(.data[[id]])),
        linetype = "Loess"
      ),
      colour = "black",
      se = TRUE,
      method = "loess",
      formula = y ~ x
    ) +
    (if (!(show_legend && length(housekeeping_genes) <= 10)) {
      ggplot2::guides(colour = "none")
    })
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
  NORM = plot_norm
)

S7::method(autoplot, nacho) <- autoplot_nacho
