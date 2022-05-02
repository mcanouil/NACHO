#' Plot quality-control metrics and thresholds of a "nacho" object
#'
#' This function allows to plot any qualit-control figures available
#' within the shiny app using [`visualise()`] or in the HTML report from [`render()`].
#'
#' @inheritParams render
#' @param object [[list]] List obtained from [`load_rcc()`] or [`normalise()`].
#' @param x [[character]] Character string naming the quality-control metrics to plot from `nacho_object`.
#'  The possible values are:
#'
#'    * `"BD"` (Binding Density)
#'    * `"FoV"` (Imaging)
#'    * `"PCL"` (Positive Control Linearity)
#'    * `"LoD"` (Limit of Detection)
#'    * `"Positive"` (Positive Controls)
#'    * `"Negative"` (Negative Controls)
#'    * `"Housekeeping"` (Housekeeping Genes)
#'    * `"PN"` (Positive Controls vs. Negative Controls)
#'    * `"ACBD"` (Average Counts vs. Binding Density)
#'    * `"ACMC"` (Average Counts vs. Median Counts)
#'    * `"PCA12"` (Principal Component 1 vs. 2)
#'    * `"PCAi"` (Principal Component scree plot)
#'    * `"PCA"` (Principal Components planes)
#'    * `"PFNF"` (Positive Factor vs. Negative Factor)
#'    * `"HF"` (Housekeeping Factor)
#'    * `"NORM"` (Normalisation Factor)
#'
#' @param ... Other arguments (Not used).
#'
#' @return NULL
#' @export
#'
#' @examples
#'
#' data(GSE74821)
#'
#' autoplot(GSE74821, x = "BD")
#'
autoplot.nacho <- function(
  object, x,
  colour = "CartridgeID",
  size = 0.5,
  show_legend = TRUE,
  show_outliers = TRUE,
  outliers_factor = 1,
  outliers_labels = NULL,
  ...
) {
  if (missing(object)) {
    stop(
      '[NACHO] "object" is missing, results from "load_rcc()" and/or "normalise()" is mandatory!'
    )
  }
  if (missing(x) | is.null(x)) {
    stop(
      paste(
        '[NACHO] "x" is missing. It must be one of the following possible values:',
        '  * "BD", "FoV", "PCL", "LoD"',
        '  * "Positive", "Negative", "Housekeeping", "PN"',
        '  * "ACBD", "ACMC"',
        '  * "PCA12", "PCAi", "PCA"',
        '  * "PFB", "HF", "NORM"',
        sep = "\n"
      )
    )
  }
  object <- check_outliers(object)

  if (!is.null(outliers_labels)) show_outliers <- TRUE

  # if (attr(object, "RCC_type") == "n8" & x %in% c("PCL", "LoD")) {
  #   stop('[NACHO] "PCL" and "LoD" are not available for the provided NanoString dataset.')
  # }
  switch(
    EXPR = x,
    "BD" = plot_metrics(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend,
      show_outliers,
      outliers_factor,
      outliers_labels
    ),
    "FoV" = plot_metrics(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend,
      show_outliers,
      outliers_factor,
      outliers_labels
    ),
    "PCL" = plot_metrics(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend,
      show_outliers,
      outliers_factor,
      outliers_labels
    ),
    "LoD" = plot_metrics(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend,
      show_outliers,
      outliers_factor,
      outliers_labels
    ),
    "Positive" = plot_cg(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend,
      show_outliers,
      outliers_factor,
      outliers_labels
    ),
    "Negative" = plot_cg(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend,
      show_outliers,
      outliers_factor,
      outliers_labels
    ),
    "Housekeeping" = plot_cg(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend,
      show_outliers,
      outliers_factor,
      outliers_labels
    ),
    "PN" = plot_pn(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend
    ),
    "ACBD" = plot_acbd(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend,
      show_outliers,
      outliers_factor,
      outliers_labels
    ),
    "ACMC" = plot_acmc(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend
    ),
    "PCA12" = plot_pca12(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend
    ),
    "PCAi" = plot_pcai(
      nacho_object = object,
      x,
      colour,
      size),
    "PCA" = plot_pca(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend
    ),
    "PFNF" = plot_pfnf(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend,
      show_outliers,
      outliers_factor,
      outliers_labels
    ),
    "HF" = plot_hf(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend,
      show_outliers,
      outliers_factor,
      outliers_labels
    ),
    "NORM" = plot_norm(
      nacho_object = object,
      x,
      colour,
      size,
      show_legend
    ),
    stop(
      paste(
        '[NACHO] "x" must be one of the following possible values:',
        '  * "BD", "FoV", "PCL", "LoD"',
        '  * "Positive", "Negative", "Housekeeping", "PN"',
        '  * "ACBD", "ACMC"',
        '  * "PCA12", "PCAi", "PCA"',
        '  * "PFB", "HF", "NORM"',
        sep = "\n"
      )
    )
  )
}

#' plot_metrics
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#' @noRd
#'
#' @return NULL
plot_metrics <- function(
  nacho_object,
  x,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels
) {
  labels <- c(
    "BD" = "Binding Density",
    "FoV" = "Field of View",
    "PCL" = "Positive Control Linearity",
    "LoD" = "Limit of Detection"
  )
  units <- c(
    "BD" = '"(Optical features / ", mu, m^2, ")"',
    "FoV" = '"(% Counted)"',
    "PCL" = '"(R^2)"',
    "LoD" = '"(Z)"'
  )

  if (attr(nacho_object, "RCC_type") == "n8" & x %in% c("PCL", "LoD")) {
    message('[NACHO] "PCL" and "LoD" are not available for RCC type "n8".')
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          x = "CartridgeID",
          y = parse(text = paste0("atop(\"", labels[x], "\", paste(",  units[x], "))")),
          colour = colour
        ) +
        ggplot2::annotate(
          "text", x = 0.5, y = 0.5, label = "Not available!",
          angle = 30, size = 24, colour = "red", alpha = 0.25
        ) +
        ggplot2::theme(axis.text = ggplot2::element_blank())
    )
  }

  if (!is.null(outliers_labels) && !outliers_labels %in% colnames(nacho_object$nacho)) {
    outliers_labels <- nacho_object$access
  }

  ggplot2::ggplot(
    data = nacho_object$nacho |>
      dplyr::mutate(
        !!nacho_object$access := gsub("_S[0-9]*$", "", .data[[nacho_object$access]])
      ) |>
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        !!x,
        "is_outlier",
        !!outliers_labels
      ) |>
      dplyr::distinct(),
    mapping = ggplot2::aes(
      x = .data[["CartridgeID"]],
      y = .data[[x]],
      colour = .data[[colour]]
    )
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    ggplot2::geom_boxplot(
      mapping = ggplot2::aes(group = .data[["CartridgeID"]]),
      fill = NA,
      outlier.shape = NA,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    {
      if (show_outliers) {
        list(
          ggbeeswarm::geom_quasirandom(
            data = ~ dplyr::filter(.x, !.data[["is_outlier"]]),
            size = size, width = 0.25, na.rm = TRUE, groupOnX = TRUE
          ),
          ggbeeswarm::geom_quasirandom(
            data = ~ dplyr::filter(.x, .data[["is_outlier"]]),
            size = size * outliers_factor,
            colour = "red",
            width = 0.25,
            na.rm = TRUE,
            groupOnX = TRUE
          ),
          if (!is.null(outliers_labels)) {
            ggrepel::geom_label_repel(
              data = ~ dplyr::filter(.x, .data[["is_outlier"]]),
              mapping = ggplot2::aes(label = .data[[outliers_labels]]),
              colour = "red",
              na.rm = TRUE
            )
          }
        )
      } else {
        ggbeeswarm::geom_quasirandom(size = size, width = 0.25, na.rm = TRUE, groupOnX = TRUE)
      }
    } +
    ggplot2::labs(
      x = "CartridgeID",
      y = parse(text = paste0("atop(\"", labels[x], "\", paste(",  units[x], "))")),
      colour = colour
    ) +
    ggplot2::geom_rect(
      data = dplyr::tibble(
        ymin = nacho_object$outliers_thresholds[[x]],
        ymax = c(-Inf, Inf)[seq_along(!!dplyr::sym("ymin"))]
      ),
      mapping = ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
      fill = "firebrick2",
      alpha = 0.2,
      colour = "transparent",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      data = dplyr::tibble(value = nacho_object$outliers_thresholds[[x]]),
      mapping = ggplot2::aes(yintercept = .data[["value"]]),
      colour = "firebrick2",
      linetype = "longdash"
    ) +
    {if (!show_legend) ggplot2::guides(colour = "none")} +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1))
}


#' plot_cg
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#' @noRd
#'
#' @return NULL
plot_cg <- function(
  nacho_object,
  x,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels
) {
  if (!is.null(outliers_labels) && !outliers_labels %in% colnames(nacho_object$nacho)) {
    outliers_labels <- nacho_object$access
  }
  if (is.null(nacho_object$housekeeping_genes) & x %in% "Housekeeping") {
    message("[NACHO] No housekeeping genes found.")
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          x = "Gene Name",
          y = "Counts + 1",
          colour = colour
        ) +
        ggplot2::annotate(
          "text", x = 0.5, y = 0.5, label = "Not available!",
          angle = 30, size = 24, colour = "red", alpha = 0.25
        ) +
        ggplot2::theme(axis.text = ggplot2::element_blank())
    )
  }

  ggplot2::ggplot(
    data = nacho_object$nacho |>
      dplyr::mutate(
        !!nacho_object$access := gsub("_S[0-9]*$", "", .data[[nacho_object$access]])
      ) |>
      dplyr::filter(!!dplyr::sym("CodeClass") %in% x) |>
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        "Name",
        "Count",
        "is_outlier",
        !!outliers_labels
      ) |>
      dplyr::distinct(),
    mapping = ggplot2::aes(
      x = .data[["Name"]],
      y = .data[["Count"]] + 1,
      colour = .data[[colour]]
    )
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    ggplot2::geom_boxplot(
      mapping = ggplot2::aes(group = .data[["Name"]]),
      fill = NA,
      outlier.shape = NA,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    {
      if (show_outliers) {
        list(
          ggbeeswarm::geom_quasirandom(
            data = ~ dplyr::filter(.x, !.data[["is_outlier"]]),
            size = size, width = 0.25, na.rm = TRUE, groupOnX = TRUE
          ),
          ggbeeswarm::geom_quasirandom(
            data = ~ dplyr::filter(.x, .data[["is_outlier"]]),
            size = size * outliers_factor,
            colour = "red",
            width = 0.25,
            na.rm = TRUE,
            groupOnX = TRUE
          ),
          if (!is.null(outliers_labels)) {
            ggrepel::geom_label_repel(
              data = ~ dplyr::filter(.x, .data[["is_outlier"]]),
              mapping = ggplot2::aes(label = .data[[outliers_labels]]),
              colour = "red",
              na.rm = TRUE
            )
          }
        )
      } else {
        ggbeeswarm::geom_quasirandom(size = size, width = 0.25, na.rm = TRUE, groupOnX = TRUE)
      }
    } +
    ggplot2::scale_y_log10(
      # limits = c(1, NA),
      labels = function(x) format(x, big.mark = ",")
    ) +
    ggplot2::labs(
      x = if (x %in% c("Negative", "Positive")) "Control Name" else "Gene Name",
      y = "Counts + 1",
      colour = colour
    ) +
    {if (!show_legend) ggplot2::guides(colour = "none")} +
    ggplot2::theme(axis.text.x = ggplot2::element_text(face = "italic", angle = 30, hjust = 1, vjust = 1))
}


#' plot_pn
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#' @noRd
#'
#' @return NULL
plot_pn <- function(
  nacho_object,
  x,
  colour,
  size,
  show_legend
) {
  ggplot2::ggplot(
    data = nacho_object$nacho |>
      dplyr::filter(!!dplyr::sym("CodeClass") %in% c("Positive", "Negative")) |>
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        "CodeClass",
        "Name",
        "Count",
        "is_outlier"
      ) |>
      dplyr::distinct(),
    mapping = ggplot2::aes(
      x = .data[[nacho_object$access]],
      y = .data[["Count"]] + 1,
      colour = .data[["Name"]],
      group = .data[["Name"]]
    )
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(facets = "CodeClass", scales = "free_y", ncol = 2) +
    ggplot2::scale_y_log10(
      # limits = c(1, NA),
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
        x = as.numeric(as.factor(.data[[nacho_object$access]])),
        linetype = "Loess",
        group = "CodeClass"
      ),
      colour = "black",
      se = TRUE,
      method = "loess"
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2)) +
    {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_acbd
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#' @noRd
#'
#' @return NULL
plot_acbd <- function(
  nacho_object,
  x,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels
) {
  if (!is.null(outliers_labels) && !outliers_labels %in% colnames(nacho_object$nacho)) {
    outliers_labels <- nacho_object$access
  }
  ggplot2::ggplot(
    data = nacho_object$nacho |>
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        "MC",
        "BD",
        "is_outlier",
        !!outliers_labels
      ) |>
      dplyr::distinct(),
    mapping = ggplot2::aes(
      x = .data[["MC"]],
      y = .data[["BD"]],
      colour = .data[[colour]]
    )
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    {
      if (show_outliers) {
        list(
          ggplot2::geom_point(
            data = ~ dplyr::filter(.x, !.data[["is_outlier"]]),
            size = size, na.rm = TRUE
          ),
          ggplot2::geom_point(
            data = ~ dplyr::filter(.x, .data[["is_outlier"]]),
            size = size * outliers_factor,
            colour = "red",
            na.rm = TRUE
          ),
          if (!is.null(outliers_labels)) {
            ggrepel::geom_label_repel(
              data = ~ dplyr::filter(.x, .data[["is_outlier"]]),
              mapping = ggplot2::aes(label = .data[[outliers_labels]]),
              colour = "red",
              na.rm = TRUE
            )
          }
        )
      } else {
        ggplot2::geom_point(size = size, na.rm = TRUE)
      }
    } +
    ggplot2::scale_x_continuous(labels = function(x) format(x, big.mark = ",")) +
    ggplot2::labs(
      x = "Average Counts",
      y = parse(text = 'atop("Binding Density", paste("(Optical features / ", mu, m^2, ")"))'),
      colour = colour
    ) +
    ggplot2::geom_rect(
      data = dplyr::tibble(
        ymin = nacho_object$outliers_thresholds[["BD"]],
        ymax = c(-Inf, Inf)[seq_along(!!dplyr::sym("ymin"))]
      ),
      mapping = ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
      fill = "firebrick2",
      alpha = 0.2,
      colour = "transparent",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      data = dplyr::tibble(value = nacho_object$outliers_thresholds[["BD"]]),
      mapping = ggplot2::aes(yintercept = .data[["value"]]),
      colour = "firebrick2",
      linetype = "longdash"
    )  +
    {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_acmc
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#' @noRd
#'
#' @return NULL
plot_acmc <- function(
  nacho_object,
  x,
  colour,
  size,
  show_legend
) {
  ggplot2::ggplot(
    data = nacho_object$nacho |>
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        "MC",
        "MedC"
      ) |>
      dplyr::distinct(),
    mapping = ggplot2::aes(
      x = .data[["MC"]],
      y = .data[["MedC"]],
      colour = .data[[colour]]
    )
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    ggplot2::geom_point(size = size, na.rm = TRUE) +
    ggplot2::scale_x_continuous(labels = function(x) format(x, big.mark = ",")) +
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
    ggplot2::labs(
      x = "Average Counts",
      y = "Median Counts",
      colour = colour
    ) +
    {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_pca12
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#' @noRd
#'
#' @return NULL
plot_pca12 <- function(
  nacho_object,
  x,
  colour,
  size,
  show_legend
) {
  ggplot2::ggplot(
    data = nacho_object$nacho |>
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        "PC01",
        "PC02"
      ) |>
      dplyr::distinct(),
    mapping = ggplot2::aes(
      x = .data[["PC01"]],
      y = .data[["PC02"]],
      colour = .data[[colour]]
    )
  ) +
    ggforce::geom_mark_ellipse(na.rm = TRUE, alpha = 0.1) +
    ggplot2::geom_point(size = size, na.rm = TRUE) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    ggplot2::scale_fill_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0.25)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.25)) +
    ggplot2::labs(x = "PC01", y = "PC02", colour = colour) +
    {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_pca
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#' @noRd
#'
#' @return NULL
plot_pca <- function(
  nacho_object,
  x,
  colour,
  size,
  show_legend
) {
  ggplot2::ggplot(
    data = dplyr::full_join(
      x = nacho_object$nacho |>
          dplyr::select(
            "CartridgeID",
            !!colour,
            !!nacho_object$access,
            sprintf("PC%02d", seq_len(min(nacho_object$n_comp, 5)))
          ) |>
          dplyr::distinct() |>
          tidyr::gather(key = "X.PC", value = "X", sprintf("PC%02d", seq_len(min(nacho_object$n_comp, 5)))),
        y = nacho_object$nacho |>
          dplyr::select(
            "CartridgeID",
            !!colour,
            !!nacho_object$access,
            sprintf("PC%02d", seq_len(min(nacho_object$n_comp, 5)))
          ) |>
          dplyr::distinct() |>
          tidyr::gather(key = "Y.PC", value = "Y", sprintf("PC%02d", seq_len(min(nacho_object$n_comp, 5)))),
        by = unique(c("CartridgeID", nacho_object$access, colour))
      ) |>
        dplyr::filter(
          as.numeric(gsub("PC", "", .data[["X.PC"]])) < as.numeric(gsub("PC", "", .data[["Y.PC"]]))
        ),
    mapping = ggplot2::aes(
      x = .data[["X"]],
      y = .data[["Y"]],
      colour = .data[[colour]],
      fill = .data[[colour]]
    )
  ) +
    ggforce::geom_mark_ellipse(na.rm = TRUE, alpha = 0.1) +
    ggplot2::geom_point(size = size, na.rm = TRUE) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    ggplot2::scale_fill_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0.25)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.25)) +
    ggplot2::labs(x = NULL, y = NULL, colour = colour, fill = colour) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["Y.PC"]]),
      cols = ggplot2::vars(.data[["X.PC"]]),
      scales = "free"
    ) +
    {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_pcai
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#' @noRd
#'
#' @return NULL
plot_pcai <- function(
  nacho_object,
  x,
  colour,
  size
) {
  ggplot2::ggplot(
    data = dplyr::mutate(
      .data = nacho_object$pc_sum,
      PoV = sprintf("%0.2f%%", !!dplyr::sym("Proportion of Variance") * 100)
    ),
    mapping = ggplot2::aes(x = .data[["PC"]], y = !!dplyr::sym("Proportion of Variance"))
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = 1, end = 0.85) +
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


#' plot_pfnf
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#' @noRd
#'
#' @return NULL
plot_pfnf <- function(
  nacho_object,
  x,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels
) {
  if (!is.null(outliers_labels) && !outliers_labels %in% colnames(nacho_object$nacho)) {
    outliers_labels <- nacho_object$access
  }
  ggplot2::ggplot(
    data = nacho_object$nacho |>
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        "Negative_factor",
        "Positive_factor",
        "is_outlier",
        !!outliers_labels
      ) |>
      dplyr::distinct(),
    mapping = ggplot2::aes(
      x = .data[["Negative_factor"]],
      y = .data[["Positive_factor"]],
      colour = .data[[colour]]
    )
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    {
      if (show_outliers) {
        list(
          ggplot2::geom_point(
            data = ~ dplyr::filter(.x, !.data[["is_outlier"]]),
            size = size, na.rm = TRUE
          ),
          ggplot2::geom_point(
            data = ~ dplyr::filter(.x, .data[["is_outlier"]]),
            size = size * outliers_factor,
            colour = "red",
            na.rm = TRUE
          ),
          if (!is.null(outliers_labels)) {
            ggrepel::geom_label_repel(
              data = ~ dplyr::filter(.x, .data[["is_outlier"]]),
              mapping = ggplot2::aes(label = .data[[outliers_labels]]),
              colour = "red",
              na.rm = TRUE
            )
          }
        )
      } else {
        ggplot2::geom_point(size = size, na.rm = TRUE)
      }
    } +
    ggplot2::labs(x = "Negative Factor", y = "Positive Factor", colour = colour) +
    ggplot2::scale_y_log10() +
    ggplot2::geom_rect(
      data = dplyr::tibble(
        ymin = nacho_object$outliers_thresholds[["Positive_factor"]],
        ymax = c(0, Inf)
      ),
      mapping = ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
      fill = "firebrick2",
      alpha = 0.2,
      colour = "transparent",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      data = dplyr::tibble(value = nacho_object$outliers_thresholds[["Positive_factor"]]),
      mapping = ggplot2::aes(yintercept = .data[["value"]]),
      colour = "firebrick2",
      linetype = "longdash"
    ) +
    {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_hf
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#' @noRd
#'
#' @return NULL
plot_hf <- function(
  nacho_object,
  x,
  colour,
  size,
  show_legend,
  show_outliers,
  outliers_factor,
  outliers_labels
) {
  if (!is.null(outliers_labels) && !outliers_labels %in% colnames(nacho_object$nacho)) {
    outliers_labels <- nacho_object$access
  }

  is_house_factor <- "House_factor" %in% colnames(nacho_object[["nacho"]])
  if (!is_house_factor) {
    message('[NACHO] "House_factor" was not computed.')
    return(
      ggplot2::ggplot() +
        ggplot2::labs(x = "Positive Factor", y = "Housekeeping Factor", colour = colour) +
        ggplot2::annotate(
          "text", x = 0.5, y = 0.5, label = "Not available!",
          angle = 30, size = 24, colour = "red", alpha = 0.25
        ) +
        ggplot2::theme(axis.text = ggplot2::element_blank())
    )
  }

  ggplot2::ggplot(
    data = nacho_object$nacho |>
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        "House_factor",
        "Positive_factor",
        "is_outlier",
        !!outliers_labels
      ) |>
      dplyr::distinct(),
    mapping = ggplot2::aes(
      x = .data[["Positive_factor"]],
      y = .data[["House_factor"]],
      colour = .data[[colour]]
    )
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    {
      if (show_outliers) {
        list(
          ggplot2::geom_point(
            data = ~ dplyr::filter(.x, !.data[["is_outlier"]]),
            size = size, na.rm = TRUE
          ),
          ggplot2::geom_point(
            data = ~ dplyr::filter(.x, .data[["is_outlier"]]),
            size = size * outliers_factor,
            colour = "red",
            na.rm = TRUE
          ),
          if (!is.null(outliers_labels)) {
            ggrepel::geom_label_repel(
              data = ~ dplyr::filter(.x, .data[["is_outlier"]]),
              mapping = ggplot2::aes(label = .data[[outliers_labels]]),
              colour = "red",
              na.rm = TRUE
            )
          }
        )
      } else {
        ggplot2::geom_point(size = size, na.rm = TRUE)
      }
    } +
    ggplot2::labs(x = "Positive Factor", y = "Housekeeping Factor", colour = colour) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::geom_rect(
      data = dplyr::tibble(
        xmin = c(0, 0, nacho_object$outliers_thresholds[["Positive_factor"]]),
        xmax = c(Inf, Inf, 0, Inf),
        ymin = c(nacho_object$outliers_thresholds[["House_factor"]], 0, 0),
        ymax = c(0, Inf, Inf, Inf)
      ),
      mapping = ggplot2::aes(xmin = .data[["xmin"]], xmax = .data[["xmax"]], ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
      fill = "firebrick2",
      alpha = 0.2,
      colour = "transparent",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      data = dplyr::tibble(value = nacho_object$outliers_thresholds[["House_factor"]]),
      mapping = ggplot2::aes(yintercept = .data[["value"]]),
      colour = "firebrick2",
      linetype = "longdash"
    ) +
    ggplot2::geom_vline(
      data = dplyr::tibble(value = nacho_object$outliers_thresholds[["Positive_factor"]]),
      mapping = ggplot2::aes(xintercept = .data[["value"]]),
      colour = "firebrick2",
      linetype = "longdash"
    ) +
    {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_norm
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#' @noRd
#'
#' @return NULL
plot_norm <- function(
  nacho_object,
  x,
  colour,
  size,
  show_legend
) {
  if (is.null(nacho_object$housekeeping_genes)) {
    probe_var <- "CodeClass"
    probe_type <- "Positive"
  } else {
    probe_var <- "Name"
    probe_type <- nacho_object$housekeeping_genes
  }

  ggplot2::ggplot(
    data = nacho_object$nacho |>
      dplyr::select(
        "CartridgeID",
        !!nacho_object$access,
        "Count",
        "Count_Norm",
        "Name",
        "CodeClass",
        "is_outlier"
      ) |>
      dplyr::distinct() |>
      dplyr::filter(.data[[probe_var]] %in% !!probe_type) |>
      tidyr::gather(key = "Status", value = "Count", c("Count", "Count_Norm")) |>
      dplyr::mutate(
        Status = factor(
          x = c("Count" = "Raw", "Count_Norm" = "Normalised")[!!dplyr::sym("Status")],
          levels = c("Count" = "Raw", "Count_Norm" = "Normalised")
        ),
        Count = !!dplyr::sym("Count") + 1
      ),
    mapping = ggplot2::aes(
      x = .data[[nacho_object$access]],
      y = .data[["Count"]]
    )
  ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(colour = .data[["Name"]], group = .data[["Name"]]),
      size = size,
      na.rm = TRUE
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data[["Status"]])) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = 1, end = 0.85) +
    ggplot2::scale_x_discrete(label = NULL) +
    ggplot2::scale_y_log10(
      # limits = c(1, NA),
      labels = function(x) format(x, big.mark = ",")
    ) +
    ggplot2::labs(
      x = "Sample Index",
      y = "Counts + 1",
      colour = if (is.null(nacho_object$housekeeping_genes)) "Positive Control" else "Housekeeping Genes",
      linetype = "Smooth"
    ) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::geom_smooth(
      mapping = ggplot2::aes(
        x = as.numeric(as.factor(.data[[nacho_object$access]])),
        linetype = "Loess"
      ),
      colour = "black",
      se = TRUE,
      method = "loess"
    ) +
    {if (!(show_legend & length(nacho_object$housekeeping_genes) <= 10)) ggplot2::guides(colour = "none")}
}
