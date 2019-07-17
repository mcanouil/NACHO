#' Plot quality-control metrics and thresholds
#'
#' This function allows to plot any qualit-control figures available
#' within the shiny app using [visualise] or in the HTML report from [render].
#'
#' @inheritParams render
#' @param x [[character]] Character string naming the quality-control metrics to plot from `nacho_object`.
#'  The possible values are:
#'
#'    * `"BD"` (Binding Density)
#'    * `"FoV"` (Imaging)
#'    * `"PC"` (Positive Control Linearity)
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
#' autoplot(GSE74821, x = "BD")
#' autoplot(GSE74821, x = "PCA")
#'
autoplot.nacho <- function(object, x, colour = "CartridgeID", size = 0.5, show_legend = TRUE, ...) {
  if (missing(x)) {
    stop(
      paste(
        '[NACHO] "x" is missing. It must be one of the following possible values:',
        '  * "BD", "FoV", "PC", "LoD"',
        '  * "Positive", "Negative", "Housekeeping", "PN"',
        '  * "ACBD", "ACMC"',
        '  * "PCA12", "PCAi", "PCA"',
        '  * "PFB", "HF", "NORM"',
        sep = "\n"
      )
    )
  }
  if (attr(object, "RCC_type") == "n8" & x %in% c("PC", "LoD")) {
    stop('[NACHO] "PC" and "LoD" are not available for the provided NanoString dataset.')
  }
  switch(
    EXPR = x,
    "BD" = plot_metrics(x, nacho_object = object, colour, size, show_legend),
    "FoV" = plot_metrics(x, nacho_object = object, colour, size, show_legend),
    "PC" = plot_metrics(x, nacho_object = object, colour, size, show_legend),
    "LoD" = plot_metrics(x, nacho_object = object, colour, size, show_legend),
    "Positive" = plot_cg(x, nacho_object = object, colour, size, show_legend),
    "Negative" = plot_cg(x, nacho_object = object, colour, size, show_legend),
    "Housekeeping" = plot_cg(x, nacho_object = object, colour, size, show_legend),
    "PN" = plot_pn(x, nacho_object = object, colour, size, show_legend),
    "ACBD" = plot_acbd(x, nacho_object = object, colour, size, show_legend),
    "ACMC" = plot_acmc(x, nacho_object = object, colour, size, show_legend),
    "PCA12" = plot_pca12(x, nacho_object = object, colour, size, show_legend),
    "PCAi" = plot_pcai(x, nacho_object = object, colour, size, show_legend),
    "PCA" = plot_pca(x, nacho_object = object, colour, size, show_legend),
    "PFNF" = plot_pfnf(x, nacho_object = object, colour, size, show_legend),
    "HF" = plot_hf(x, nacho_object = object, colour, size, show_legend),
    "NORM" = plot_norm(x, nacho_object = object, colour, size, show_legend),
    stop(
      paste(
        '[NACHO] "x" must be one of the following possible values:',
        '  * "BD", "FoV", "PC", "LoD"',
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
#'
#' @return NULL
plot_metrics <- function(
  x,
  nacho_object,
  colour,
  size,
  show_legend
) {
  labels <- c(
    "BD" = "Binding Density",
    "FoV" = "Field of View",
    "PC" = "Positive Control linearity",
    "LoD" = "Limit of Detection"
  )
  units <- c(
    "BD" = '"(Optical features / ", mu, m^2, ")"',
    "FoV" = '"(% Counted)"',
    "PC" = '(R^2)',
    "LoD" = '"(Z)"'
  )
  ggplot2::ggplot(
    data = nacho_object$nacho %>%
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        !!x
      ) %>%
      dplyr::distinct(),
    mapping = ggplot2::aes(
      x = !!ggplot2::sym("CartridgeID"),
      y = !!ggplot2::sym(x),
      colour = !!ggplot2::sym(colour)
    )
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
    ggbeeswarm::geom_quasirandom(size = size, width = 0.25, na.rm = TRUE, groupOnX = TRUE) +
    ggplot2::labs(
      x = "CartridgeID",
      y = parse(text = paste0('paste("', labels[x], '", " ", ',  units[x], ")"))
    ) +
    ggplot2::geom_hline(
      data = dplyr::tibble(value = nacho_object$outliers_thresholds[[x]]),
      mapping = ggplot2::aes(yintercept = !!ggplot2::sym("value")),
      colour = "firebrick2",
      linetype = "longdash"
    ) +
    {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_cg
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#'
#' @return NULL
plot_cg <- function(
  x,
  nacho_object,
  colour,
  size,
  show_legend
) {
  p <- ggplot2::ggplot(
    data = nacho_object$nacho %>%
      dplyr::filter(!!dplyr::sym("CodeClass") %in% x) %>%
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        "Name",
        "Count"
      ) %>%
      dplyr::distinct(),
    mapping = ggplot2::aes(
      x = !!ggplot2::sym("Name"),
      y = !!ggplot2::sym("Count"),
      colour = !!ggplot2::sym(colour)
    )
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
    ggbeeswarm::geom_quasirandom(size = size, width = 0.25, na.rm = TRUE, groupOnX = TRUE) +
    ggplot2::scale_y_log10(limits = c(1, NA)) +
    ggplot2::labs(
      x = if (x %in% c("Negative", "Positive")) "Control Name" else "Gene Name",
      y = "Counts + 1"
    ) +
    {if (!show_legend) ggplot2::guides(colour = "none")} +
    ggplot2::theme(axis.text.x = ggplot2::element_text(face = "italic"))

  number_ticks_x <- nacho_object$nacho %>%
    dplyr::filter(!!dplyr::sym("CodeClass") %in% x) %>%
    dplyr::select("Name") %>%
    unlist() %>%
    unique() %>%
    length()

  if (number_ticks_x > 10 | show_legend) {
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
  }

  p
}


#' plot_pn
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#'
#' @return NULL
plot_pn <- function(
  x,
  nacho_object,
  colour,
  size,
  show_legend
) {
  ggplot2::ggplot(
    data = nacho_object$nacho %>%
      dplyr::filter(!!dplyr::sym("CodeClass") %in% c("Positive", "Negative")) %>%
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        "CodeClass",
        "Name",
        "Count"
      ) %>%
      dplyr::distinct(),
    mapping = ggplot2::aes(
      x = !!ggplot2::sym(nacho_object$access),
      y = !!ggplot2::sym("Count"),
      colour = !!ggplot2::sym("Name"),
      group = !!ggplot2::sym("Name")
    )
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(facets = "CodeClass", scales = "free_y", ncol = 2) +
    ggplot2::scale_y_log10(limits = c(1, NA)) +
    ggplot2::scale_x_discrete(labels = NULL) +
    ggplot2::labs(x = "Sample Index", y = "Counts + 1", colour = "Control") +
    ggplot2::theme(
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_acbd
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#'
#' @return NULL
plot_acbd <- function(
  x,
  nacho_object,
  colour,
  size,
  show_legend
) {
  nacho_object$nacho %>%
    dplyr::select(
      "CartridgeID",
      !!colour,
      !!nacho_object$access,
      "MC",
      "BD"
    ) %>%
    dplyr::distinct() %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = !!ggplot2::sym("MC"),
        y = !!ggplot2::sym("BD"),
        colour = !!ggplot2::sym(colour)
      )
    ) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::geom_point(size = size, na.rm = TRUE) +
      ggplot2::labs(
        x = "Average Counts",
        y = parse(text = 'paste("Binding Density", " ", "(Optical features / ", mu, m^2, ")")')
      ) +
      {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_acmc
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#'
#' @return NULL
plot_acmc <- function(
  x,
  nacho_object,
  colour,
  size,
  show_legend
) {
  nacho_object$nacho %>%
    dplyr::select(
      "CartridgeID",
      !!colour,
      !!nacho_object$access,
      "MC",
      "MedC"
    ) %>%
    dplyr::distinct() %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = !!ggplot2::sym("MC"),
        y = !!ggplot2::sym("MedC"),
        colour = !!ggplot2::sym(colour)
      )
    ) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::geom_point(size = size, na.rm = TRUE) +
      ggplot2::labs(
        x = "Average Counts",
        y = "Median Counts"
      ) +
      {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_pca12
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#'
#' @return NULL
plot_pca12 <- function(
  x,
  nacho_object,
  colour,
  size,
  show_legend
) {
  nacho_object$nacho %>%
    dplyr::select(
      "CartridgeID",
      !!colour,
      !!nacho_object$access,
      "PC1",
      "PC2"
    ) %>%
    dplyr::distinct() %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = !!ggplot2::sym("PC1"),
        y = !!ggplot2::sym("PC2"),
        colour = !!ggplot2::sym(colour)
      )
    ) +
      ggplot2::geom_point(size = size, na.rm = TRUE) +
      ggplot2::stat_ellipse(na.rm = TRUE) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_pca
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#'
#' @return NULL
plot_pca <- function(
  x,
  nacho_object,
  colour,
  size,
  show_legend
) {
  dplyr::full_join(
    x = nacho_object$nacho %>%
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        paste0("PC", 1:min(nacho_object$n_comp, 5))
      ) %>%
      dplyr::distinct() %>%
      tidyr::gather(key = "X.PC", value = "X", paste0("PC", 1:min(nacho_object$n_comp, 5))),
    y = nacho_object$nacho %>%
      dplyr::select(
        "CartridgeID",
        !!colour,
        !!nacho_object$access,
        paste0("PC", 1:min(nacho_object$n_comp, 5))
      ) %>%
      dplyr::distinct() %>%
      tidyr::gather(key = "Y.PC", value = "Y", paste0("PC", 1:min(nacho_object$n_comp, 5))),
    by = unique(c("CartridgeID", nacho_object$access, colour))
  ) %>%
    dplyr::filter(
      as.numeric(gsub("PC", "", !!dplyr::sym("X.PC"))) <
        as.numeric(gsub("PC", "", !!dplyr::sym("Y.PC")))
    ) %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = !!ggplot2::sym("X"),
        y = !!ggplot2::sym("Y"),
        colour = !!ggplot2::sym(colour)
      )
    ) +
      ggplot2::geom_point(size = size, na.rm = TRUE) +
      ggplot2::stat_ellipse(na.rm = TRUE) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(!!ggplot2::sym("Y.PC")),
        cols = ggplot2::vars(!!ggplot2::sym("X.PC")),
        scales = "free"
      ) +
      {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_pcai
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#'
#' @return NULL
plot_pcai <- function(
  x,
  nacho_object,
  colour,
  size,
  show_legend
) {
  nacho_object$pc_sum %>%
    dplyr::mutate(PoV = scales::percent(!!dplyr::sym("Proportion of Variance"))) %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(x = !!ggplot2::sym("PC"), y = !!dplyr::sym("Proportion of Variance"))
    ) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(
        mapping = ggplot2::aes(label = !!ggplot2::sym("PoV")),
        vjust = -1,
        show.legend = FALSE
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent,
        expand = ggplot2::expand_scale(mult = c(0, 0.15))
      ) +
      ggplot2::labs(x = "Number of Principal Component", y = "Proportion of Variance") +
      {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_pfnf
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#'
#' @return NULL
plot_pfnf <- function(
  x,
  nacho_object,
  colour,
  size,
  show_legend
) {
  nacho_object$nacho %>%
    dplyr::select(
      "CartridgeID",
      !!colour,
      !!nacho_object$access,
      "Negative_factor",
      "Positive_factor"
    ) %>%
    dplyr::distinct() %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = !!ggplot2::sym("Negative_factor"),
        y = !!ggplot2::sym("Positive_factor"),
        colour = !!ggplot2::sym(colour)
      )
    ) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::geom_point(size = size, na.rm = TRUE) +
      ggplot2::labs(x = "Negative Factor", y = "Positive Factor") +
      ggplot2::scale_y_log10() +
      {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_hf
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#'
#' @return NULL
plot_hf <- function(
  x,
  nacho_object,
  colour,
  size,
  show_legend
) {
  nacho_object$nacho %>%
    dplyr::select(
      "CartridgeID",
      !!colour,
      !!nacho_object$access,
      "House_factor",
      "Positive_factor"
    ) %>%
    dplyr::distinct() %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = !!ggplot2::sym("Positive_factor"),
        y = !!ggplot2::sym("House_factor"),
        colour = !!ggplot2::sym(colour)
      )
    ) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::geom_point(size = size, na.rm = TRUE) +
      ggplot2::labs(x = "Positive Factor", y = "Houskeeping Factor") +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10() +
      {if (!show_legend) ggplot2::guides(colour = "none")}
}


#' plot_norm
#'
#' @inheritParams autoplot.nacho
#'
#' @keywords internal
#'
#' @return NULL
plot_norm <- function(
  x,
  nacho_object,
  colour,
  size,
  show_legend
) {
  if (is.null(nacho_object$housekeeping_genes)) {
    probe_var <- dplyr::sym("CodeClass")
    probe_type <- "Positive"
  } else {
    probe_var <- dplyr::sym("Name")
    probe_type <- nacho_object$housekeeping_genes
  }

  nacho_object$nacho %>%
    dplyr::select(
      "CartridgeID",
      !!nacho_object$access,
      "Count",
      "Count_Norm",
      "Name"
    ) %>%
    dplyr::distinct() %>%
    dplyr::filter(!!probe_var %in% !!probe_type) %>%
    tidyr::gather(key = "Status", value = "Count", c("Count", "Count_Norm")) %>%
    dplyr::mutate(
      Status = factor(
        x = c("Count" = "Raw", "Count_Norm" = "Normalised")[!!dplyr::sym("Status")],
        levels = c("Count" = "Raw", "Count_Norm" = "Normalised")
      ),
      Count = !!dplyr::sym("Count") + 1
    ) %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = !!ggplot2::sym(nacho_object$access),
        y = !!ggplot2::sym("Count")
      )
    ) +
      ggplot2::geom_line(
        mapping = ggplot2::aes(colour = !!ggplot2::sym("Name"), group = !!ggplot2::sym("Name")),
        size = size,
        na.rm = TRUE
      ) +
      ggplot2::facet_grid(cols = ggplot2::vars(!!ggplot2::sym("Status"))) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::scale_x_discrete(label = NULL) +
      ggplot2::scale_y_log10(limits = c(1, NA)) +
      ggplot2::labs(
        x = "Sample Index",
        y = "Counts + 1",
        colour = if (is.null(nacho_object$housekeeping_genes)) "Positive Control" else "Housekeeping Genes",
        linetype = "Smooth"
      ) +
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank()
      ) +
      ggplot2::geom_smooth(
        mapping = ggplot2::aes(
          x = as.numeric(as.factor(!!ggplot2::sym(nacho_object$access))),
          linetype = "Loess"
        ),
        colour = "black",
        se = TRUE,
        method = "loess"
      ) +
      {if (!(show_legend & length(nacho_object$housekeeping_genes)<=10)) ggplot2::guides(colour = "none")}
}
