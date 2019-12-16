#' Print method for nacho object
#'
#' This function allows to print text and figures from the results of a call to `summarise()`
#' or `normalise()`.
#' It is intended to be used in a `Rmarkdown` chunk.
#'
#' @param x [[list]] List obtained from [summarise] or [normalise].
#' @inheritParams render
#' @inheritParams autoplot.nacho
#' @param echo [[logical]] A boolean to indicate whether text and plots should be printed.
#'   Mainly for use within a Rmarkdown chunk.
#' @param title_level [[numeric]] A numeric to indicate the title level to start with, using markdown style,
#'   *i.e.*, the number of "#".
#' @param ... Other arguments (Not used).
#'
#' @return NULL
#' @export
#'
#' @examples
#'
#' data(GSE74821)
#' print(GSE74821)
#'
print.nacho <- function(x, colour = "CartridgeID", size = 0.5, show_legend = FALSE, echo = FALSE, title_level = 1, ...) {
  if (!echo) return(utils::str(x, 1))

  if (is.numeric(x$nacho[[colour]])) {
    x$nacho[[colour]] <- as.character(x$nacho[[colour]])
  }
  prefix_title <- function(title_level, x) paste(c("\n\n", rep("#", title_level + x)), collapse = "")

  cat(prefix_title(title_level, 0), "RCC Summary\n\n")
  cat('  - Samples:', length(unique(x$nacho[[x$access]])), "\n")
  genes <- table(x$nacho[["CodeClass"]]) /
    length(unique(x$nacho[[x$access]]))
  cat(paste0("  - ", names(genes), ": ", genes, "\n"))

  cat(prefix_title(title_level, 0), "Settings\n\n")
  cat('  - Predict housekeeping genes:', x$housekeeping_predict, "\n")
  cat('  - Normalise using housekeeping genes:', x$housekeeping_norm, "\n")
  cat(
    '  - Housekeeping genes available:',
    paste(x$housekeeping_genes[-length(x$housekeeping_genes)], collapse = ", "),
    "and",
    x$housekeeping_genes[length(x$housekeeping_genes)], "\n"
  )
  cat('  - Normalise using:', x$normalisation_method, "\n")
  cat('  - Principal components to compute:', x$n_comp, "\n")
  cat('  - Remove outliers:', x$remove_outliers, "\n")
  cat(
    "\n",
    '    + ', 'Binding Density (BD) <',
      round(x$outliers_thresholds[["BD"]][1], 3), '\n',
    '    + ', 'Binding Density (BD) >',
      round(x$outliers_thresholds[["BD"]][2], 3), '\n',
    '    + ', 'Field of View (FoV) <',
      round(x$outliers_thresholds[["FoV"]], 3), '\n',
    '    + ', 'Positive Control Linearity (PCL) <',
      round(x$outliers_thresholds[["PCL"]], 3), '\n',
    '    + ', 'Limit of Detection (LoD) <',
      round(x$outliers_thresholds[["LoD"]], 3), '\n',
    '    + ', 'Positive normalisation factor (Positive_factor) <',
      round(x$outliers_thresholds[["Positive_factor"]][1], 3), '\n',
    '    + ', 'Positive normalisation factor (Positive_factor) >',
      round(x$outliers_thresholds[["Positive_factor"]][2], 3), '\n',
    '    + ', 'Housekeeping normalisation factor (house_factor) <',
      round(x$outliers_thresholds[["House_factor"]][1], 3), '\n',
    '    + ', 'Housekeeping normalisation factor (house_factor) >',
      round(x$outliers_thresholds[["House_factor"]][2], 3), '\n'
  )

  cat(prefix_title(title_level, 0), "QC Metrics\n\n")
  labels <- c(
    "BD" = "Binding Density",
    "FoV" = "Field of View (Imaging)",
    "PCL" = "Positive Control Linearity",
    "LoD" = "Limit of Detection"
  )
  units <- c(
    "BD" = '"(Optical features / ", mu, m^2, ")"',
    "FoV" = '"(% Counted)"',
    "PCL" = '(R^2)',
    "LoD" = '"(Z)"'
  )
  details <- c(
    "BD" = paste(
      readLines(system.file("app", "www", "about-bd.md", package = "NACHO")),
      collapse = "\n"
    ),
    "FoV" = paste(
      readLines(system.file("app", "www", "about-fov.md", package = "NACHO")),
      collapse = "\n"
    ),
    "PCL" = paste(
      readLines(system.file("app", "www", "about-pcl.md", package = "NACHO")),
      collapse = "\n"
    ),
    "LoD" = paste(
      readLines(system.file("app", "www", "about-lod.md", package = "NACHO")),
      collapse = "\n"
    )
  )

  metrics <- switch(
    EXPR = attr(x, "RCC_type"),
    "n1" = c("BD", "FoV", "PCL", "LoD"),
    "n8" = c("BD", "FoV")
  )

  for (imetric in metrics) {
    cat(prefix_title(title_level, 1), labels[imetric], "\n\n")
    cat(details[imetric], "\n")
    suppressWarnings(print(autoplot.nacho(
      x = imetric,
      object = x,
      colour = colour,
      size = size,
      show_legend = show_legend
    )))
    cat("\n")
  }


  sections <- data.frame(
    title = c(
      "Control Genes", "Positive", "Negative", "Housekeeping", "Control Probe Expression",
      "QC Visuals", "Average Count vs. Binding Density", "Average Count vs. Median Count",
      "Principal Component", "PC1 vs. PC2", "Factorial planes", "Inertia",
      "Normalisation Factors", "Positive Factor vs. Background Threshold", "Housekeeping Factor",
      "Normalisation Result"
    ),
    plot = c(
      NA, "Positive", "Negative", "Housekeeping", "PN",
      NA, "ACBD", "ACMC",
      NA, "PCA12", "PCA", "PCAi",
      NA, "PFNF", "HF", "NORM"
    ),
    level = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 2, 2, 2, 0, 1, 1, 1)
  )

  for (isection in seq(nrow(sections))) {
    cat(prefix_title(title_level, sections[isection, "level"]), sections[isection, "title"], "\n\n")
    if (!is.na(sections[isection, "plot"])) {
      suppressWarnings(print(autoplot.nacho(
        x = sections[isection, "plot"],
        object = x,
        colour = colour,
        size = size,
        show_legend = show_legend
      )))
      cat("\n")
    }
  }

  invisible()
}
