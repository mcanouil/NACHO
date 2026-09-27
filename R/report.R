#' @include nacho-class.R
NULL

#' Write the quality-control report of a nacho object as markdown
#'
#' Prints the markdown text and the figures of the report that [render()]
#' builds, for use in an R Markdown chunk with `results = "asis"`.
#'
#' @param x A `nacho` object from [load_rcc()] or [normalise()].
#' @inheritParams render
#' @param title_level The level of the first heading, *i.e.*, the number of
#'   `"#"`.
#'
#' @return `x`, invisibly.
#'
#' @keywords internal
#' @noRd
report_markdown <- function(
  x,
  colour = "CartridgeID",
  size = 0.5,
  show_legend = FALSE,
  show_outliers = TRUE,
  outliers_factor = 1,
  outliers_labels = NULL,
  title_level = 1
) {
  check_nacho(x)

  prefix_title <- function(title_level, level) {
    paste(c("\n\n", rep("#", title_level + level)), collapse = "")
  }
  plot_section <- function(type) {
    suppressWarnings(print(autoplot_nacho(
      x,
      type = type,
      colour = colour,
      size = size,
      show_legend = show_legend,
      show_outliers = show_outliers,
      outliers_factor = outliers_factor,
      outliers_labels = outliers_labels
    )))
  }

  cat(prefix_title(title_level, 0), "RCC Summary\n\n")
  cat("  - Samples:", ncol(x), "\n")
  genes <- table(x@probes[["CodeClass"]])
  cat(paste0("  - ", names(genes), ": ", genes, "\n"))

  housekeeping_genes <- x@probes[["Name"]][x@probes[["is_housekeeping"]]]
  thresholds <- x@thresholds
  cat(prefix_title(title_level, 0), "Settings\n\n")
  cat(
    "  - Predict housekeeping genes:",
    x@settings[["housekeeping_predict"]],
    "\n"
  )
  cat(
    "  - Normalise using housekeeping genes:",
    x@settings[["housekeeping_norm"]],
    "\n"
  )
  cat(
    "  - Housekeeping genes available:",
    paste(housekeeping_genes[-length(housekeeping_genes)], collapse = ", "),
    "and",
    housekeeping_genes[length(housekeeping_genes)],
    "\n"
  )
  cat("  - Normalise using:", x@settings[["normalisation_method"]], "\n")
  cat(
    "  - Principal components to compute:",
    x@settings[["n_comp"]],
    "\n"
  )
  cat(
    "\n",
    "    + ",
    "Binding Density (BD) <",
    round(thresholds[["BD"]][1], 3),
    "\n",
    "    + ",
    "Binding Density (BD) >",
    round(thresholds[["BD"]][2], 3),
    "\n",
    "    + ",
    "Field of View (FoV) <",
    round(thresholds[["FoV"]], 3),
    "\n",
    "    + ",
    "Positive Control Linearity (PCL) <",
    round(thresholds[["PCL"]], 3),
    "\n",
    "    + ",
    "Limit of Detection (LoD) <",
    round(thresholds[["LoD"]], 3),
    "\n",
    "    + ",
    "Positive normalisation factor (Positive_factor) <",
    round(thresholds[["Positive_factor"]][1], 3),
    "\n",
    "    + ",
    "Positive normalisation factor (Positive_factor) >",
    round(thresholds[["Positive_factor"]][2], 3),
    "\n",
    "    + ",
    "Housekeeping normalisation factor (house_factor) <",
    round(thresholds[["House_factor"]][1], 3),
    "\n",
    "    + ",
    "Housekeeping normalisation factor (house_factor) >",
    round(thresholds[["House_factor"]][2], 3),
    "\n"
  )

  details <- vapply(
    X = c(
      "BD" = "about-bd.md",
      "FoV" = "about-fov.md",
      "PCL" = "about-pcl.md",
      "LoD" = "about-lod.md"
    ),
    FUN = function(file) {
      paste(
        readLines(system.file("app", "www", file, package = "NACHO")),
        collapse = "\n"
      )
    },
    FUN.VALUE = character(1)
  )

  metrics <- switch(
    EXPR = x@rcc_type,
    "n1" = c("BD", "FoV", "PCL", "LoD"),
    "n8" = c("BD", "FoV")
  )

  cat(prefix_title(title_level, 0), "QC Metrics\n\n")
  for (imetric in metrics) {
    cat(prefix_title(title_level, 1), metric_labels[imetric], "\n\n")
    cat(details[imetric], "\n\n")
    plot_section(imetric)
    cat("\n")
  }

  sections <- data.frame(
    title = c(
      "Control Genes",
      "Positive",
      "Negative",
      "Housekeeping",
      "Control Probe Expression",
      "Quality-Control Visuals",
      "Average Count vs. Binding Density",
      "Average Count vs. Median Count",
      "Principal Component",
      "PC1 vs. PC2",
      "Factorial planes",
      "Proportion of Variance Explained",
      "Normalisation",
      "Positive Factor vs. Background Threshold",
      "Housekeeping Factor",
      "Normalisation Result"
    ),
    plot = c(
      NA,
      "Positive",
      "Negative",
      "Housekeeping",
      "PN",
      NA,
      "ACBD",
      "ACMC",
      NA,
      "PCA12",
      "PCA",
      "PCAi",
      NA,
      "PFNF",
      "HF",
      "NORM"
    ),
    level = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 2, 2, 2, 0, 1, 1, 1)
  )

  for (isection in seq_len(nrow(sections))) {
    cat(
      prefix_title(title_level, sections[isection, "level"]),
      sections[isection, "title"],
      "\n\n"
    )
    if (!is.na(sections[isection, "plot"])) {
      plot_section(sections[isection, "plot"])
      cat("\n")
    }
  }

  qc <- nacho_qc(x)
  if (any(qc[["is_outlier"]] %in% TRUE)) {
    cat(prefix_title(title_level, 1), "Outliers", "\n\n")
    print(knitr::kable(
      qc[qc[["is_outlier"]] %in% TRUE, setdiff(names(qc), "is_outlier")],
      row.names = FALSE
    ))
  }

  invisible(x)
}
