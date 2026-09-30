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
    if (length(housekeeping_genes) == 0) {
      "none"
    } else {
      cli::ansi_collapse(housekeeping_genes)
    },
    "\n"
  )
  cat("  - Normalise using:", x@settings[["normalisation_method"]], "\n")
  cat(
    "  - Principal components to compute:",
    x@settings[["n_comp"]],
    "\n"
  )
  threshold_lines <- list(
    list("Binding Density (BD) <", thresholds[["BD"]][1]),
    list("Binding Density (BD) >", thresholds[["BD"]][2]),
    list("Field of View (FoV) <", thresholds[["FoV"]]),
    list("Positive Control Linearity (PCL) <", thresholds[["PCL"]]),
    list("Limit of Detection (LoD) <", thresholds[["LoD"]]),
    list(
      "Positive normalisation factor (Positive_factor) <",
      thresholds[["Positive_factor"]][1]
    ),
    list(
      "Positive normalisation factor (Positive_factor) >",
      thresholds[["Positive_factor"]][2]
    ),
    list(
      "Housekeeping normalisation factor (house_factor) <",
      thresholds[["House_factor"]][1]
    ),
    list(
      "Housekeeping normalisation factor (house_factor) >",
      thresholds[["House_factor"]][2]
    )
  )
  threshold_lines <- Filter(
    function(line) is.finite(line[[2]]),
    threshold_lines
  )
  do.call(
    cat,
    c(
      list("\n"),
      unlist(
        lapply(threshold_lines, function(line) {
          list("    + ", line[[1]], round(line[[2]], 3), "\n")
        }),
        recursive = FALSE
      )
    )
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
  failed <- qc[["status"]] %in% "fail"
  if (any(failed)) {
    cat(prefix_title(title_level, 1), "Outliers", "\n\n")
    print(knitr::kable(
      qc[failed, c(names(qc)[1], "lane", "CartridgeID", "n_flags", "reason")],
      row.names = FALSE
    ))
  }

  invisible(x)
}
