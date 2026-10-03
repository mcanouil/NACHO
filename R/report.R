#' @include qc-table.R
NULL

#' Readable names of the quality-control metrics
#'
#' @noRd
qc_metric_labels <- c(
  BD = "Binding density",
  FoV = "Field of view",
  PCL = "Positive control linearity",
  LoD = "Limit of detection",
  Positive_factor = "Positive normalisation factor",
  House_factor = "Content normalisation factor",
  Housekeeping_detected = "Housekeeping genes above background",
  Ligation_order = "Ligation controls in order",
  Ligation_R2 = "Ligation control linearity",
  Ligation_NEG = "Ligation negative above the detection limit",
  Haemolysis = "Haemolysis"
)

#' Values a metric can take, where a bound at the edge never flags
#'
#' @noRd
qc_metric_domains <- list(
  FoV = c(0, 100),
  PCL = c(0, 1),
  Ligation_order = c(0, 1),
  Ligation_R2 = c(0, 1)
)

#' Short description of each plot, for screen readers
#'
#' @noRd
plot_alt_texts <- c(
  BD = "Binding density of each sample, grouped by cartridge, with the thresholds shaded.",
  FoV = "Share of fields of view counted for each sample, grouped by cartridge, with the threshold shaded.",
  PCL = "Positive control linearity of each sample, grouped by cartridge, with the threshold shaded.",
  LoD = "Limit of detection of each sample, grouped by cartridge, with the threshold shaded.",
  Positive = "Counts of each positive control probe across samples.",
  Negative = "Counts of each negative control probe across samples.",
  Housekeeping = "Counts of each housekeeping gene across samples.",
  PN = "Counts of positive and negative control probes for each sample.",
  ACBD = "Average count against binding density, one point per sample.",
  ACMC = "Average count against median count, one point per sample.",
  PCA12 = "Samples on the first two principal components.",
  PCAi = "Share of variance explained by each principal component.",
  PCA = "Samples on each pair of the first principal components.",
  PFNF = "Positive normalisation factor against the negative factor, one point per sample, with the thresholds shaded.",
  HF = "Housekeeping factor against the positive factor, one point per sample, with the thresholds shaded.",
  NORM = "Housekeeping gene counts before and after normalisation, one line per gene.",
  Stability = "geNorm stability of each housekeeping gene, from the least to the most stable.",
  RLE = "Relative log expression of each sample after normalisation.",
  BatchFactors = "Normalisation factors of each sample, grouped by cartridge.",
  PCBatch = "Share of each principal component explained by cartridge and date."
)

#' Help page of a metric or of the app
#'
#' @param name The page name, such as `"bd"` or `"nacho"`.
#'
#' @noRd
help_text <- function(name) {
  path <- system.file(
    "app",
    "www",
    paste0("about-", name, ".md"),
    package = "NACHO"
  )
  if (!nzchar(path)) {
    nacho_abort(
      "The help page {.val {name}} is missing from the installed package.",
      class = "missing_file"
    )
  }
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

#' Samples that fail quality control, with their reasons
#'
#' @param x A `nacho` object.
#'
#' @noRd
qc_failures <- function(x) {
  qc <- nacho_qc(x)
  columns <- intersect(
    c(
      x@settings[["id_colname"]],
      "lane",
      "lane_status",
      "CartridgeID",
      "n_flags",
      "reason"
    ),
    names(qc)
  )
  qc[qc[["status"]] %in% "fail", columns, drop = FALSE]
}

#' Markdown lines summarising the object
#'
#' @noRd
report_overview <- function(x) {
  qc <- nacho_qc(x)
  thresholds <- x@thresholds
  instrument <- thresholds[["instrument"]]
  c(
    paste0("- Samples: ", nrow(qc)),
    paste0("- Cartridges: ", length(unique(qc[["CartridgeID"]]))),
    paste0("- Flagged samples: ", sum(qc[["status"]] %in% "fail")),
    paste0(
      "- Thresholds: ",
      thresholds[["preset"]],
      " preset, instrument ",
      if (is.na(instrument)) "unknown" else instrument
    ),
    paste0(
      "- Normalisation: ",
      x@settings[["normalisation_method"]],
      ", background ",
      x@settings[["background"]]
    )
  )
}

#' One Quarto callout per failing sample
#'
#' @noRd
report_callouts <- function(x) {
  failures <- qc_failures(x)
  if (nrow(failures) == 0) {
    return("No sample fails a quality-control threshold.")
  }
  ids <- failures[[x@settings[["id_colname"]]]]
  unlist(lapply(seq_len(nrow(failures)), function(i) {
    c(
      "::: {.callout-warning}",
      paste0("## `", ids[i], "`"),
      "",
      failures[["reason"]][i],
      ":::",
      ""
    )
  }))
}

#' Markdown lines for the thresholds that can flag a sample
#'
#' Leaves out infinite bounds and bounds at the edge of the values a metric can
#' take, since neither can flag a sample.
#'
#' @noRd
report_thresholds <- function(x) {
  thresholds <- x@thresholds
  metrics <- qc_metrics[
    qc_metrics %in% intersect(names(x@samples), names(thresholds))
  ]
  lines <- vapply(
    metrics,
    function(metric) {
      limits <- thresholds[[metric]]
      lower <- limits[1]
      upper <- if (length(limits) == 2) limits[2] else Inf
      domain <- qc_metric_domains[[metric]] %||% c(-Inf, Inf)
      has_lower <- is.finite(lower) && lower > domain[1]
      has_upper <- is.finite(upper) && upper < domain[2]
      shown <- function(v) format(signif(v, 3))
      bounds <- if (has_lower && has_upper) {
        paste(shown(lower), "to", shown(upper))
      } else if (has_lower) {
        paste("at least", shown(lower))
      } else if (has_upper) {
        paste("at most", shown(upper))
      } else {
        return(NA_character_)
      }
      paste0("- ", qc_metric_labels[[metric]], " (`", metric, "`): ", bounds)
    },
    character(1)
  )
  unname(lines[!is.na(lines)])
}

#' The sections of the report that apply to the object
#'
#' @param group A column of `nacho_samples(x)` with the biological groups, or
#'   `NULL`.
#'
#' @noRd
report_sections <- function(x, group = NULL) {
  section <- function(
    title,
    level,
    plot = NA_character_,
    help = NA_character_
  ) {
    data.frame(
      title = title,
      level = level,
      plot = plot,
      help = help,
      alt = if (is.na(plot)) NA_character_ else plot_alt_texts[[plot]]
    )
  }
  n_housekeeping <- sum(x@probes[["is_housekeeping"]])
  has_house_factor <- "House_factor" %in%
    names(x@samples) &&
    any(!is.na(x@samples[["House_factor"]]))
  metrics <- if (x@rcc_type == "n8") {
    c("BD", "FoV")
  } else {
    c("BD", "FoV", "PCL", "LoD")
  }
  rows <- c(
    list(section("Quality-control metrics", 1)),
    lapply(metrics, function(metric) {
      section(qc_metric_labels[[metric]], 2, metric, tolower(metric))
    }),
    list(
      section("Control probes", 1),
      section("Positive controls", 2, "Positive"),
      section("Negative controls", 2, "Negative")
    ),
    if (n_housekeeping > 0) {
      list(section("Housekeeping genes", 2, "Housekeeping"))
    },
    list(
      section("Positive against negative controls", 2, "PN"),
      section("Counts", 1),
      section("Average count against binding density", 2, "ACBD"),
      section("Average count against median count", 2, "ACMC"),
      section("Principal components", 1),
      section("First two components", 2, "PCA12"),
      section("Planes of the first components", 2, "PCA"),
      section("Variance explained", 2, "PCAi"),
      section("Normalisation", 1),
      section("Positive against negative factor", 2, "PFNF", "pf")
    ),
    if (has_house_factor) list(section("Housekeeping factor", 2, "HF", "hgf")),
    list(
      section("Normalisation result", 2, "NORM"),
      section("Relative log expression", 2, "RLE")
    ),
    if (n_housekeeping >= 3) {
      list(section("Housekeeping gene stability", 2, "Stability"))
    },
    list(
      section("Batch effects", 1),
      section("Normalisation factors by cartridge", 2, "BatchFactors"),
      section("Principal components and batches", 2, "PCBatch")
    )
  )
  do.call(rbind, rows)
}

#' Design and cross-tables of the batch diagnostics
#'
#' @noRd
report_batch_tables <- function(x, group) {
  diagnostics <- batch_diagnostics(x, group = group)
  design <- diagnostics[["design"]]
  crosstabs <- diagnostics[["crosstabs"]]
  c(
    if (any(design[["confounded"]])) {
      c(
        "::: {.callout-important}",
        "## Batch and biology are confounded",
        "",
        "At least one batch level holds a single group, so no normalisation can tell batch from biology.",
        ":::",
        ""
      )
    },
    knitr::kable(design, digits = 2),
    "",
    unlist(lapply(names(crosstabs), function(batch) {
      c(
        paste0("Groups by `", batch, "`:"),
        "",
        knitr::kable(as.data.frame.matrix(crosstabs[[batch]])),
        ""
      )
    }))
  )
}

#' Check the options of the report
#'
#' @inheritParams render
#'
#' @return The options, as a list.
#'
#' @noRd
check_report_options <- function(
  x,
  colour = "CartridgeID",
  group = NULL,
  size = 1,
  show_legend = TRUE,
  outliers_factor = 1,
  outliers_labels = NULL,
  call = rlang::caller_env()
) {
  samples <- nacho_samples(x)
  check_string(colour, call = call)
  check_column(colour, samples, data_arg = "nacho_samples(x)", call = call)
  check_string(group, allow_null = TRUE, call = call)
  if (!is.null(group)) {
    check_column(group, samples, data_arg = "nacho_samples(x)", call = call)
  }
  check_string(outliers_labels, allow_null = TRUE, call = call)
  if (!is.null(outliers_labels)) {
    check_column(
      outliers_labels,
      samples,
      data_arg = "nacho_samples(x)",
      call = call
    )
  }
  check_bool(show_legend, call = call)
  for (value in list(size = size, outliers_factor = outliers_factor)) {
    if (
      !is.numeric(value) ||
        length(value) != 1 ||
        !is.finite(value) ||
        value <= 0
    ) {
      nacho_abort(
        "{.arg size} and {.arg outliers_factor} must be single positive numbers.",
        class = "bad_argument",
        call = call
      )
    }
  }
  list(
    colour = colour,
    group = group,
    size = size,
    show_legend = show_legend,
    outliers_factor = outliers_factor,
    outliers_labels = outliers_labels
  )
}

#' Read and check what render() saved for the report
#'
#' @param path The `.rds` file.
#'
#' @noRd
report_setup <- function(path) {
  saved <- readRDS(path)
  check_nacho(saved[["object"]], arg = "object")
  options <- do.call(
    check_report_options,
    c(list(x = saved[["object"]]), saved[["options"]])
  )
  list(
    object = saved[["object"]],
    options = options,
    sections = report_sections(saved[["object"]], options[["group"]])
  )
}

#' Print the body of the report
#'
#' Run in a Quarto chunk with `output: asis`.
#'
#' @noRd
report_body <- function(report) {
  x <- report[["object"]]
  options <- report[["options"]]
  sections <- report[["sections"]]
  for (i in seq_len(nrow(sections))) {
    cat(
      "\n\n",
      strrep("#", sections[["level"]][i]),
      " ",
      sections[["title"]][i],
      "\n\n",
      sep = ""
    )
    if (
      sections[["title"]][i] == "Batch effects" && !is.null(options[["group"]])
    ) {
      cat(report_batch_tables(x, options[["group"]]), sep = "\n")
    }
    if (!is.na(sections[["help"]][i])) {
      cat(help_text(sections[["help"]][i]), "\n\n")
    }
    if (!is.na(sections[["plot"]][i])) {
      plot <- withCallingHandlers(
        autoplot(
          x,
          type = sections[["plot"]][i],
          colour = options[["colour"]],
          size = options[["size"]],
          show_legend = options[["show_legend"]],
          outliers_factor = options[["outliers_factor"]],
          outliers_labels = options[["outliers_labels"]]
        ),
        nacho_warning_metric_unavailable = function(cnd) {
          invokeRestart("muffleWarning")
        }
      )
      print(plot)
      cat("\n\n")
    }
  }
  invisible(report)
}

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
  instrument <- thresholds[["instrument"]]
  cat(
    "  - Thresholds preset:",
    paste0(
      thresholds[["preset"]],
      ", instrument: ",
      if (is.na(instrument)) "unknown" else instrument
    ),
    "\n"
  )
  threshold_labels <- c(
    BD = "Binding Density (BD)",
    FoV = "Field of View (FoV)",
    PCL = "Positive Control Linearity (PCL)",
    LoD = "Limit of Detection (LoD)",
    Positive_factor = "Positive normalisation factor (Positive_factor)",
    House_factor = "Housekeeping normalisation factor (house_factor)",
    Housekeeping_detected = "Housekeeping genes above background (Housekeeping_detected)",
    Ligation_order = "Ligation controls in order (Ligation_order)",
    Ligation_R2 = "Ligation controls linearity (Ligation_R2)",
    Ligation_NEG = "Ligation negative above detection limit (Ligation_NEG)",
    Haemolysis = "Haemolysis (Haemolysis)"
  )
  threshold_lines <- list()
  reported <- intersect(names(x@samples), names(thresholds))
  for (metric in qc_metrics[qc_metrics %in% reported]) {
    label <- if (metric %in% names(threshold_labels)) {
      threshold_labels[[metric]]
    } else {
      metric
    }
    limits <- thresholds[[metric]]
    threshold_lines <- c(
      threshold_lines,
      list(list(paste(label, "<"), limits[1])),
      if (length(limits) == 2) list(list(paste(label, ">"), limits[2]))
    )
  }
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
