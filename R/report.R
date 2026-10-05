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
  Ligation_R2 = c(0, 1),
  Housekeeping_detected = c(0, Inf)
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
  HF = "Content normalisation factor against the positive factor, one point per sample, with the thresholds shaded.",
  NORM = "Control or housekeeping gene counts before and after normalisation, one line per probe.",
  Stability = "geNorm stability of each housekeeping gene, from the most to the least stable.",
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
  path <- system.file("about", paste0("about-", name, ".md"), package = "NACHO")
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
#' @param qc The quality-control table of `x`.
#'
#' @noRd
qc_failures <- function(x, qc = nacho_qc(x)) {
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
  if (x@rcc_type == "n8") {
    metrics <- setdiff(metrics, c("PCL", "LoD"))
  }
  lines <- vapply(
    metrics,
    function(metric) {
      limits <- thresholds[[metric]]
      lower <- min(limits)
      upper <- if (length(limits) == 2) max(limits) else Inf
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

#' Markdown lines for the settings that shape the data
#'
#' @noRd
report_settings <- function(x) {
  settings <- x@settings
  housekeeping <- settings[["housekeeping_genes"]]
  c(
    paste0(
      "- Housekeeping genes: ",
      if (length(housekeeping) == 0) {
        "none"
      } else {
        paste(housekeeping, collapse = ", ")
      }
    ),
    paste0(
      "- Housekeeping genes predicted: ",
      if (isTRUE(settings[["housekeeping_predict"]])) "yes" else "no"
    ),
    paste0(
      "- Normalised with housekeeping genes: ",
      if (isTRUE(settings[["housekeeping_norm"]])) "yes" else "no"
    ),
    paste0("- Principal components: ", settings[["n_comp"]]),
    if (identical(settings[["normalisation_method"]], "RUVg")) {
      paste0("- RUV factors: ", settings[["ruv_k"]])
    }
  )
}

#' The sections of the report that apply to the object
#'
#' @noRd
report_sections <- function(x) {
  section <- function(
    title,
    level,
    plot = NA_character_,
    help = NA_character_,
    batch = FALSE
  ) {
    data.frame(
      title = title,
      level = level,
      plot = plot,
      help = help,
      batch = batch,
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
  has_batch <- length(intersect(
    c("CartridgeID", "Date"),
    names(nacho_samples(x))
  )) >
    0
  has_components <- ncol(x@pca[["scores"]]) >= 2
  has_stability <- n_housekeeping >= 3 &&
    !is.null(tryCatch(
      housekeeping_stability(x),
      nacho_error = function(cnd) NULL
    ))
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
      if (has_components) section("First two components", 2, "PCA12"),
      if (has_components) section("Planes of the first components", 2, "PCA"),
      section("Variance explained", 2, "PCAi"),
      section("Normalisation", 1),
      section("Positive against negative factor", 2, "PFNF", "pf")
    ),
    if (has_house_factor) {
      list(section("Content normalisation factor", 2, "HF", "hgf"))
    },
    list(
      section("Normalisation result", 2, "NORM"),
      section("Relative log expression", 2, "RLE")
    ),
    if (has_stability) {
      list(section("Housekeeping gene stability", 2, "Stability"))
    },
    list(
      section("Batch effects", 1, batch = TRUE),
      section("Normalisation factors by cartridge", 2, "BatchFactors"),
      if (has_batch && has_components) {
        section("Principal components and batches", 2, "PCBatch")
      }
    )
  )
  do.call(rbind, rows)
}

#' Design and cross-tables of the batch diagnostics
#'
#' @noRd
report_batch_tables <- function(x, group) {
  batch <- intersect(c("CartridgeID", "Date"), names(nacho_samples(x)))
  if (length(batch) == 0L) {
    return(c(
      "The samples have no `CartridgeID` or `Date` column, so there is no batch design to show.",
      ""
    ))
  }
  diagnostics <- batch_diagnostics(x, group = group, batch = batch)
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
    unlist(lapply(names(crosstabs), function(column) {
      c(
        paste0("Groups by `", column, "`:"),
        "",
        knitr::kable(as.data.frame.matrix(crosstabs[[column]])),
        ""
      )
    }))
  )
}

#' Check that a report option is a single positive number
#'
#' @noRd
check_positive_number <- function(value, arg, call = rlang::caller_env()) {
  if (
    !is.numeric(value) ||
      length(value) != 1 ||
      !is.finite(value) ||
      value <= 0
  ) {
    nacho_abort(
      "{.arg {arg}} must be a single positive number.",
      class = "bad_argument",
      call = call
    )
  }
  invisible(value)
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
  check_positive_number(size, "size", call = call)
  check_positive_number(outliers_factor, "outliers_factor", call = call)
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
  if (!file.exists(path)) {
    nacho_abort(
      c(
        "The report data {.file {path}} does not exist.",
        i = "Use {.fn render} to build the report."
      ),
      class = "bad_object"
    )
  }
  saved <- readRDS(path)
  if (!is.list(saved) || !"object" %in% names(saved)) {
    nacho_abort(
      c(
        "The report data {.file {path}} is not a list with an {.field object}.",
        i = "Use {.fn render} to build the report."
      ),
      class = "bad_object"
    )
  }
  check_nacho(saved[["object"]], arg = "object")
  known <- c(
    "colour",
    "group",
    "size",
    "show_legend",
    "outliers_factor",
    "outliers_labels"
  )
  saved_options <- saved[["options"]]
  options <- do.call(
    check_report_options,
    c(
      list(x = saved[["object"]]),
      saved_options[intersect(names(saved_options), known)]
    )
  )
  list(
    object = saved[["object"]],
    options = options,
    sections = report_sections(saved[["object"]])
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
    if (sections[["batch"]][i] && !is.null(options[["group"]])) {
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
          cat(cli::ansi_strip(conditionMessage(cnd)), "\n\n")
          invokeRestart("muffleWarning")
        }
      )
      print(plot)
      cat("\n\n")
    }
  }
  invisible(report)
}
