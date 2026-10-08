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
  Positive_factor = "Positive normalization factor",
  House_factor = "Content normalization factor",
  Housekeeping_detected = "Housekeeping genes above background",
  Ligation_order = "Ligation controls in order",
  Ligation_R2 = "Ligation control linearity",
  Ligation_NEG = "Ligation negative above the detection limit",
  Haemolysis = "Hemolysis"
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
  PFNF = "Positive normalization factor against the negative factor, one point per sample, with the thresholds shaded.",
  HF = "Content normalization factor against the positive factor, one point per sample, with the thresholds shaded.",
  NORM = "Control or housekeeping gene counts before and after normalization, one line per probe.",
  Stability = "geNorm stability of each housekeeping gene, from the most to the least stable.",
  RLE = "Relative log expression of each sample after normalization.",
  BatchFactors = "Normalization factors of each sample, grouped by cartridge.",
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

#' One row per failing sample and failing metric
#'
#' On PlexSet files, a sample whose lane fails a lane metric gets a row for
#' it, with the value of the lane sample that fails and the label marked
#' "(lane)".
#'
#' @param x A `nacho` object.
#' @param qc The quality-control table of `x`.
#'
#' @return A data frame with `sample`, `cartridge`, `lane`, `metric`,
#'   `metric_label`, `value` and `limit`, ordered as the samples.
#'
#' @noRd
qc_failure_rows <- function(x, qc = nacho_qc(x)) {
  metrics <- qc_metrics[paste0(qc_metrics, "_status") %in% names(qc)]
  lanes <- x@rcc_type == "n8" && "lane" %in% names(qc)
  lane_key <- if (lanes) paste(qc[["CartridgeID"]], qc[["lane"]], sep = "\r")
  rows <- lapply(metrics, function(metric) {
    fails <- qc[[paste0(metric, "_status")]] %in% "fail"
    source <- seq_len(nrow(qc))
    inherited <- rep(FALSE, nrow(qc))
    if (lanes && metric %in% lane_metrics) {
      failing <- which(fails)
      first_failing <- failing[match(lane_key, lane_key[failing])]
      inherited <- !is.na(first_failing) & !fails
      source[inherited] <- first_failing[inherited]
    }
    keep <- which(fails | inherited)
    if (length(keep) == 0) {
      return(NULL)
    }
    data.frame(
      row = keep,
      order = match(metric, qc_metrics),
      metric = metric,
      metric_label = paste0(
        qc_metric_labels[[metric]],
        ifelse(inherited[keep], " (lane)", "")
      ),
      value = trimws(formatC(
        qc[[metric]][source[keep]],
        digits = 4,
        format = "fg"
      )),
      limit = threshold_bounds(x, metric)
    )
  })
  rows <- do.call(rbind, rows)
  rows <- rows[order(rows[["row"]], rows[["order"]]), , drop = FALSE]
  column <- function(name) {
    if (name %in% names(qc)) {
      as.character(qc[[name]][rows[["row"]]])
    } else {
      rep(NA_character_, nrow(rows))
    }
  }
  data.frame(
    sample = column(x@settings[["id_colname"]]),
    cartridge = column("CartridgeID"),
    lane = if (lanes) column("lane") else rep(NA_character_, nrow(rows)),
    rows[c("metric", "metric_label", "value", "limit")],
    row.names = NULL
  )
}

#' Make text show literally in a Markdown table cell
#'
#' Escapes the Markdown punctuation with a backslash and writes `&`, `<` and
#' `>` as HTML entities, which Pandoc reads in both HTML and Typst output.
#'
#' @noRd
md_escape <- function(text) {
  text <- gsub(
    "([][!\"#$%'()*+,./:;=?@\\\\^_`{|}~-])",
    "\\\\\\1",
    text,
    perl = TRUE
  )
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  gsub(">", "&gt;", text, fixed = TRUE)
}

#' Markdown lines for the decision summary of the report
#'
#' Three counts in a `nacho-verdict` div, then a table of the flagged samples
#' with one row per failing metric.
#'
#' @param x A `nacho` object.
#'
#' @noRd
report_decisions <- function(x) {
  qc <- nacho_qc(x)
  overview <- app_overview(x, qc)
  passed <- overview$samples - overview$flagged
  n_reasons <- length(overview$reasons)
  count <- function(n, one, many) {
    paste(if (n == 1) one else many)
  }
  boxes <- c(
    "::: {.nacho-verdict}",
    paste0(
      "[",
      passed,
      "]{.n} ",
      count(passed, "sample passes", "samples pass"),
      " every check"
    ),
    "",
    paste0(
      "[",
      overview$flagged,
      "]{.n .flag} ",
      count(overview$flagged, "sample is", "samples are"),
      " flagged"
    ),
    "",
    paste0(
      "[",
      n_reasons,
      "]{.n} ",
      count(n_reasons, "metric drives", "metrics drive"),
      " the flags"
    ),
    ":::",
    ""
  )
  if (overview$flagged == 0) {
    return(c(boxes, "Every sample passes every check."))
  }
  rows <- qc_failure_rows(x, qc)
  lanes <- x@rcc_type == "n8" && "lane" %in% names(qc)
  unit <- if (lanes) {
    paste0(md_escape(rows[["cartridge"]]), ", lane ", md_escape(rows[["lane"]]))
  } else {
    ifelse(is.na(rows[["cartridge"]]), "", md_escape(rows[["cartridge"]]))
  }
  c(
    boxes,
    paste0(
      overview$flagged,
      " ",
      count(overview$flagged, "sample falls", "samples fall"),
      " outside at least one limit."
    ),
    "Look at them before you use the counts downstream.",
    if (passed > 0) {
      paste0(
        "The other ",
        passed,
        " ",
        count(passed, "sample passes", "samples pass"),
        " every check."
      )
    },
    "",
    paste0(
      "| Sample | ",
      if (lanes) "Lane" else "Cartridge",
      " | Metric | Value | Limit |"
    ),
    "|---|---|---|---:|---|",
    paste0(
      "| ",
      md_escape(rows[["sample"]]),
      " | ",
      unit,
      " | ",
      rows[["metric_label"]],
      " | ",
      rows[["value"]],
      " | ",
      rows[["limit"]],
      " |"
    )
  )
}

#' What each threshold means, in plain words
#'
#' @noRd
parameter_meanings <- c(
  BD = "Optical features per square micron; too high means codes overlap.",
  FoV = "Share of imaged fields the scanner could count.",
  PCL = "How well positive controls follow their known concentrations.",
  LoD = "How far the 0.5 fM positive control sits above background.",
  Positive_factor = "Scale applied to bring each sample's positive controls in line.",
  House_factor = "Scale applied to bring each sample's housekeeping genes in line.",
  Housekeeping_detected = "Housekeeping genes that clear background in each sample.",
  Ligation_order = "Whether ligation controls rank in their expected order.",
  Ligation_R2 = "How well ligation controls follow their known concentrations.",
  Ligation_NEG = "Largest ligation negative control minus the detection limit.",
  Haemolysis = "Ratio of miR-451a to miR-23a-3p; high values point to hemolysis."
)

#' What each normalisation method does, in plain words
#'
#' @noRd
method_meanings <- c(
  GEO = "Positive controls, then housekeeping genes, scale each sample by their geometric mean.",
  GLM = "Positive controls, then housekeeping genes, scale each sample through a Poisson model.",
  RUVg = "Positive controls scale each sample, then unwanted variation found in the housekeeping genes is removed.",
  stable_mirna = "The five most stable miRNAs scale each sample.",
  total_mirna = "The miRNAs above 50 counts scale each sample.",
  spike_in = "The spike-in controls scale each sample.",
  ligation = "The ligation positive controls scale each sample."
)

#' Where the value of a threshold comes from
#'
#' @return `"your choice"` when the value differs from its preset, otherwise
#'   `"NACHO 2"` for the legacy preset, the instrument for binding density,
#'   `"NACHO"` for the miRNA metrics Bruker gives no limit for, and
#'   `"nSolver"` for the others.
#'
#' @noRd
threshold_source <- function(x, metric) {
  preset <- x@thresholds[["preset"]]
  instrument <- x@thresholds[["instrument"]]
  if (is.na(instrument)) {
    instrument <- "max"
  }
  references <- lapply(c(FALSE, TRUE), function(haemolysis) {
    nacho_thresholds(instrument, preset, haemolysis = haemolysis)[[metric]]
  })
  value <- as.numeric(x@thresholds[[metric]])
  same <- vapply(
    references,
    function(reference) {
      length(value) == length(reference) &&
        isTRUE(all.equal(value, as.numeric(reference)))
    },
    logical(1)
  )
  if (!any(same)) {
    "your choice"
  } else if (preset == "legacy") {
    "NACHO 2"
  } else if (metric == "BD") {
    toupper(instrument)
  } else if (
    metric %in% c("Ligation_order", "Ligation_R2", "Ligation_NEG", "Haemolysis")
  ) {
    "NACHO"
  } else {
    "nSolver"
  }
}

#' A source as a tag for both report formats
#'
#' @noRd
source_tag <- function(source) {
  ifelse(
    source == "your choice",
    "[your choice]{.tag .user}",
    paste0("[", source, "]{.tag}")
  )
}

#' The settings that shape the data, with their meaning and source
#'
#' The defaults are those of [load_rcc()].
#' The default housekeeping genes are the `Housekeeping` probes of the RCC
#' files.
#' A RUVg factor count is the default when [suggest_ruv_k()] suggests the same
#' count; when it cannot suggest one, the count is the user's choice.
#'
#' @return A data frame with `parameter`, `value` (escaped for Markdown),
#'   `meaning` and `source`.
#'
#' @noRd
setting_rows <- function(x) {
  settings <- x@settings
  defaults <- formals(load_rcc)
  default <- function(name) eval(defaults[[name]])
  method <- settings[["normalisation_method"]]
  background <- settings[["background"]]
  mode <- settings[["background_mode"]]
  predict <- isTRUE(settings[["housekeeping_predict"]])
  genes <- settings[["housekeeping_genes"]]
  probe_genes <- x@probes[["Name"]][
    grepl("Housekeeping", x@probes[["CodeClass"]])
  ]
  panel <- settings[["panel"]] %||% "mrna"
  default_norm <- (panel == "mrna" || predict) &&
    (length(probe_genes) > 0 || predict)
  rows <- list(
    c(
      "Normalization method",
      md_escape(method),
      method_meanings[[method]],
      identical(method, default("normalisation_method"))
    ),
    c(
      "Background",
      md_escape(
        if (background == "none") background else paste0(background, ", ", mode)
      ),
      if (background == "none") {
        "Counts are used as read, with no background from the negative controls."
      } else if (mode == "subtract") {
        "The negative-control background is subtracted from each count, floored at 0."
      } else {
        "Counts below the negative-control background are raised to it."
      },
      identical(background, default("background")) &&
        identical(mode, default("background_mode"))
    )
  )
  if (method %in% c("GEO", "GLM", "RUVg")) {
    rows <- c(
      rows,
      list(
        c(
          "Housekeeping genes",
          if (length(genes) == 0) {
            "none"
          } else {
            paste(md_escape(genes), collapse = ", ")
          },
          "Genes used to correct for differences in sample input.",
          !predict && setequal(genes, probe_genes)
        ),
        c(
          "Housekeeping prediction",
          if (predict) "yes" else "no",
          "Whether NACHO picked the most stable genes as housekeeping genes.",
          predict == default("housekeeping_predict")
        ),
        c(
          "Housekeeping normalization",
          if (isTRUE(settings[["housekeeping_norm"]])) "yes" else "no",
          "Whether the housekeeping genes scale each sample.",
          isTRUE(settings[["housekeeping_norm"]]) == default_norm
        )
      )
    )
  }
  if (identical(method, "RUVg")) {
    suggested <- tryCatch(
      {
        table <- suggest_ruv_k(x)
        table[["k"]][table[["suggested"]]]
      },
      nacho_error = function(cnd) NULL
    )
    rows <- c(
      rows,
      list(c(
        "RUV factors",
        settings[["ruv_k"]] %||% "none",
        "Factors of unwanted variation removed from the counts.",
        !is.null(suggested) && identical(suggested, settings[["ruv_k"]])
      ))
    )
  }
  rows <- c(
    rows,
    list(c(
      "Principal components",
      settings[["n_comp"]],
      "Components computed for the principal component plots.",
      settings[["n_comp"]] == default("n_comp")
    ))
  )
  rows <- do.call(rbind, rows)
  data.frame(
    parameter = rows[, 1],
    value = rows[, 2],
    meaning = rows[, 3],
    source = ifelse(rows[, 4] == "TRUE", "default", "your choice")
  )
}

#' Markdown lines for the settings and thresholds of the report
#'
#' One row per threshold that can flag a sample, then one per setting, each
#' with its value, its meaning and where the value comes from.
#'
#' @param x A `nacho` object.
#'
#' @noRd
report_parameters <- function(x) {
  metrics <- report_metrics(x)
  bounds <- vapply(metrics, threshold_bounds, character(1), x = x)
  metrics <- metrics[!is.na(bounds)]
  settings <- setting_rows(x)
  parameter <- c(unname(qc_metric_labels[metrics]), settings[["parameter"]])
  value <- c(unname(bounds[metrics]), settings[["value"]])
  meaning <- c(unname(parameter_meanings[metrics]), settings[["meaning"]])
  source <- c(
    vapply(metrics, threshold_source, character(1), x = x, USE.NAMES = FALSE),
    settings[["source"]]
  )
  c(
    "These are the values NACHO used for this report.",
    "Anyone can rerun the analysis with the same settings and get the same result.",
    "",
    "| Parameter | Value | What it means | Source |",
    "|---|---|---|---|",
    paste0(
      "| ",
      parameter,
      " | ",
      value,
      " | ",
      meaning,
      " | ",
      source_tag(source),
      " |"
    )
  )
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

#' Metrics whose thresholds the report shows
#'
#' PlexSet files have no positive control linearity or limit of detection.
#'
#' @noRd
report_metrics <- function(x) {
  metrics <- qc_metrics[
    qc_metrics %in% intersect(names(x@samples), names(x@thresholds))
  ]
  if (x@rcc_type == "n8") {
    metrics <- setdiff(metrics, plexset_unassessed)
  }
  metrics
}

#' The limits of a metric in words
#'
#' @return `"0.05 to 2.25"`, `"at least 75"` or `"at most 3"`, or `NA` when
#'   the bounds are infinite or at the edge of the values the metric can take,
#'   since such bounds cannot flag a sample.
#'
#' @noRd
threshold_bounds <- function(x, metric) {
  limits <- x@thresholds[[metric]]
  lower <- min(limits)
  upper <- if (length(limits) == 2) max(limits) else Inf
  domain <- qc_metric_domains[[metric]] %||% c(-Inf, Inf)
  has_lower <- is.finite(lower) && lower > domain[1]
  has_upper <- is.finite(upper) && upper < domain[2]
  shown <- function(v) format(signif(v, 3))
  if (has_lower && has_upper) {
    paste(shown(lower), "to", shown(upper))
  } else if (has_lower) {
    paste("at least", shown(lower))
  } else if (has_upper) {
    paste("at most", shown(upper))
  } else {
    NA_character_
  }
}

#' Markdown lines for the thresholds that can flag a sample
#'
#' Leaves out the bounds that [threshold_bounds()] cannot put in words.
#'
#' @noRd
report_thresholds <- function(x) {
  metrics <- report_metrics(x)
  bounds <- vapply(metrics, threshold_bounds, character(1), x = x)
  keep <- !is.na(bounds)
  paste0(
    "- ",
    qc_metric_labels[metrics[keep]],
    " (`",
    metrics[keep],
    "`): ",
    bounds[keep]
  )
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

#' The plot types that make sense for the object
#'
#' Shared by the report and the app, so both leave out the same plots.
#'
#' @noRd
applicable_plots <- function(x) {
  has_house_factor <- "House_factor" %in%
    names(x@samples) &&
    any(!is.na(x@samples[["House_factor"]]))
  has_batch <- length(intersect(
    c("CartridgeID", "Date"),
    names(nacho_samples(x))
  )) >
    0
  has_components <- ncol(x@pca[["scores"]]) >= 2
  n_housekeeping <- sum(x@probes[["is_housekeeping"]])
  has_stability <- n_housekeeping >= 3 &&
    !is.null(tryCatch(
      housekeeping_stability(x),
      nacho_error = function(cnd) NULL
    ))
  c(
    "BD",
    "FoV",
    if (x@rcc_type != "n8") c("PCL", "LoD"),
    "Positive",
    "Negative",
    if (n_housekeeping > 0) "Housekeeping",
    "PN",
    "ACBD",
    "ACMC",
    if (has_components) c("PCA12", "PCA"),
    "PCAi",
    "PFNF",
    if (has_house_factor) "HF",
    "NORM",
    "RLE",
    if (has_stability) "Stability",
    "BatchFactors",
    if (has_batch && has_components) "PCBatch"
  )
}

#' The sections of the report that apply to the object
#'
#' @noRd
report_sections <- function(x) {
  plots <- applicable_plots(x)
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
  plot_section <- function(plot, title, level = 2, help = NA_character_) {
    if (plot %in% plots) section(title, level, plot, help)
  }
  rows <- c(
    list(section("Quality-control metrics", 1)),
    lapply(intersect(c("BD", "FoV", "PCL", "LoD"), plots), function(metric) {
      section(qc_metric_labels[[metric]], 2, metric, tolower(metric))
    }),
    list(
      section("Control probes", 1),
      plot_section("Positive", "Positive controls"),
      plot_section("Negative", "Negative controls"),
      plot_section("Housekeeping", "Housekeeping genes"),
      plot_section("PN", "Positive against negative controls"),
      section("Counts", 1),
      plot_section("ACBD", "Average count against binding density"),
      plot_section("ACMC", "Average count against median count"),
      section("Principal components", 1),
      plot_section("PCA12", "First two components"),
      plot_section("PCA", "Planes of the first components"),
      plot_section("PCAi", "Variance explained"),
      section("Normalisation", 1),
      plot_section("PFNF", "Positive against negative factor", help = "pf"),
      plot_section("HF", "Content normalization factor", help = "hgf"),
      plot_section("NORM", "Normalisation result"),
      plot_section("RLE", "Relative log expression"),
      plot_section("Stability", "Housekeeping gene stability"),
      section("Batch effects", 1, batch = TRUE),
      plot_section("BatchFactors", "Normalisation factors by cartridge"),
      plot_section("PCBatch", "Principal components and batches")
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
#' The report takes its title, author and cover fields from the metadata that
#' [render()] passes, so only [render()] is supported.
#' A title in the template would win over that metadata.
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

#' Cover fields of the report
#'
#' Quarto reads these as document metadata, so the title, the author and the
#' `nacho.*` fields reach the cover partials.
#' The date uses English month names whatever the locale.
#'
#' @param x A `nacho` object.
#' @param title The report title, or `NULL` or empty for the default.
#' @param author Who prepared the report, or `NULL` or blank for nobody.
#'
#' The date is not a field: the cover reads it from `nacho$prepared`, because
#' Quarto would reformat a `date` field in the PDF.
#'
#' @noRd
report_metadata <- function(x, title = NULL, author = NULL) {
  blank <- function(value) is.null(value) || !nzchar(trimws(value))
  overview <- app_overview(x)
  today <- Sys.Date()
  date <- paste0(
    month.name[as.integer(format(today, "%m"))],
    " ",
    as.integer(format(today, "%d")),
    ", ",
    format(today, "%Y")
  )
  author <- if (blank(author)) NULL else trimws(author)
  rcc_version <- x@provenance[["file_version"]]
  if (length(rcc_version) != 1 || is.na(rcc_version)) {
    rcc_version <- "unknown"
  }
  metadata <- list(
    title = if (blank(title)) "NanoString quality-control report" else title,
    author = author,
    nacho = list(
      prepared = if (is.null(author)) {
        paste("Prepared on", date)
      } else {
        paste0("Prepared by ", author, " \u00b7 ", date)
      },
      samples = as.character(overview$samples),
      unit = overview$unit,
      units = as.character(overview$units),
      flagged = as.character(overview$flagged),
      method = overview$method,
      rcc_version = rcc_version,
      nacho_version = as.character(utils::packageVersion("NACHO")),
      r_version = paste(R.version$major, R.version$minor, sep = ".")
    )
  )
  metadata[lengths(metadata) > 0]
}

#' Make cover text reach the document as typed
#'
#' Quarto reads the title, the author and `nacho$prepared` as Markdown, so
#' smart quotes, emphasis and HTML would change them.
#' A backslash before each ASCII punctuation character keeps them literal.
#'
#' @param metadata The list from [report_metadata()].
#'
#' @noRd
report_metadata_literal <- function(metadata) {
  literal <- function(value) {
    gsub("([[:punct:]])", "\\\\\\1", value)
  }
  for (field in intersect(c("title", "author"), names(metadata))) {
    metadata[[field]] <- literal(metadata[[field]])
  }
  metadata$nacho$prepared <- literal(metadata$nacho$prepared)
  metadata
}

#' Check the title or the author of the report
#'
#' `NULL` and the empty string mean "not given".
#'
#' @noRd
check_cover_text <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (is.null(x) || identical(x, "")) {
    return(invisible(x))
  }
  check_string(x, arg = arg, call = call)
}
