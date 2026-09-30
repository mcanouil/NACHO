#' Quality-control thresholds used by NACHO 2
#'
#' @noRd
default_thresholds <- function() {
  list(
    BD = c(0.1, 2.25),
    FoV = 75,
    LoD = 2,
    PCL = 0.95,
    Positive_factor = c(1 / 4, 4),
    House_factor = c(1 / 11, 11)
  )
}

#' Settings for data that do not come from NACHO
#'
#' The defaults match those of [load_rcc()], which a test checks, except
#' `housekeeping_norm`, which is on only when the probes include
#' `Housekeeping` ones.
#'
#' @param probes The probe table, with a `CodeClass` column.
#' @param id_colname The name of the sample id column.
#'
#' @noRd
default_settings <- function(probes, id_colname) {
  list(
    id_colname = id_colname,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = any(grepl("Housekeeping", probes[["CodeClass"]])),
    normalisation_method = "GEO",
    background = "none",
    background_mode = "threshold",
    n_comp = 10L
  )
}

#' Tell which values fail a threshold
#'
#' Two limits give a range; one limit is a lower bound.
#' Missing values never fail.
#'
#' @noRd
metric_fails <- function(values, limits) {
  if (length(limits) == 2) {
    !is.na(values) & (values < min(limits) | values > max(limits))
  } else {
    !is.na(values) & values < limits
  }
}

#' Flag samples that fail any quality-control threshold
#'
#' PCL and LoD are not flagged for PlexSet files, whose controls are shared by
#' the eight samples of a lane.
#'
#' @noRd
compute_outliers <- function(samples, thresholds, rcc_type) {
  metrics <- c(
    "BD",
    "FoV",
    "Positive_factor",
    if ("House_factor" %in% names(samples)) "House_factor",
    if (identical(rcc_type, "n1")) c("PCL", "LoD")
  )
  fails <- lapply(metrics, function(metric) {
    metric_fails(samples[[metric]], thresholds[[metric]])
  })
  Reduce(`|`, fails, rep(FALSE, nrow(samples)))
}

#' Principal component analysis of the samples
#'
#' Sample scores of `log(counts + 1)` over every probe, as in NACHO 2.0.7.
#' The results match `stats::prcomp()` and `summary()` on it, up to the sign
#' of each component.
#' An eigen decomposition of the smaller cross product replaces the full
#' singular value decomposition, which is about twice as slow at 768 samples.
#' Each component then gets a fixed sign through `fix_pca_signs()`, since
#' `eigen()` returns eigenvectors of arbitrary sign that can otherwise differ
#' between machines.
#'
#' @noRd
compute_pca <- function(counts, n_comp) {
  hint <- "This is an internal error in NACHO; the object settings may have been changed through {.code @}."
  if (!rlang::is_scalar_integerish(n_comp, finite = TRUE)) {
    nacho_abort(
      c(
        "{.arg n_comp} must be a whole number, not {.obj_type_friendly {n_comp}}.",
        i = hint
      ),
      class = "internal"
    )
  }
  if (n_comp < 0) {
    nacho_abort(
      c(
        "{.arg n_comp} must be at least 0, not {.val {n_comp}}.",
        i = hint
      ),
      class = "internal"
    )
  }
  n_samples <- ncol(counts)
  n_probes <- nrow(counts)
  max_comp <- max(min(n_samples - 1L, n_probes), 0L)
  if (n_comp > max_comp) {
    nacho_warn(
      c(
        "{.arg n_comp} = {n_comp} is more than the {max_comp} component{?s} available.",
        i = "Using {.code n_comp = {max_comp}}."
      ),
      class = "n_comp_reduced"
    )
    n_comp <- max_comp
  }
  if (anyNA(counts)) {
    nacho_warn(
      c(
        "{sum(is.na(counts))} missing count{?s} were set to 0 before PCA.",
        i = "Probes absent from some RCC files usually mean mixed CodeSets."
      ),
      class = "missing_counts"
    )
    counts[is.na(counts)] <- 0L
  }
  components <- sprintf("PC%02d", seq_len(n_comp))
  if (n_comp == 0) {
    return(list(
      scores = matrix(
        numeric(0),
        nrow = n_samples,
        ncol = 0,
        dimnames = list(colnames(counts), NULL)
      ),
      importance = data.frame(
        PC = character(0),
        "Standard deviation" = numeric(0),
        "Proportion of Variance" = numeric(0),
        "Cumulative Proportion" = numeric(0),
        check.names = FALSE
      )
    ))
  }
  centred <- scale(t(log(counts + 1)), center = TRUE, scale = FALSE)
  keep <- seq_len(n_comp)
  if (n_samples <= n_probes) {
    decomposition <- eigen(tcrossprod(centred), symmetric = TRUE)
    values <- pmax(decomposition[["values"]][keep], 0)
    scores <- decomposition[["vectors"]][, keep, drop = FALSE] *
      rep(sqrt(values), each = n_samples)
  } else {
    decomposition <- eigen(crossprod(centred), symmetric = TRUE)
    values <- pmax(decomposition[["values"]][keep], 0)
    scores <- centred %*% decomposition[["vectors"]][, keep, drop = FALSE]
  }
  dimnames(scores) <- list(colnames(counts), components)
  scores <- fix_pca_signs(scores)
  variance <- values / max(1, n_samples - 1)
  proportion <- values / sum(centred^2)
  list(
    scores = scores,
    importance = data.frame(
      PC = components,
      "Standard deviation" = sqrt(variance),
      "Proportion of Variance" = round(proportion, 5),
      "Cumulative Proportion" = round(cumsum(proportion), 5),
      check.names = FALSE
    )
  )
}

#' Give each principal component a fixed sign
#'
#' `eigen()` returns eigenvectors of arbitrary sign, which differs from
#' `prcomp()` and can differ between LAPACK builds.
#' Each column is flipped, if needed, so the score with the largest absolute
#' value is positive; the first such sample wins a tie.
#'
#' @noRd
fix_pca_signs <- function(scores) {
  for (component in seq_len(ncol(scores))) {
    largest <- which.max(abs(scores[, component]))
    if (scores[largest, component] < 0) {
      scores[, component] <- -scores[, component]
    }
  }
  scores
}

#' Record where an object comes from
#'
#' @noRd
new_provenance <- function(data_directory, file_version, software_version) {
  list(
    nacho_version = as.character(utils::packageVersion("NACHO")),
    schema_version = nacho_schema_version,
    file_version = unique(as.character(file_version)),
    software_version = unique(as.character(software_version)),
    data_directory = data_directory,
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#' Sample columns computed by the quality-control pipeline
#'
#' @noRd
computed_sample_columns <- c(
  "Date",
  "ID",
  "BD",
  "ScannerID",
  "StagePosition",
  "CartridgeID",
  "FoV",
  "PCL",
  "LoD",
  "MC",
  "MedC",
  "Positive_factor",
  "Negative_factor",
  "Background",
  "House_factor",
  "is_outlier"
)

#' Geometric mean of each column
#'
#' Zeros count as 1 and missing probes are ignored.
#'
#' @noRd
geometric_means <- function(m) {
  m[m == 0] <- 1
  exp(colMeans(log(m), na.rm = TRUE))
}

#' Background statistics and modes
#'
#' @noRd
background_statistics <- c("none", "mean", "mean_2sd", "median", "max", "geo")
background_modes <- c("threshold", "subtract")

#' Background level of each sample, from its kept negative controls
#'
#' @param counts Count matrix of probes by samples, with probe names as row
#'   names.
#' @param code_class The code class of each row of `counts`.
#' @param excluded Names of the negative probes left out.
#' @param statistic One of `background_statistics`.
#'
#' @return One level per sample, or `NULL` for `"none"`.
#'
#' @noRd
background_levels <- function(
  counts,
  code_class,
  excluded,
  statistic,
  call = rlang::caller_env()
) {
  if (statistic == "none") {
    return(NULL)
  }
  negatives <- counts[
    code_class == "Negative" & !rownames(counts) %in% excluded,
    ,
    drop = FALSE
  ]
  if (nrow(negatives) == 0) {
    nacho_abort(
      c(
        "{.code background = {.val {statistic}}} needs negative control probes, and there are none.",
        i = "Use {.code background = \"none\"}."
      ),
      class = "bad_argument",
      call = call
    )
  }
  if (statistic == "mean_2sd" && nrow(negatives) < 2) {
    nacho_abort(
      c(
        "{.code background = \"mean_2sd\"} needs at least two negative probes, and {nrow(negatives)} {?is/are} left.",
        i = "Use another {.arg background} statistic."
      ),
      class = "bad_argument",
      call = call
    )
  }
  level <- switch(
    statistic,
    mean = colMeans(negatives, na.rm = TRUE),
    mean_2sd = colMeans(negatives, na.rm = TRUE) +
      2 * apply(negatives, 2, stats::sd, na.rm = TRUE),
    median = apply(negatives, 2, stats::median, na.rm = TRUE),
    max = apply(negatives, 2, max, na.rm = TRUE),
    geo = geometric_means(negatives)
  )
  unname(level)
}

#' Apply a background level to every probe of each sample
#'
#' `"threshold"` raises counts below the level to the level, as Bruker
#' recommends when fold changes matter; `"subtract"` removes the level and
#' floors at 0.
#' Missing counts stay missing.
#'
#' @noRd
apply_background <- function(counts, level, mode) {
  if (is.null(level)) {
    return(counts * 1)
  }
  levels <- matrix(level, nrow(counts), ncol(counts), byrow = TRUE)
  out <- if (mode == "threshold") {
    pmax(counts, levels)
  } else {
    pmax(counts - levels, 0)
  }
  dimnames(out) <- dimnames(counts)
  out
}

#' Negative probes whose median is far from the overall median
#'
#' A probe is excluded when its median is more than 50 % away from the median
#' of every negative count; when every probe would be excluded, none is.
#'
#' @noRd
excluded_negatives <- function(counts, code_class) {
  negatives <- counts[code_class == "Negative", , drop = FALSE]
  if (nrow(negatives) == 0) {
    return(character(0))
  }
  overall <- stats::median(negatives, na.rm = TRUE)
  medians <- apply(negatives, 1, stats::median, na.rm = TRUE)
  excluded <- rownames(negatives)[abs(overall - medians) > 0.5 * overall]
  if (length(excluded) == nrow(negatives)) character(0) else excluded
}

#' Known concentrations of the control probes
#'
#' Read from the name, for example `POS_A(128)`, or 0 for negatives and 32 for
#' positives when some names carry no concentration.
#'
#' @noRd
control_concentrations <- function(probe_names) {
  pattern <- "^[^(]*\\((.*)\\)$"
  if (all(grepl(pattern, probe_names))) {
    return(as.numeric(sub(pattern, "\\1", probe_names)))
  }
  unname(c(NEG = 0, POS = 32)[sub("(NEG).*|(POS).*", "\\1\\2", probe_names)])
}

#' Positive and negative normalisation factors of each sample
#'
#' @noRd
control_factors <- function(counts, probes, excluded, method) {
  probe_names <- probes[["Name"]]
  code_class <- probes[["CodeClass"]]
  used <- code_class %in%
    c("Positive", "Negative") &
    !probe_names %in% c("POS_F(0.125)", excluded)
  if (method == "GEO") {
    positive <- geometric_means(
      counts[used & code_class == "Positive", , drop = FALSE]
    )
    return(list(positive_factor = mean(positive) / positive))
  }
  concentration <- control_concentrations(probe_names[used])
  controls <- counts[used, , drop = FALSE]
  coefficients <- vapply(
    seq_len(ncol(controls)),
    function(k) {
      keep <- !is.na(controls[, k])
      fit <- stats::glm(
        y ~ x,
        family = stats::poisson(link = "identity"),
        data = data.frame(x = concentration[keep], y = controls[keep, k] + 1)
      )
      unname(stats::coef(fit)[1:2])
    },
    numeric(2)
  )
  list(positive_factor = mean(coefficients[2, ]) / coefficients[2, ])
}

#' Apply the background, then the positive factor
#'
#' @noRd
scale_counts <- function(counts, background, background_mode, positive_factor) {
  out <- apply_background(counts, background, background_mode)
  sweep(out, 2, positive_factor, "*")
}

#' Content normalisation factor from reference rows
#'
#' Values below 1 count as 1, so a gene at background does not pull the
#' geometric mean towards 0.
#'
#' @param scaled_rows The reference rows of the background-applied,
#'   positive-scaled counts.
#'
#' @noRd
content_factor <- function(scaled_rows) {
  scaled_rows[!is.na(scaled_rows) & scaled_rows < 1] <- 1
  geometric <- geometric_means(scaled_rows)
  mean(geometric) / geometric
}

#' Predict the five most stable housekeeping genes
#'
#' @param scaled The background-applied, positive-scaled counts.
#' @param probes The probe table, with a `CodeClass` column.
#'
#' @noRd
predict_housekeeping <- function(scaled, probes) {
  rows <- grepl("Endogenous|Housekeeping", probes[["CodeClass"]])
  normalised <- scaled[rows, , drop = FALSE]
  # Rounds and floors as NACHO 2 did, to reproduce its gene selection until
  # this function is replaced; the stored counts stay unrounded.
  normalised <- round(normalised)
  normalised[!is.na(normalised) & normalised <= 0] <- 0.1
  ratios <- log2(sweep(normalised, 2, colMeans(normalised, na.rm = TRUE), "/"))
  ratios[is.infinite(ratios)] <- NA
  spread <- sort(apply(ratios, 1, stats::sd, na.rm = TRUE))
  names(spread)[seq_len(min(5, length(spread)))]
}

#' Positive control linearity of each sample
#'
#' The squared Pearson correlation of log2 counts on log2 concentrations.
#'
#' @noRd
sample_pcl <- function(positives, probe_names) {
  zero <- colSums(positives == 0, na.rm = TRUE) > 0
  measured <- log2(positives)
  measured[, zero] <- log2(positives[, zero, drop = FALSE] + 1)
  known <- log2(suppressWarnings(as.numeric(sub(
    "^[^(]*\\((.*)\\)$",
    "\\1",
    probe_names
  ))))
  round(
    apply(measured, 2, function(m) {
      stats::cor(m, known, use = "complete.obs")^2
    }),
    5
  )
}

#' Limit of detection of each sample
#'
#' The z-score of POS_E against the negatives, or `NA` when they do not vary.
#'
#' @noRd
sample_lod <- function(pos_e, negatives) {
  spread <- apply(negatives, 2, stats::sd, na.rm = TRUE)
  z <- (as.vector(pos_e) - colMeans(negatives, na.rm = TRUE)) / spread
  z[is.na(spread) | spread == 0] <- NA_real_
  round(z, 2)
}

#' Per-sample quality-control metrics
#'
#' @noRd
sample_metrics <- function(counts, probes, samples, warn_missing = TRUE) {
  probe_names <- probes[["Name"]]
  code_class <- probes[["CodeClass"]]
  positive <- code_class == "Positive"
  pos_e <- which(positive & grepl("POS_E", probe_names))
  if (length(pos_e) == 1) {
    pcl <- sample_pcl(counts[positive, , drop = FALSE], probe_names[positive])
    lod <- sample_lod(
      counts[pos_e, , drop = FALSE],
      counts[code_class == "Negative", , drop = FALSE]
    )
  } else {
    pcl <- lod <- rep(NA_real_, ncol(counts))
  }
  endogenous <- counts[grepl("Endogenous", code_class), , drop = FALSE]
  lane_names <- c(
    "ID",
    "BindingDensity",
    "ScannerID",
    "StagePosition",
    "CartridgeID",
    "FovCounted",
    "FovCount"
  )
  missing_lane <- lane_names[
    !paste0("Lane_Attributes.lane_", lane_names) %in% names(samples)
  ]
  missing_date <- !"Sample_Attributes.sample_Date" %in% names(samples)
  missing <- c(
    if (length(missing_lane) > 0) paste0("Lane_Attributes.lane_", missing_lane),
    if (missing_date) "Sample_Attributes.sample_Date"
  )
  if (warn_missing && length(missing) > 0) {
    nacho_warn(
      c(
        "Some lane or sample attributes are missing, so the metrics that need them are {.val NA}.",
        i = "Missing: {.field {missing}}."
      ),
      class = "metric_unavailable"
    )
  }
  lane <- function(name) {
    column <- paste0("Lane_Attributes.lane_", name)
    if (column %in% names(samples)) {
      samples[[column]]
    } else {
      rep(NA_character_, nrow(samples))
    }
  }
  date <- if (missing_date) {
    rep(NA_character_, nrow(samples))
  } else {
    samples[["Sample_Attributes.sample_Date"]]
  }
  data.frame(
    Date = date,
    ID = lane("ID"),
    BD = as.numeric(lane("BindingDensity")),
    ScannerID = lane("ScannerID"),
    StagePosition = lane("StagePosition"),
    CartridgeID = lane("CartridgeID"),
    FoV = round(
      as.numeric(lane("FovCounted")) / as.numeric(lane("FovCount")) * 100,
      2
    ),
    PCL = unname(pcl),
    LoD = unname(lod),
    MC = unname(round(colMeans(endogenous, na.rm = TRUE), 2)),
    MedC = unname(apply(endogenous, 2, stats::median, na.rm = TRUE))
  )
}

#' Build a nacho object from count matrices
#'
#' Computes every quality-control metric, normalisation factor, normalised
#' count, principal component and outlier flag.
#' Computed columns already in `samples` are replaced.
#'
#' @param counts Integer matrix of probes by samples, with probe names as row
#'   names and sample ids as column names.
#' @param probes Data frame with `CodeClass`, `Name` and `Accession`, one row
#'   per row of `counts`.
#' @param samples Data frame with the id column first and the RCC attribute
#'   columns, one row per column of `counts`.
#' @param warn_missing Whether to warn about missing lane or sample
#'   attributes.
#'   Only a first build warns, so rebuilding an object does not repeat it.
#'
#' @noRd
build_nacho <- function(
  counts,
  probes,
  samples,
  settings,
  thresholds,
  rcc_type,
  provenance,
  warn_missing = TRUE
) {
  probes <- as.data.frame(probes)[, c("CodeClass", "Name", "Accession")]
  samples <- as.data.frame(samples)
  samples <- samples[,
    setdiff(
      names(samples),
      c(
        computed_sample_columns,
        grep("^PC[0-9]+$", names(samples), value = TRUE)
      )
    ),
    drop = FALSE
  ]
  code_class <- probes[["CodeClass"]]

  housekeeping_genes <- settings[["housekeeping_genes"]]
  if (is.null(housekeeping_genes) && any(grepl("Housekeeping", code_class))) {
    housekeeping_genes <- unique(probes[["Name"]][grepl(
      "Housekeeping",
      code_class
    )])
  }
  excluded <- excluded_negatives(counts, code_class)
  factors <- control_factors(
    counts,
    probes,
    excluded,
    settings[["normalisation_method"]]
  )
  background <- background_levels(
    counts,
    code_class,
    excluded,
    settings[["background"]]
  )
  scaled <- scale_counts(
    counts,
    background,
    settings[["background_mode"]],
    factors[["positive_factor"]]
  )

  if (isTRUE(settings[["housekeeping_predict"]])) {
    nacho_inform("Searching for the best housekeeping genes.")
    predicted <- predict_housekeeping(scaled, probes)
    if (length(predicted) == 0) {
      nacho_warn(
        "No suitable housekeeping genes were found; the default ones are used.",
        class = "no_housekeeping"
      )
    } else {
      nacho_inform(c(
        "Normalising with the predicted housekeeping genes:",
        stats::setNames(predicted, rep("*", length(predicted)))
      ))
      housekeeping_genes <- predicted
    }
  }

  house_factor <- NULL
  if (!is.null(housekeeping_genes)) {
    house_factor <- content_factor(
      scaled[probes[["Name"]] %in% housekeeping_genes, , drop = FALSE]
    )
  }

  metrics <- sample_metrics(counts, probes, samples, warn_missing)
  metrics[["Positive_factor"]] <- unname(factors[["positive_factor"]])
  negatives <- code_class == "Negative" & !probes[["Name"]] %in% excluded
  metrics[["Negative_factor"]] <- if (any(negatives)) {
    unname(geometric_means(counts[negatives, , drop = FALSE]))
  } else {
    NA_real_
  }
  metrics[["Background"]] <- background %||% NA_real_
  if (!is.null(house_factor)) {
    metrics[["House_factor"]] <- unname(house_factor)
  }
  samples <- cbind(samples, metrics)
  samples[["is_outlier"]] <- compute_outliers(samples, thresholds, rcc_type)

  normalised <- if (
    isTRUE(settings[["housekeeping_norm"]]) && !is.null(house_factor)
  ) {
    sweep(scaled, 2, house_factor, "*")
  } else {
    scaled
  }

  probes[["is_housekeeping"]] <- probes[["Name"]] %in% housekeeping_genes
  probes[["is_excluded"]] <- probes[["Name"]] %in% excluded
  settings[["housekeeping_genes"]] <- housekeeping_genes

  nacho(
    counts = counts,
    normalised = normalised,
    probes = probes,
    samples = samples,
    settings = settings,
    thresholds = thresholds,
    pca = compute_pca(counts, settings[["n_comp"]]),
    rcc_type = rcc_type,
    provenance = provenance
  )
}
