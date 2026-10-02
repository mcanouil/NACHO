#' Settings for data that do not come from NACHO
#'
#' The defaults match those of [load_rcc()], which a test checks.
#' `housekeeping_norm` is `NULL`, and `nacho_metadata_settings()` resolves it
#' from the panel and the probes.
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
    housekeeping_norm = NULL,
    normalisation_method = "GEO",
    ruv_k = NULL,
    background = "none",
    background_mode = "threshold",
    n_comp = 10L,
    panel = NULL
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
#' Includes `is_outlier`, which NACHO 2 stored and `nacho_qc()` now replaces.
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
  "Detection_rate",
  "Background",
  "House_factor",
  "Housekeeping_detected",
  "Ligation_order",
  "Ligation_R2",
  "Ligation_NEG",
  "Haemolysis",
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
#' @param warn Whether to warn about samples without a background level.
#'
#' @return One level per sample, or `NULL` for `"none"`.
#'   A sample without a usable negative count gets `NA`.
#'
#' @noRd
background_levels <- function(
  counts,
  code_class,
  excluded,
  statistic,
  warn = TRUE,
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
    max = apply(negatives, 2, function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }),
    geo = geometric_means(negatives)
  )
  level[is.nan(level)] <- NA_real_
  unavailable <- colnames(counts)[is.na(level)]
  if (warn && length(unavailable) > 0) {
    nacho_warn(
      c(
        "{length(unavailable)} sample{?s} ha{?s/ve} no usable negative counts, so the background is {.code NA}.",
        i = "Affected: {.val {unavailable}}."
      ),
      class = "metric_unavailable",
      call = call
    )
  }
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
    storage.mode(counts) <- "double"
    return(counts)
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

#' Negative probes to leave out
#'
#' `"nsolver"` follows Bruker: drop the one or two negative probes whose mean
#' count sits more than 3-fold above every other negative, when at least four
#' negatives exist.
#' `"legacy"` keeps the NACHO 2 rule: drop probes whose median is more than
#' 50 % away from the median of every negative count, unless that drops them
#' all.
#'
#' @noRd
excluded_negatives <- function(counts, code_class, preset) {
  negatives <- counts[code_class == "Negative", , drop = FALSE]
  if (nrow(negatives) == 0) {
    return(character(0))
  }
  if (preset == "legacy") {
    overall <- stats::median(negatives, na.rm = TRUE)
    medians <- apply(negatives, 1, stats::median, na.rm = TRUE)
    excluded <- rownames(negatives)[which(
      abs(overall - medians) > 0.5 * overall
    )]
    return(if (length(excluded) == nrow(negatives)) character(0) else excluded)
  }
  if (nrow(negatives) < 4) {
    return(character(0))
  }
  means <- sort(rowMeans(negatives, na.rm = TRUE), decreasing = TRUE)
  if (length(means) < 4) {
    return(character(0))
  }
  floored <- pmax(means, 1)
  n_high <- if (floored[2] > 3 * floored[3]) {
    2L
  } else if (floored[1] > 3 * floored[2]) {
    1L
  } else {
    0L
  }
  names(means)[seq_len(n_high)]
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

#' Normalisation methods
#'
#' @noRd
nacho_normalisation_methods <- c(
  "GEO",
  "GLM",
  "RUVg",
  "stable_mirna",
  "total_mirna",
  "spike_in",
  "ligation"
)

#' Positive normalisation factor of each sample
#'
#' @noRd
control_factors <- function(counts, probes, excluded, method) {
  probe_names <- probes[["Name"]]
  code_class <- probes[["CodeClass"]]
  used <- code_class %in%
    c("Positive", "Negative") &
    !probe_names %in% c("POS_F(0.125)", excluded)
  positive <- geometric_means(
    counts[used & code_class == "Positive", , drop = FALSE]
  )
  geo <- list(
    positive_factor = mean(positive) / positive,
    glm_failed = character(0)
  )
  if (method != "GLM") {
    return(geo)
  }
  concentration <- control_concentrations(probe_names[used])
  controls <- counts[used, , drop = FALSE]
  slopes <- vapply(
    seq_len(ncol(controls)),
    function(k) glm_slope(concentration, controls[, k]),
    numeric(1)
  )
  if (anyNA(slopes)) {
    geo[["glm_failed"]] <- colnames(counts)[is.na(slopes)]
    return(geo)
  }
  list(positive_factor = mean(slopes) / slopes, glm_failed = character(0))
}

#' Slope of a Poisson GLM with an identity link on the controls
#'
#' Counts plus 1 against known concentrations, started from the least
#' squares line, so the identity link starts inside its valid region.
#' A fit that does not converge, or whose slope or fitted values are not
#' positive, gives `NA`.
#'
#' @noRd
glm_slope <- function(concentration, counts) {
  keep <- !is.na(counts) & !is.na(concentration)
  if (sum(keep) < 2) {
    return(NA_real_)
  }
  data <- data.frame(x = concentration[keep], y = counts[keep] + 1)
  start <- stats::coef(stats::lm(y ~ x, data = data))
  if (!is.finite(start[[2]]) || start[[2]] <= 0) {
    return(NA_real_)
  }
  start <- c(max(start[[1]], 1), start[[2]])
  fit <- tryCatch(
    suppressWarnings(stats::glm(
      y ~ x,
      family = stats::poisson(link = "identity"),
      data = data,
      start = start,
      control = stats::glm.control(maxit = 100)
    )),
    error = function(cnd) NULL
  )
  if (is.null(fit) || !fit[["converged"]]) {
    return(NA_real_)
  }
  slope <- unname(stats::coef(fit)[[2]])
  if (!is.finite(slope) || slope <= 0 || any(stats::fitted(fit) <= 0)) {
    return(NA_real_)
  }
  slope
}

#' Content normalisation of the scaled counts
#'
#' @param counts The raw counts, which the miRNA methods use to pick their
#'   reference probes.
#' @param call The environment whose call names the function in errors.
#'
#' @return A list: `normalised`, `house_factor` (or `NULL`), `extra_columns`
#'   (a data frame of sample columns, possibly with no column), `settings` and
#'   `content_probes` (the miRNA reference probes, or `NULL`).
#'
#' @noRd
content_normalise <- function(
  scaled,
  counts,
  probes,
  settings,
  housekeeping_genes,
  call = rlang::caller_env()
) {
  none <- data.frame(row.names = seq_len(ncol(scaled)))
  if (settings[["normalisation_method"]] == "RUVg") {
    input <- ruv_input_from_scaled(
      scaled,
      probes,
      housekeeping_genes,
      call = call
    )
    k <- settings[["ruv_k"]]
    if (is.null(k)) {
      table <- ruv_k_table(
        input[["log_expr"]],
        input[["controls"]],
        5L,
        call = call
      )
      k <- table[["k"]][table[["suggested"]]]
      nacho_inform(
        "Using RUVg with {.code ruv_k = {k}}, as {.fn suggest_ruv_k} suggests."
      )
    }
    fit <- ruvg(input[["log_expr"]], input[["controls"]], k, call = call)
    normalised <- scaled
    normalised[input[["rows"]], ] <- pmax(2^t(fit[["corrected"]]) - 1, 0)
    settings[["ruv_k"]] <- as.integer(ncol(fit[["W"]]))
    extra <- as.data.frame(fit[["W"]])
    rownames(extra) <- NULL
    return(list(
      normalised = normalised,
      house_factor = NULL,
      extra_columns = extra,
      settings = settings,
      content_probes = NULL
    ))
  }
  if (settings[["normalisation_method"]] %in% mirna_methods) {
    if (!identical(settings[["panel"]], "mirna")) {
      nacho_abort(
        c(
          "{.code normalisation_method = {.val {settings[['normalisation_method']]}}} is for miRNA panels.",
          i = "Use {.val GEO}, {.val GLM} or {.val RUVg} for mRNA panels."
        ),
        class = "bad_argument",
        call = call
      )
    }
    reference <- mirna_reference(
      settings[["normalisation_method"]],
      counts,
      probes,
      call = call
    )
    house_factor <- content_factor(
      scaled[probes[["Name"]] %in% reference, , drop = FALSE]
    )
    settings["ruv_k"] <- list(NULL)
    return(list(
      normalised = sweep(scaled, 2, house_factor, "*"),
      house_factor = house_factor,
      extra_columns = none,
      settings = settings,
      content_probes = reference
    ))
  }
  use_housekeeping <- !identical(settings[["panel"]], "mirna") ||
    isTRUE(settings[["housekeeping_norm"]])
  house_factor <- if (!is.null(housekeeping_genes) && use_housekeeping) {
    content_factor(
      scaled[probes[["Name"]] %in% housekeeping_genes, , drop = FALSE]
    )
  }
  normalised <- if (
    isTRUE(settings[["housekeeping_norm"]]) && !is.null(house_factor)
  ) {
    sweep(scaled, 2, house_factor, "*")
  } else {
    scaled
  }
  settings["ruv_k"] <- list(NULL)
  list(
    normalised = normalised,
    house_factor = house_factor,
    extra_columns = none,
    settings = settings,
    content_probes = NULL
  )
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
  mean(geometric, na.rm = TRUE) / geometric
}

#' Predict the five most stable genes with geNorm
#'
#' @noRd
predict_housekeeping <- function(counts, probes) {
  rows <- grepl("Endogenous|Housekeeping", probes[["CodeClass"]])
  log_expr <- stability_input(
    counts[rows, , drop = FALSE],
    probes[["detection_rate"]][rows],
    min_detection = 0.9
  )
  if (ncol(log_expr) < 3 || nrow(log_expr) < 2) {
    return(character(0))
  }
  ranking <- genorm_ranking(log_expr)[["ranking"]]
  ranking[seq_len(min(5, length(ranking)))]
}

#' Positive control linearity of each sample
#'
#' The squared Pearson correlation of log2 counts on log2 concentrations.
#'
#' @noRd
sample_pcl <- function(positives, probe_names, preset) {
  known <- log2(suppressWarnings(as.numeric(sub(
    "^[^(]*\\((.*)\\)$",
    "\\1",
    probe_names
  ))))
  if (preset == "nsolver") {
    keep <- !grepl("^POS_F", probe_names)
    positives <- positives[keep, , drop = FALSE]
    known <- known[keep]
    measured <- log2(positives + 1)
  } else {
    zero <- colSums(positives == 0, na.rm = TRUE) > 0
    measured <- log2(positives)
    measured[, zero] <- log2(positives[, zero, drop = FALSE] + 1)
  }
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

#' Turn the NaN of a mean over nothing into a plain missing value
#'
#' @noRd
missing_not_nan <- function(x) {
  x <- unname(x)
  x[is.nan(x)] <- NA_real_
  x
}

#' Per-sample quality-control metrics
#'
#' @noRd
sample_metrics <- function(
  counts,
  probes,
  samples,
  preset,
  warn_missing = TRUE
) {
  probe_names <- probes[["Name"]]
  code_class <- probes[["CodeClass"]]
  positive <- code_class == "Positive"
  pos_e <- which(positive & grepl("POS_E", probe_names))
  if (length(pos_e) == 1) {
    pcl <- sample_pcl(
      counts[positive, , drop = FALSE],
      probe_names[positive],
      preset
    )
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
#' @param call The environment whose call names the function in errors.
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
  warn_missing = TRUE,
  call = rlang::caller_env()
) {
  probes <- as.data.frame(probes)[, c("CodeClass", "Name", "Accession")]
  samples <- as.data.frame(samples)
  samples <- samples[,
    setdiff(
      names(samples),
      c(
        computed_sample_columns,
        grep("^(PC|W_)[0-9]+$", names(samples), value = TRUE)
      )
    ),
    drop = FALSE
  ]
  code_class <- probes[["CodeClass"]]
  settings[["panel"]] <- detect_panel(probes, samples)

  housekeeping_genes <- settings[["housekeeping_genes"]]
  if (is.null(housekeeping_genes) && any(grepl("Housekeeping", code_class))) {
    housekeeping_genes <- unique(probes[["Name"]][grepl(
      "Housekeeping",
      code_class
    )])
  }
  preset <- thresholds[["preset"]]
  excluded <- excluded_negatives(counts, code_class, preset)
  factors <- control_factors(
    counts,
    probes,
    excluded,
    settings[["normalisation_method"]]
  )
  provenance[["glm_fallback"]] <- NULL
  if (length(factors[["glm_failed"]]) > 0) {
    # Read by the cli message below.
    failed <- cli::cli_vec(factors[["glm_failed"]], list("vec-trunc" = 5)) # nolint: object_usage_linter.
    nacho_warn(
      c(
        paste(
          "The positive control GLM did not fit",
          "{length(factors[['glm_failed']])} sample{?s},",
          "so NACHO used the geometric mean ({.val GEO}) instead."
        ),
        x = "Failed: {.val {failed}}.",
        i = "Check the positive controls of those samples with {.code autoplot(x, type = \"Positive\")}."
      ),
      class = "glm_convergence",
      call = call
    )
    settings[["normalisation_method"]] <- "GEO"
    provenance[["glm_fallback"]] <- factors[["glm_failed"]]
  }
  limits <- detection_limits(counts, code_class, excluded)
  hits <- detected(counts, limits)
  background <- background_levels(
    counts,
    code_class,
    excluded,
    settings[["background"]],
    warn = warn_missing
  )
  scaled <- scale_counts(
    counts,
    background,
    settings[["background_mode"]],
    factors[["positive_factor"]]
  )

  probes[["detection_rate"]] <- missing_not_nan(rowMeans(hits, na.rm = TRUE))

  if (
    isTRUE(settings[["housekeeping_predict"]]) &&
      !settings[["normalisation_method"]] %in% mirna_methods
  ) {
    nacho_inform("Searching for the best housekeeping genes.")
    predicted <- predict_housekeeping(counts, probes)
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

  content <- content_normalise(
    scaled,
    counts,
    probes,
    settings,
    housekeeping_genes,
    call = call
  )
  house_factor <- content[["house_factor"]]
  normalised <- content[["normalised"]]
  settings <- content[["settings"]]

  metrics <- sample_metrics(counts, probes, samples, preset, warn_missing)
  metrics[["Positive_factor"]] <- unname(factors[["positive_factor"]])
  negatives <- code_class == "Negative" & !probes[["Name"]] %in% excluded
  metrics[["Negative_factor"]] <- if (any(negatives)) {
    unname(geometric_means(counts[negatives, , drop = FALSE]))
  } else {
    NA_real_
  }
  metrics[["Background"]] <- background %||% NA_real_
  no_limit <- is.na(limits)
  if (any(no_limit) && warn_missing) {
    lacking <- colnames(counts)[no_limit]
    n_more <- max(length(lacking) - 5, 0)
    nacho_warn(
      c(
        paste(
          "{.field Detection_rate} and {.field Housekeeping_detected} need",
          "two kept negative probes with counts,",
          "so they are {.val NA} for some samples."
        ),
        i = paste0(
          "Samples without a detection limit: ",
          paste(utils::head(lacking, 5), collapse = ", "),
          if (n_more > 0) paste0(" and ", n_more, " more"),
          "."
        )
      ),
      class = "metric_unavailable"
    )
  }
  metrics[["Detection_rate"]] <- missing_not_nan(colMeans(
    hits[grepl("Endogenous", code_class), , drop = FALSE],
    na.rm = TRUE
  ))
  if (!is.null(house_factor)) {
    metrics[["House_factor"]] <- unname(house_factor)
  }
  housekeeping_rows <- probes[["Name"]] %in% housekeeping_genes
  skip_housekeeping <- settings[["panel"]] == "mirna" &&
    !isTRUE(settings[["housekeeping_norm"]])
  metrics[["Housekeeping_detected"]] <- if (
    skip_housekeeping || !any(housekeeping_rows)
  ) {
    NA_integer_
  } else {
    found <- colSums(hits[housekeeping_rows, , drop = FALSE], na.rm = TRUE)
    found[no_limit] <- NA
    unname(as.integer(found))
  }
  if (settings[["panel"]] == "mirna") {
    metrics <- cbind(metrics, ligation_metrics(counts, probes, limits))
    metrics[["Haemolysis"]] <- haemolysis_metric(counts, probes)
  }
  samples <- cbind(samples, metrics, content[["extra_columns"]])

  probes[["is_housekeeping"]] <- probes[["Name"]] %in% housekeeping_genes
  probes[["is_excluded"]] <- probes[["Name"]] %in% excluded
  provenance[["excluded_negatives"]] <- excluded
  provenance[["content_probes"]] <- content[["content_probes"]]
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
