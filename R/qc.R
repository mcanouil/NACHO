#' Quality-control thresholds used by NACHO 2
#'
#' @keywords internal
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

#' Tell which values fail a threshold
#'
#' Two limits give a range; one limit is a lower bound.
#' Missing values never fail.
#'
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
#' @noRd
compute_pca <- function(counts, n_comp) {
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
  "House_factor",
  "is_outlier"
)

#' Geometric mean of each column
#'
#' Zeros count as 1 and missing probes are ignored.
#'
#' @keywords internal
#' @noRd
geometric_means <- function(m) {
  m[m == 0] <- 1
  exp(colMeans(log(m), na.rm = TRUE))
}

#' Negative probes whose median is far from the overall median
#'
#' A probe is excluded when its median is more than 50 % away from the median
#' of every negative count; when every probe would be excluded, none is.
#'
#' @keywords internal
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
#' @keywords internal
#' @noRd
control_concentrations <- function(names) {
  pattern <- "^[^(]*\\((.*)\\)$"
  if (all(grepl(pattern, names))) {
    return(as.numeric(sub(pattern, "\\1", names)))
  }
  unname(c(NEG = 0, POS = 32)[sub("(NEG).*|(POS).*", "\\1\\2", names)])
}

#' Positive and negative normalisation factors of each sample
#'
#' @keywords internal
#' @noRd
control_factors <- function(counts, probes, excluded, method) {
  names <- probes[["Name"]]
  code_class <- probes[["CodeClass"]]
  used <- code_class %in%
    c("Positive", "Negative") &
    !names %in% c("POS_F(0.125)", excluded)
  if (method == "GEO") {
    positive <- geometric_means(
      counts[used & code_class == "Positive", , drop = FALSE]
    )
    negative <- geometric_means(
      counts[used & code_class == "Negative", , drop = FALSE]
    )
    return(list(
      positive_factor = mean(positive) / positive,
      negative_factor = negative
    ))
  }
  concentration <- control_concentrations(names[used])
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
  list(
    positive_factor = mean(coefficients[2, ]) / coefficients[2, ],
    negative_factor = coefficients[1, ]
  )
}

#' Geometric mean of the background-corrected housekeeping counts
#'
#' Corrected counts below 1 are floored at 1.
#'
#' @keywords internal
#' @noRd
housekeeping_geometric_means <- function(
  counts,
  negative_factor,
  positive_factor
) {
  corrected <- sweep(counts, 2, negative_factor, "-")
  corrected <- sweep(corrected, 2, positive_factor, "*")
  corrected[!is.na(corrected) & corrected < 1] <- 1
  geometric_means(corrected)
}

#' Normalise a count matrix
#'
#' Background-corrected and scaled counts are rounded, then floored at 0.1.
#'
#' @keywords internal
#' @noRd
normalise_matrix <- function(
  counts,
  negative_factor,
  positive_factor,
  house_factor
) {
  out <- sweep(counts, 2, negative_factor, "-")
  out <- sweep(out, 2, positive_factor, "*")
  if (!is.null(house_factor)) {
    out <- sweep(out, 2, house_factor, "*")
  }
  out <- round(out)
  out[!is.na(out) & out <= 0] <- 0.1
  out
}

#' Predict the five most stable housekeeping genes
#'
#' @keywords internal
#' @noRd
predict_housekeeping <- function(
  counts,
  probes,
  negative_factor,
  positive_factor
) {
  rows <- grepl("Endogenous|Housekeeping", probes[["CodeClass"]])
  normalised <- normalise_matrix(
    counts[rows, , drop = FALSE],
    negative_factor,
    positive_factor,
    house_factor = NULL
  )
  ratios <- log2(sweep(normalised, 2, colMeans(normalised, na.rm = TRUE), "/"))
  ratios[is.infinite(ratios)] <- NA
  spread <- sort(apply(ratios, 1, stats::sd, na.rm = TRUE))
  names(spread)[seq_len(min(5, length(spread)))]
}

#' Positive control linearity of each sample
#'
#' The squared Pearson correlation of log2 counts on log2 concentrations.
#'
#' @keywords internal
#' @noRd
sample_pcl <- function(positives, names) {
  zero <- colSums(positives == 0, na.rm = TRUE) > 0
  measured <- log2(positives)
  measured[, zero] <- log2(positives[, zero, drop = FALSE] + 1)
  known <- log2(suppressWarnings(as.numeric(sub(
    "^[^(]*\\((.*)\\)$",
    "\\1",
    names
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
#' @keywords internal
#' @noRd
sample_lod <- function(pos_e, negatives) {
  spread <- apply(negatives, 2, stats::sd, na.rm = TRUE)
  z <- (as.vector(pos_e) - colMeans(negatives, na.rm = TRUE)) / spread
  z[is.na(spread) | spread == 0] <- NA_real_
  round(z, 2)
}

#' Per-sample quality-control metrics
#'
#' @keywords internal
#' @noRd
sample_metrics <- function(counts, probes, samples) {
  names <- probes[["Name"]]
  code_class <- probes[["CodeClass"]]
  positive <- code_class == "Positive"
  pos_e <- which(positive & grepl("POS_E", names))
  if (length(pos_e) == 1) {
    pcl <- sample_pcl(counts[positive, , drop = FALSE], names[positive])
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
  missing <- c(missing_lane, if (missing_date) "Date")
  if (length(missing) > 0) {
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
#'
#' @keywords internal
#' @noRd
build_nacho <- function(
  counts,
  probes,
  samples,
  settings,
  thresholds,
  rcc_type,
  provenance
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

  if (isTRUE(settings[["housekeeping_predict"]])) {
    nacho_inform("Searching for the best housekeeping genes.")
    predicted <- predict_housekeeping(
      counts,
      probes,
      factors[["negative_factor"]],
      factors[["positive_factor"]]
    )
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
    geometric <- housekeeping_geometric_means(
      counts[probes[["Name"]] %in% housekeeping_genes, , drop = FALSE],
      factors[["negative_factor"]],
      factors[["positive_factor"]]
    )
    house_factor <- mean(geometric) / geometric
  }

  metrics <- sample_metrics(counts, probes, samples)
  metrics[["Positive_factor"]] <- unname(factors[["positive_factor"]])
  metrics[["Negative_factor"]] <- unname(factors[["negative_factor"]])
  if (!is.null(house_factor)) {
    metrics[["House_factor"]] <- unname(house_factor)
  }
  samples <- cbind(samples, metrics)
  samples[["is_outlier"]] <- compute_outliers(samples, thresholds, rcc_type)

  normalised <- normalise_matrix(
    counts,
    factors[["negative_factor"]],
    factors[["positive_factor"]],
    if (isTRUE(settings[["housekeeping_norm"]])) house_factor
  )
  storage.mode(normalised) <- "double"

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
