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
  fit <- stats::prcomp(t(log(counts + 1)), rank. = n_comp)
  importance <- summary(fit)[["importance"]][, seq_len(n_comp), drop = FALSE]
  scores <- fit[["x"]][, seq_len(n_comp), drop = FALSE]
  colnames(scores) <- components
  list(
    scores = scores,
    importance = data.frame(
      PC = components,
      "Standard deviation" = unname(importance[1, ]),
      "Proportion of Variance" = unname(importance[2, ]),
      "Cumulative Proportion" = unname(importance[3, ]),
      check.names = FALSE
    )
  )
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
