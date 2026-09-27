#' @include nacho-class.R
NULL

#' Rebuild the NACHO 2 long layout
#'
#' One row per probe and sample, with every sample column, the PCA scores,
#' `CodeClass`, `Name`, `Accession`, `Count` and `Count_Norm`.
#'
#' @param x A `nacho` object.
#' @param rows Probe row indices to keep; `NULL` keeps every probe.
#'
#' @keywords internal
#' @noRd
long_table <- function(x, rows = NULL) {
  if (is.null(rows)) {
    rows <- seq_len(nrow(x@counts))
  }
  counts <- x@counts[rows, , drop = FALSE]
  normalised <- x@normalised[rows, , drop = FALSE]
  n_probes <- nrow(counts)
  n_samples <- ncol(counts)
  sample_rows <- rep(seq_len(n_samples), each = n_probes)
  probe_rows <- rep(seq_len(n_probes), times = n_samples)
  scores <- as.data.frame(x@pca[["scores"]])
  probes <- x@probes[rows, c("CodeClass", "Name", "Accession"), drop = FALSE]
  pieces <- list(
    x@samples[sample_rows, , drop = FALSE],
    if (ncol(scores) > 0) scores[sample_rows, , drop = FALSE],
    probes[probe_rows, , drop = FALSE],
    data.frame(Count = as.vector(counts), Count_Norm = as.vector(normalised))
  )
  long <- do.call(cbind, Filter(Negate(is.null), pieces))
  rownames(long) <- NULL
  data.table::as.data.table(long)
}

format_nacho <- function(x, ...) {
  housekeeping <- x@probes[["Name"]][x@probes[["is_housekeeping"]]]
  kind <- if (x@rcc_type == "n8") "PlexSet" else "single-sample"
  uses_housekeeping <- isTRUE(x@settings[["housekeeping_norm"]]) &&
    length(housekeeping) > 0
  c(
    cli::format_inline(
      "<nacho> {ncol(x@counts)} sample{?s}, {nrow(x@counts)} probe{?s}, from ",
      kind,
      " RCC files"
    ),
    cli::format_inline(
      "Normalisation: {x@settings[['normalisation_method']]}, ",
      if (uses_housekeeping) {
        "with {length(housekeeping)} housekeeping gene{?s}"
      } else {
        "without housekeeping genes"
      }
    ),
    cli::format_inline(
      "Flagged samples: {sum(x@samples[['is_outlier']] %in% TRUE)} of {ncol(x@counts)}"
    ),
    cli::format_inline(
      "Created with NACHO {x@provenance[['nacho_version']]}"
    )
  )
}

print_nacho <- function(x, ...) {
  cat(format_nacho(x), sep = "\n")
  invisible(x)
}

summary_nacho <- function(object, ...) {
  metrics <- c("BD", "FoV", "PCL", "LoD", "Positive_factor", "House_factor")
  if (object@rcc_type == "n8") {
    metrics <- setdiff(metrics, c("PCL", "LoD"))
  }
  metrics <- intersect(metrics, names(object@samples))
  thresholds <- object@thresholds
  data.frame(
    metric = metrics,
    lower = vapply(metrics, function(m) min(thresholds[[m]]), numeric(1)),
    upper = vapply(
      metrics,
      function(m) {
        if (length(thresholds[[m]]) == 2) max(thresholds[[m]]) else NA_real_
      },
      numeric(1)
    ),
    n_fail = vapply(
      metrics,
      function(m) sum(metric_fails(object@samples[[m]], thresholds[[m]])),
      integer(1)
    ),
    n_missing = vapply(
      metrics,
      function(m) sum(is.na(object@samples[[m]])),
      integer(1)
    ),
    row.names = NULL
  )
}

as_data_frame_nacho <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  long = FALSE
) {
  check_bool(long)
  if (long) {
    return(as.data.frame(long_table(x)))
  }
  nacho_samples(x)
}

#' Basic methods for a nacho object
#'
#' @section Usage:
#' ```r
#' print(x)
#' format(x)
#' summary(object)
#' dim(x)
#' as.data.frame(x, long = FALSE)
#' x[i, j]
#' ```
#'
#' @section Arguments:
#' * `x`, `object`: A `nacho` object.
#' * `long`: If `TRUE`, `as.data.frame()` returns one row per probe and
#'   sample (the NACHO 2 `$nacho` layout) instead of one row per sample.
#' * `i`: Probes to keep.
#' * `j`: Samples to keep.
#'
#' @name nacho-methods
#' @usage NULL
NULL

S7::method(print, nacho) <- print_nacho
S7::method(format, nacho) <- format_nacho
S7::method(summary, nacho) <- summary_nacho
S7::method(dim, nacho) <- function(x) dim(x@counts)
S7::method(as.data.frame, nacho) <- as_data_frame_nacho
