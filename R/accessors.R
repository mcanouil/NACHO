#' @include nacho-class.R
NULL

#' Get the content of a nacho object
#'
#' These accessors are the only public way into a `nacho` object.
#'
#' @param x A `nacho` object from [load_rcc()] or [normalise()].
#' @param normalised If `TRUE`, return the normalised counts instead of the
#'   raw counts.
#' @param log2 If `TRUE`, return `log2(count + 1)`.
#'
#' @return
#' * `nacho_counts()`: a matrix with one row per probe and one column per
#'   sample.
#' * `nacho_samples()`: a data frame with one row per sample; the first column
#'   holds the sample ids, and the PCA scores come last (`PC01`, `PC02`, ...).
#'   `Detection_rate` (the share of endogenous genes detected) and
#'   `Housekeeping_detected` (the number of housekeeping genes detected) are
#'   `NA` for a sample without a detection limit.
#' * `nacho_probes()`: a data frame with one row per probe: `CodeClass`,
#'   `Name`, `Accession`, `detection_rate` (the share of samples with a
#'   detection limit in which the probe is above it), `is_housekeeping` and
#'   `is_excluded`.
#' * `nacho_qc()`: a data frame with one row per sample: the id, `lane`,
#'   `CartridgeID`, each quality-control metric with its status (`"pass"`,
#'   `"fail"` or `NA` when it cannot be assessed), the other factors,
#'   `lane_status` for PlexSet files, `n_flags` (the sample's own failing
#'   metrics, not those inherited from its lane), the overall `status` and a
#'   readable `reason` (which also names the lane metrics a sample inherits).
#'
#' @name nacho-accessors
#'
#' @examples
#' data(GSE74821)
#' nacho_counts(GSE74821)[1:5, 1:3]
#' head(nacho_samples(GSE74821)[, 1:5])
#' head(nacho_probes(GSE74821))
#' head(nacho_qc(GSE74821))
NULL

#' @rdname nacho-accessors
#' @export
nacho_counts <- function(x, normalised = FALSE, log2 = FALSE) {
  check_nacho(x)
  check_bool(normalised)
  check_bool(log2)
  values <- if (normalised) x@normalised else x@counts
  if (log2) {
    values <- base::log2(values + 1)
  }
  values
}

#' @rdname nacho-accessors
#' @export
nacho_samples <- function(x) {
  check_nacho(x)
  scores <- as.data.frame(x@pca[["scores"]])
  rownames(scores) <- NULL
  if (ncol(scores) == 0) {
    return(x@samples)
  }
  cbind(x@samples, scores)
}

#' @rdname nacho-accessors
#' @export
nacho_probes <- function(x) {
  check_nacho(x)
  x@probes
}

#' @rdname nacho-accessors
#' @export
nacho_qc <- function(x) {
  check_nacho(x)
  qc_table(x@samples, x@thresholds, x@rcc_type, x@settings[["id_colname"]])
}
