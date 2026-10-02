#' @include qc.R
NULL

#' Detection limit of each sample
#'
#' The mean plus two standard deviations of the kept negative controls, or
#' `NA` when fewer than two are left.
#'
#' @noRd
detection_limits <- function(counts, code_class, excluded) {
  negatives <- counts[
    code_class == "Negative" & !rownames(counts) %in% excluded,
    ,
    drop = FALSE
  ]
  if (nrow(negatives) < 2) {
    return(rep(NA_real_, ncol(counts)))
  }
  unname(
    colMeans(negatives, na.rm = TRUE) +
      2 * apply(negatives, 2, stats::sd, na.rm = TRUE)
  )
}

#' Which counts sit above their sample's detection limit
#'
#' @noRd
detected <- function(counts, limits) {
  counts > matrix(limits, nrow(counts), ncol(counts), byrow = TRUE)
}

#' Share of samples in which each probe is detected
#'
#' @noRd
probe_detection_rates <- function(counts, code_class, excluded) {
  limits <- detection_limits(counts, code_class, excluded)
  missing_not_nan(rowMeans(detected(counts, limits), na.rm = TRUE))
}

#' Keep genes detected in enough samples
#'
#' A gene is detected in a sample when its raw count is above the mean plus
#' two standard deviations of that sample's negative controls.
#' Control and housekeeping probes are always kept, where housekeeping probes
#' are the probes of CodeClass Housekeeping.
#' Endogenous genes chosen as housekeeping genes are filtered like any other
#' gene, and the ones that are dropped leave the `housekeeping_genes` setting.
#' The samples' `Detection_rate` is not recomputed after filtering.
#' A gene's detection rate is the share of the samples with a detection limit
#' in which it is detected, so a sample with fewer than two negative probes
#' does not count.
#' Endogenous genes whose detection rate is missing, because every sample with
#' a detection limit has a missing count for them, are dropped, even with
#' `min_rate = 0`.
#' With all-zero negative controls the detection limit is 0, so every
#' non-zero count counts as detected.
#' An object without endogenous genes comes back unchanged.
#' When no sample has a detection limit, `filter_detected()` stops with an
#' error.
#'
#' @param x A `nacho` object from [load_rcc()] or [normalise()].
#' @param min_rate The smallest share of samples with a detection limit,
#'   between 0 and 1, in which an endogenous gene must be detected.
#'
#' @return A `nacho` object with fewer probes.
#' @export
#' @examples
#' data(GSE74821)
#' dim(filter_detected(GSE74821, min_rate = 0.5))
filter_detected <- function(x, min_rate = 0.5) {
  check_nacho(x)
  check_proportion(min_rate)
  rate <- x@probes[["detection_rate"]]
  endogenous <- grepl("Endogenous", x@probes[["CodeClass"]])
  if (!any(endogenous)) {
    return(x)
  }
  if (is.null(rate) || all(is.na(rate[endogenous]))) {
    nacho_abort(
      c(
        "No sample has a detection limit, so {.fn filter_detected} cannot tell which genes are detected.",
        i = "A detection limit needs two kept negative probes with counts."
      ),
      class = "no_detection_rate"
    )
  }
  keep <- !endogenous | (!is.na(rate) & rate >= min_rate)
  filtered <- x[which(keep), ]
  genes <- filtered@settings[["housekeeping_genes"]]
  if (!is.null(genes)) {
    genes <- genes[genes %in% filtered@probes[["Name"]]]
    filtered@settings["housekeeping_genes"] <- list(
      if (length(genes) > 0) genes
    )
  }
  filtered
}
