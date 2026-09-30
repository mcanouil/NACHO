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

#' Keep genes detected in enough samples
#'
#' A gene is detected in a sample when its raw count is above the mean plus
#' two standard deviations of that sample's negative controls.
#' Control and housekeeping probes are always kept, where housekeeping probes
#' are the probes of CodeClass Housekeeping.
#' Endogenous genes chosen as housekeeping genes are filtered like any other
#' gene.
#' The samples' `Detection_rate` is not recomputed after filtering.
#' Endogenous genes whose detection rate is missing, because a sample has
#' fewer than two negative probes, are dropped.
#'
#' @param x A `nacho` object from [load_rcc()] or [normalise()].
#' @param min_rate The smallest share of samples, between 0 and 1, in which an
#'   endogenous gene must be detected.
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
  keep <- !endogenous | (!is.na(rate) & rate >= min_rate)
  x[which(keep), ]
}
