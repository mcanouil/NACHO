#' norm_geo
#'
#' @param data [[data.frame]] A `list` of `data.frame` with the count data.
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[list]]
norm_geo <- function(data) {
  geometric_mean_pos <- geometric_probes(data = data, probes_type = "Positive")
  geometric_mean_neg <- geometric_probes(data = data, probes_type = "Negative")
  list(
    geometric_mean_neg = geometric_mean_neg,
    positive_factor = mean(geometric_mean_pos) / geometric_mean_pos
  )
}
