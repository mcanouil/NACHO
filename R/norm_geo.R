#' norm_geo
#'
#' @param data [list(data.frame)]
#' @param exclude_probes [vector(character)]
#'
#' @return [vector(numeric)]
norm_geo <- function(data, exclude_probes = NULL) {
  geometric_mean_pos <- geometric_probes(data = data, probes_type = "Positive")
  geometric_mean_neg <- geometric_probes(data = data, probes_type = "Negative")
  list(
    geometric_mean_neg = geometric_mean_neg,
    positive_factor = mean(geometric_mean_pos) / geometric_mean_pos
  )
}
