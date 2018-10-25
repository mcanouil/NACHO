#' norm_geo
#'
#' @param data [list(data.frame)]
#' @param exclude_probes [vector(character)]
#'
#' @return
#' @export
#'
#' @examples
norm_geo <- function(data, exclude_probes = NULL) {
  geometric_mean_pos <- geometric_probes(data = data, probes_type = "Positive", exclude_probes = exclude_probes)
  positive_factor <- sapply(
    X = geometric_mean_pos,
    FUN = function(x) {mean(geometric_mean_pos) / x}
  )
  return(positive_factor)
}
