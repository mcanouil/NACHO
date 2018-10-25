#' factor_calculation
#'
#' @param nacho_df [data.frame]
#' @param id_colname [character]
#' @param housekeeping_genes [vector(character)]
#' @param predict_housekeeping [logical]
#' @param normalisation_method [character]
#' @param exclude_probes [vector(character)]
#'
#' @return [data.frame]
#'
#' @importFrom dplyr group_by
#' @importFrom tidyr nest
factor_calculation <- function(
  nacho_df,
  id_colname,
  housekeeping_genes,
  predict_housekeeping = is.null(housekeeping_genes),
  normalisation_method,
  exclude_probes
) {
  nested_nacho_df <- tidyr::nest(dplyr::group_by(.data = nacho_df, get(id_colname)))
  colnames(nested_nacho_df)[1] <- id_colname
  probes_out <- c("POS_F(0.125)", exclude_probes)

  positive_factor <- switch(
    EXPR = normalisation_method,
    "GLM" = {
      norm_glm(data = nested_nacho_df[["data"]], exclude_probes = probes_out)
    },
    "GEO" = {
      norm_geo(data = nested_nacho_df[["data"]], exclude_probes = probes_out)
    },
    stop('normalisation_method should be either "GLM" or "GEO"!')
  )
  geometric_mean_neg <- geometric_probes(
    data = nested_nacho_df[["data"]],
    probes_type = "Negative",
    exclude_probes = probes_out
  )

  if (predict_housekeeping | is.null(housekeeping_genes)) {
    norm_factor <- data.frame(
      "Positive_factor" = positive_factor,
      "Negative_factor" = geometric_mean_neg
    )
  } else {
    geometric_mean_house <- mapply(
      FUN = geometric_housekeeping,
      data = nested_nacho_df[["data"]],
      positive_factor = positive_factor,
      intercept = geometric_mean_neg,
      housekeeping_genes = list(housekeeping_genes)
    )
    house_factor <- sapply(X = geometric_mean_house, FUN = function(x) {mean(geometric_mean_house) / x})
    house_factor <- unname(house_factor)

    norm_factor <- data.frame(
      "Positive_factor" = positive_factor,
      "Negative_factor" = geometric_mean_neg,
      "House_factor" = house_factor
    )
  }
  rownames(norm_factor) <- nested_nacho_df[[id_colname]]
  norm_factor[[id_colname]] <- nested_nacho_df[[id_colname]]
  return(norm_factor)
}
