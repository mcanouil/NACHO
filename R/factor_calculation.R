#' factor_calculation
#'
#' @inheritParams qc_rcc
#' @inheritParams load_rcc
#' @inheritParams normalise
#' @param exclude_probes [[character]] A vector of probes to exclude.
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[data.frame]]
factor_calculation <- function(
  nacho_df,
  id_colname,
  housekeeping_genes,
  housekeeping_predict,
  normalisation_method,
  exclude_probes
) {
  CodeClass <- Name <- NULL # no visible binding for global variable
  exclude_probes <- c("POS_F(0.125)", exclude_probes)
  control_data <- nacho_df[
    j = .SD,
    .SDcols = c("Name", "CodeClass", "Count", id_colname)
  ][
    CodeClass %in% c("Positive", "Negative") &
      ! Name %in% exclude_probes
  ][
    order(Name)
  ]

  factors_norm_fun <- switch(
    EXPR = normalisation_method,
    "GLM" = norm_glm,
    "GEO" = norm_geo,
    stop('[NACHO] "normalisation_method" should be either "GLM" or "GEO"!')
  )
  factors_norm <- factors_norm_fun(data = split(control_data, control_data[[id_colname]]))
  positive_factor <- factors_norm[["positive_factor"]]
  geometric_mean_neg <- factors_norm[["geometric_mean_neg"]]

  if (housekeeping_predict | is.null(housekeeping_genes)) {
    norm_factor <- data.table::data.table(
      "Positive_factor" = positive_factor,
      "Negative_factor" = geometric_mean_neg
    )
  } else {
    geometric_mean_house <- mapply(
      FUN = geometric_housekeeping,
      data = split(nacho_df, nacho_df[[id_colname]]),
      positive_factor = positive_factor,
      intercept = geometric_mean_neg,
      housekeeping_genes = list(housekeeping_genes)
    )
    norm_factor <- data.table::data.table(
      "Positive_factor" = positive_factor,
      "Negative_factor" = geometric_mean_neg,
      "House_factor" = mean(geometric_mean_house) / geometric_mean_house
    )
  }
  norm_factor[[id_colname]] <- names(positive_factor)
  norm_factor
}
