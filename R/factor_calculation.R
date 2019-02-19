#' factor_calculation
#'
#' @param nacho_df [data.frame]
#' @param id_colname [character]
#' @param housekeeping_genes [vector(character)]
#' @param housekeeping_predict [logical]
#' @param normalisation_method [character]
#' @param exclude_probes [vector(character)]
#'
#' @return [data.frame]
factor_calculation <- function(
  nacho_df,
  id_colname,
  housekeeping_genes,
  housekeeping_predict,
  normalisation_method,
  exclude_probes
) {

  exclude_probes <- c("POS_F(0.125)", exclude_probes)

  control_data <- nacho_df[, c("Name", "CodeClass", "Count", id_colname)]
  control_data <- control_data[control_data[["CodeClass"]] %in% c("Positive", "Negative"), ]
  control_data <- control_data[order(control_data[["Name"]]), ]
  control_data <- control_data[!control_data[, "Name"] %in% exclude_probes, ]
  nested_control_data <- tidyr::nest(dplyr::group_by(.data = control_data, get(id_colname)))
  colnames(nested_control_data)[1] <- id_colname

  factors_norm <- switch(
    EXPR = normalisation_method,
    "GLM" = {
      norm_glm(data = nested_control_data[["data"]])
    },
    "GEO" = {
      norm_geo(data = nested_control_data[["data"]])
    },
    stop('normalisation_method should be either "GLM" or "GEO"!')
  )
  positive_factor <- factors_norm[["positive_factor"]]
  geometric_mean_neg <- factors_norm[["geometric_mean_neg"]]

  nested_nacho_df <- tidyr::nest(dplyr::group_by(.data = nacho_df, get(id_colname)))
  colnames(nested_nacho_df)[1] <- id_colname

  if (housekeeping_predict | is.null(housekeeping_genes)) {
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
    house_factor <- mean(geometric_mean_house) / geometric_mean_house
    house_factor <- unname(house_factor)

    norm_factor <- data.frame(
      "Positive_factor" = positive_factor,
      "Negative_factor" = geometric_mean_neg,
      "House_factor" = house_factor
    )
  }
  rownames(norm_factor) <- nested_nacho_df[[id_colname]]
  norm_factor[[id_colname]] <- nested_nacho_df[[id_colname]]
  norm_factor
}
