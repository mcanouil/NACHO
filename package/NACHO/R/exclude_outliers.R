#' exclude_outliers
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
exclude_outliers <- function(object) {
  nacho_df <- object[["nacho"]]
  id_colname <- object[["access"]]
  all_out <- unique(unlist(details_outlier(nacho_df = nacho_df, id_colname = id_colname)))

  if (length(all_out)!=0) {
    return(nacho_df[!nacho_df[[id_colname]] %in% all_out, ])
  } else {
    return(nacho_df)
  }
}
