#' qc_features
#'
#' @param data [data.frame]
#' @param id_colname [character]
#'
#' @return [data.frame]
#'
#' @importFrom dplyr group_by
#' @importFrom stats median
#' @importFrom tidyr nest
qc_features <- function(data, id_colname) {
  nested_data_df <- tidyr::nest(dplyr::group_by(.data = data, get(id_colname)))
  colnames(nested_data_df)[1] <- id_colname

  output <- lapply(X = nested_data_df[["data"]], FUN = function(.data) {
    positives <- .data[.data[["CodeClass"]] %in% "Positive", c("Name", "Count")]
    counts <- .data[grep("Endogenous", .data[["CodeClass"]]), ][["Count"]]

    if (any(grepl("POS_E", positives[["Name"]]))) {
      pc <- qc_positive_control(counts = positives)
      negatives <- .data[.data[["CodeClass"]] %in% "Negative", ][["Count"]]
      ld <- qc_limit_detection(
        pos_e = positives[[grep("POS_E", positives[["Name"]]), "Count"]],
        negatives = negatives
      )
    } else {
      pc <- 0
      ld <- 0
    }
    fov <- qc_imaging(
      fov_counted = as.numeric(unique(.data[["lane_FovCounted"]])),
      fov_count = as.numeric(unique(.data[["lane_FovCount"]]))
    )

    mean_count <- round(mean(counts), 2)
    median_count <- stats::median(counts)

    output <- c(
      "Date" = unique(.data[["sample_Date"]]),
      "ID" = unique(.data[["lane_ID"]]),
      "BD" = unique(.data[["lane_BindingDensity"]]),
      "ScannerID" = unique(.data[["lane_ScannerID"]]),
      "StagePosition" = unique(.data[["lane_StagePosition"]]),
      "CartridgeID" = unique(.data[["lane_CartridgeID"]]),
      "FoV" = fov,
      "PC" = ifelse(is.na(pc), 0, pc),
      "LoD" = ld,
      "MC" = mean_count,
      "MedC" = median_count
    )
    return(output)
  })
  output <- as.data.frame(do.call("rbind", output), stringsAsFactors = FALSE)
  output[[id_colname]] <- nested_data_df[[id_colname]]

  output[c("BD", "FoV", "PC", "LoD", "MC", "MedC")] <- lapply(
    X = output[c("BD", "FoV", "PC", "LoD", "MC", "MedC")],
    FUN = as.numeric
  )
  return(output)
}