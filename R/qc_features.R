#' qc_features
#'
#' @param data [[data.frame]] A `data.frame` with the count data.
#' @inheritParams load_rcc
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[data.frame]]
qc_features <- function(data, id_colname) {
  output <- lapply(X = split(data, data[[id_colname]]), FUN = function(.data) {
    positives <- .data[.data[["CodeClass"]] %in% "Positive", c("Name", "Count")]
    counts <- .data[grep("Endogenous", .data[["CodeClass"]]), ][["Count"]]

    if (any(grepl("POS_E", positives[["Name"]]))) {
      pcl <- qc_positive_control(counts = positives)
      negatives <- .data[.data[["CodeClass"]] %in% "Negative", ][["Count"]]
      lod <- qc_limit_detection(
        pos_e = positives[[grep("POS_E", positives[["Name"]]), "Count"]],
        negatives = negatives
      )
    } else {
      pcl <- 0
      lod <- 0
    }
    fov <- qc_imaging(
      fov_counted = as.numeric(unique(.data[["Lane_Attributes.lane_FovCounted"]])),
      fov_count = as.numeric(unique(.data[["Lane_Attributes.lane_FovCount"]]))
    )

    mean_count <- round(mean(counts), 2)
    median_count <- stats::median(counts)

    c(
      "Date" = unique(.data[["Sample_Attributes.sample_Date"]]),
      "ID" = unique(.data[["Lane_Attributes.lane_ID"]]),
      "BD" = unique(.data[["Lane_Attributes.lane_BindingDensity"]]),
      "ScannerID" = unique(.data[["Lane_Attributes.lane_ScannerID"]]),
      "StagePosition" = unique(.data[["Lane_Attributes.lane_StagePosition"]]),
      "CartridgeID" = unique(.data[["Lane_Attributes.lane_CartridgeID"]]),
      "FoV" = fov,
      "PCL" = ifelse(is.na(pcl), 0, pcl),
      "LoD" = lod,
      "MC" = mean_count,
      "MedC" = median_count
    )
  })
  output <- data.table::as.data.table(
    x = do.call("rbind", output),
    keep.rownames = id_colname
  )
  metrics_in <- intersect(names(output), c("BD", "FoV", "PCL", "LoD", "MC", "MedC"))
  output[
    j = c(metrics_in) := lapply(.SD, as.numeric),
    .SDcols = c(metrics_in)
  ]
}
