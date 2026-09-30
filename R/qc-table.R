#' @include qc.R
NULL

#' Metrics assessed against thresholds, in display order
#'
#' @noRd
qc_metrics <- c("BD", "FoV", "PCL", "LoD", "Positive_factor", "House_factor")

#' Metrics shared by the eight samples of a PlexSet lane
#'
#' @noRd
lane_metrics <- c("BD", "FoV")

#' Metrics not assessed for PlexSet files
#'
#' @noRd
plexset_unassessed <- c("PCL", "LoD")

#' Describe why a value fails its limits
#'
#' @noRd
failure_reason <- function(metric, value, limits) {
  shown <- function(v) format(signif(v, 3))
  if (length(limits) == 2 && value > limits[2]) {
    paste(metric, shown(value), "above", shown(limits[2]))
  } else {
    paste(metric, shown(value), "below", shown(limits[1]))
  }
}

#' Build the quality-control table
#'
#' @noRd
qc_table <- function(samples, thresholds, rcc_type, id_colname) {
  metrics <- qc_metrics[
    qc_metrics %in% names(samples) & qc_metrics %in% names(thresholds)
  ]
  column <- function(name) {
    if (name %in% names(samples)) samples[[name]] else rep(NA, nrow(samples))
  }
  out <- data.frame(
    column(id_colname),
    lane = column("ID"),
    CartridgeID = column("CartridgeID")
  )
  names(out)[1] <- id_colname
  fails <- matrix(
    FALSE,
    nrow(samples),
    length(metrics),
    dimnames = list(NULL, metrics)
  )
  for (metric in metrics) {
    values <- samples[[metric]]
    status <- ifelse(
      metric_fails(values, thresholds[[metric]]),
      "fail",
      ifelse(is.na(values), NA_character_, "pass")
    )
    if (rcc_type == "n8" && metric %in% plexset_unassessed) {
      status <- rep(NA_character_, nrow(samples))
    }
    out[[metric]] <- values
    out[[paste0(metric, "_status")]] <- status
    fails[, metric] <- status %in% "fail"
  }
  for (name in c("MC", "MedC", "Negative_factor", "Background")) {
    out[[name]] <- column(name)
  }
  statuses <- out[paste0(metrics, "_status")]
  assessed <- rowSums(!is.na(statuses)) > 0
  failed <- rowSums(fails) > 0
  inherited <- matrix(
    FALSE,
    nrow(samples),
    length(metrics),
    dimnames = list(NULL, metrics)
  )
  if (rcc_type == "n8") {
    lane_key <- paste(out[["CartridgeID"]], out[["lane"]], sep = "\r")
    shared <- intersect(lane_metrics, metrics)
    for (metric in shared) {
      inherited[, metric] <- stats::ave(fails[, metric], lane_key, FUN = any) &
        !fails[, metric]
    }
    lane_failed <- stats::ave(
      rowSums(fails[, shared, drop = FALSE]) > 0,
      lane_key,
      FUN = any
    )
    lane_assessed <- stats::ave(assessed, lane_key, FUN = any)
    out[["lane_status"]] <- ifelse(
      lane_failed,
      "fail",
      ifelse(lane_assessed, "pass", NA_character_)
    )
    failed <- failed | lane_failed
  }
  out[["n_flags"]] <- as.integer(rowSums(fails))
  out[["status"]] <- ifelse(
    failed,
    "fail",
    ifelse(assessed, "pass", NA_character_)
  )
  out[["reason"]] <- vapply(
    seq_len(nrow(samples)),
    function(k) {
      own <- vapply(
        metrics[fails[k, ]],
        function(m) failure_reason(m, samples[[m]][k], thresholds[[m]]),
        character(1)
      )
      lane <- if (any(inherited[k, ])) {
        paste("lane fails", paste(metrics[inherited[k, ]], collapse = ", "))
      }
      reasons <- c(unname(own), lane)
      if (length(reasons) == 0) {
        return(NA_character_)
      }
      paste(reasons, collapse = "; ")
    },
    character(1)
  )
  out
}

#' Samples whose quality-control status is fail
#'
#' @noRd
flagged_samples <- function(x) {
  qc <- qc_table(
    x@samples,
    x@thresholds,
    x@rcc_type,
    x@settings[["id_colname"]]
  )
  qc[["status"]] %in% "fail"
}
