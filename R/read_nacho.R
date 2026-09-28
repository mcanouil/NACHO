#' Convert a NACHO 2 object to NACHO 3
#'
#' NACHO 3 stores data in an S7 `nacho` object instead of the NACHO 2 list.
#' `upgrade_nacho()` converts a NACHO 2 object once: it keeps the raw counts,
#' the sample sheet, the settings and the thresholds, and computes the
#' quality-control metrics, the normalised counts, the PCA and the outlier
#' flags again with the NACHO 3 definitions.
#'
#' @param x A NACHO 2 object, as returned by `load_rcc()` or `normalise()` in
#'   NACHO 2.
#'
#' @return A `nacho` object.
#' @export
#' @examples
#' data(GSE74821)
#' identical(upgrade_nacho(GSE74821), GSE74821)
upgrade_nacho <- function(x) {
  if (S7::S7_inherits(x, nacho)) {
    nacho_inform(
      "{.arg x} is already a NACHO 3 object, so it is returned unchanged."
    )
    return(x)
  }
  if (!is_nacho_v2(x)) {
    nacho_abort(
      "{.arg x} must be a NACHO 2 object, not {.obj_type_friendly {x}}.",
      class = "bad_object"
    )
  }
  long <- as.data.frame(x[["nacho"]])
  id <- x[["access"]]
  probe_columns <- c("CodeClass", "Name", "Accession")
  probes <- unique(long[, probe_columns])
  probes <- probes[
    order(probes[["CodeClass"]], probes[["Name"]], method = "radix"),
  ]
  rownames(probes) <- NULL
  if (anyDuplicated(probes[["Name"]]) > 0) {
    nacho_abort(
      "{.arg x} has probes whose name appears with more than one code class or accession.",
      class = "bad_object"
    )
  }
  dropped <- c(
    probe_columns,
    "Count",
    "Count_Norm",
    "file_path",
    grep("^PC[0-9]+$", names(long), value = TRUE),
    computed_sample_columns
  )
  sample_columns <- setdiff(names(long), c(dropped, id))
  samples <- long[!duplicated(long[[id]]), c(id, sample_columns), drop = FALSE]
  samples <- samples[
    order(as.character(samples[[id]]), method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(samples) <- NULL
  ids <- as.character(samples[[id]])
  counts <- matrix(
    NA_integer_,
    nrow = nrow(probes),
    ncol = length(ids),
    dimnames = list(probes[["Name"]], ids)
  )
  counts[cbind(
    match(long[["Name"]], probes[["Name"]]),
    match(long[[id]], ids)
  )] <-
    as.integer(long[["Count"]])

  thresholds <- x[["outliers_thresholds"]]
  check_thresholds(thresholds, arg = "x$outliers_thresholds")
  provenance <- new_provenance(
    data_directory = x[["data_directory"]],
    file_version = samples[["Header.header_FileVersion"]],
    software_version = samples[["Header.header_SoftwareVersion"]]
  )
  provenance[["upgraded_from"]] <- list(
    nacho_version = "2",
    remove_outliers = isTRUE(x[["remove_outliers"]])
  )
  upgraded <- build_nacho(
    counts = counts,
    probes = probes,
    samples = samples,
    settings = list(
      id_colname = id,
      housekeeping_genes = x[["housekeeping_genes"]],
      housekeeping_predict = FALSE,
      housekeeping_norm = isTRUE(x[["housekeeping_norm"]]),
      normalisation_method = x[["normalisation_method"]],
      n_comp = as.integer(x[["n_comp"]])
    ),
    thresholds = thresholds,
    rcc_type = attr(x, "RCC_type"),
    provenance = provenance
  )
  nacho_inform(c(
    "Converted a NACHO 2 object to NACHO 3.",
    i = "The PCA, normalised counts and outlier flags are recomputed with NACHO 3 and may differ from the saved ones.",
    i = if (isTRUE(x[["housekeeping_predict"]])) {
      "The housekeeping genes NACHO 2 predicted are kept, and are not predicted again."
    }
  ))
  upgraded
}
