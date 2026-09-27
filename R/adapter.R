#' Build a nacho object from the long NACHO 2 table
#'
#' Temporary bridge while the QC pipeline still works on the long table.
#'
#' @keywords internal
#' @noRd
nacho_from_long <- function(
  long,
  pca,
  settings,
  thresholds,
  rcc_type,
  provenance
) {
  long <- data.table::as.data.table(long)
  id <- settings[["id_colname"]]
  probe_columns <- c("CodeClass", "Name", "Accession")
  probes <- unique(long[, probe_columns, with = FALSE])
  probes <- probes[order(probes[["CodeClass"]], probes[["Name"]])]
  duplicated_names <- unique(probes[["Name"]][duplicated(probes[["Name"]])])
  if (length(duplicated_names) > 0) {
    nacho_abort(
      c(
        "Probe names must be unique within the RCC files.",
        x = "Duplicated: {.val {utils::head(duplicated_names, 5)}}."
      ),
      class = "rcc_parse"
    )
  }
  sample_columns <- setdiff(
    names(long),
    c(probe_columns, "Count", "Count_Norm", "file_path")
  )
  samples <- as.data.frame(
    long[
      !duplicated(long[[id]]),
      c(id, setdiff(sample_columns, id)),
      with = FALSE
    ]
  )
  ids <- as.character(samples[[id]])
  cell <- cbind(match(long[["Name"]], probes[["Name"]]), match(long[[id]], ids))
  counts <- matrix(
    NA_integer_,
    nrow(probes),
    length(ids),
    dimnames = list(probes[["Name"]], ids)
  )
  counts[cell] <- as.integer(long[["Count"]])
  normalised <- matrix(
    NA_real_,
    nrow(probes),
    length(ids),
    dimnames = list(probes[["Name"]], ids)
  )
  normalised[cell] <- as.numeric(long[["Count_Norm"]])
  scores <- pca[["scores"]][ids, , drop = FALSE]
  samples[["is_outlier"]] <- compute_outliers(samples, thresholds, rcc_type)
  probes <- as.data.frame(probes)
  probes[["is_housekeeping"]] <- probes[["Name"]] %in%
    settings[["housekeeping_genes"]]
  probes[["is_excluded"]] <- FALSE
  nacho(
    counts = counts,
    normalised = normalised,
    probes = probes,
    samples = samples,
    settings = settings,
    thresholds = thresholds,
    pca = list(scores = scores, importance = pca[["importance"]]),
    rcc_type = rcc_type,
    provenance = provenance
  )
}
