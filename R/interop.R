#' @include nacho-class.R
NULL

check_bioconductor <- function(packages, reason, call = rlang::caller_env()) {
  for (package in packages) {
    check_package(
      package,
      reason = reason,
      install = sprintf('BiocManager::install("%s")', package),
      call = call
    )
  }
}

#' Convert between nacho objects and Bioconductor containers
#'
#' `as_summarized_experiment()` gives a `SummarizedExperiment` with the raw and
#' normalised counts as assays `counts` and `normalised`, [nacho_samples()] as
#' `colData`, [nacho_probes()] as `rowData`, and the settings, thresholds, RCC
#' type and provenance in `metadata()$nacho`.
#'
#' `as_nacho()` goes the other way, from a `SummarizedExperiment`.
#' It computes the quality-control
#' metrics and the normalisation from the raw counts; settings and thresholds
#' come from `metadata()$nacho` when present, and are the defaults otherwise.
#'
#' @param x A `nacho` object for `as_summarized_experiment()`; a
#'   `SummarizedExperiment` for `as_nacho()`.
#' @param id_colname Name of the sample id column to create when `x` does not
#'   come from NACHO.
#'
#' @return A `SummarizedExperiment`, or a `nacho` object.
#' @export
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'   se <- as_summarized_experiment(GSE74821)
#'   as_nacho(se)
#' }
as_summarized_experiment <- function(x) {
  check_nacho(x)
  check_bioconductor(
    c("SummarizedExperiment", "S4Vectors"),
    "to build a SummarizedExperiment"
  )
  samples <- nacho_samples(x)
  rownames(samples) <- samples[[1]]
  probes <- x@probes
  rownames(probes) <- probes[["Name"]]
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = x@counts, normalised = x@normalised),
    colData = S4Vectors::DataFrame(samples, check.names = FALSE),
    rowData = S4Vectors::DataFrame(probes, check.names = FALSE),
    metadata = list(
      nacho = list(
        settings = x@settings,
        thresholds = x@thresholds,
        rcc_type = x@rcc_type,
        provenance = x@provenance
      )
    )
  )
}

#' @rdname as_summarized_experiment
#' @export
as_nacho <- function(x, id_colname = "IDFILE") {
  if (S7::S7_inherits(x, nacho)) {
    return(x)
  }
  if (methods::is(x, "SummarizedExperiment")) {
    return(nacho_from_se(x, id_colname))
  }
  nacho_abort(
    "{.arg x} must be a {.cls SummarizedExperiment}, not {.obj_type_friendly {x}}.",
    class = "bad_object"
  )
}

nacho_from_parts <- function(
  counts,
  probes,
  samples,
  metadata,
  id_colname,
  call = rlang::caller_env()
) {
  if (!"CodeClass" %in% names(probes)) {
    nacho_abort(
      "The probe data must have a {.field CodeClass} column.",
      class = "bad_object",
      call = call
    )
  }
  if (anyNA(counts) || any(counts != round(counts)) || any(counts < 0)) {
    nacho_abort(
      "The counts must be raw, non-negative whole numbers.",
      class = "bad_object",
      call = call
    )
  }
  storage.mode(counts) <- "integer"
  if (!"Name" %in% names(probes)) {
    probes[["Name"]] <- rownames(counts)
  }
  if (!"Accession" %in% names(probes)) {
    probes[["Accession"]] <- NA_character_
  }
  rownames(counts) <- probes[["Name"]]
  if (!is.null(metadata)) {
    id_colname <- metadata[["settings"]][["id_colname"]]
  }
  samples[[id_colname]] <- colnames(counts)
  samples <- samples[,
    c(id_colname, setdiff(names(samples), id_colname)),
    drop = FALSE
  ]
  rownames(samples) <- NULL
  settings <- metadata[["settings"]] %||%
    list(
      id_colname = id_colname,
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = any(grepl("Housekeeping", probes[["CodeClass"]])),
      normalisation_method = "GEO",
      n_comp = 10L
    )
  build_nacho(
    counts = counts,
    probes = probes,
    samples = samples,
    settings = settings,
    thresholds = metadata[["thresholds"]] %||% default_thresholds(),
    rcc_type = metadata[["rcc_type"]] %||% "n1",
    provenance = metadata[["provenance"]] %||%
      new_provenance(
        data_directory = NULL,
        file_version = samples[["Header.header_FileVersion"]],
        software_version = samples[["Header.header_SoftwareVersion"]]
      )
  )
}

nacho_from_se <- function(x, id_colname) {
  check_bioconductor(
    c("SummarizedExperiment", "S4Vectors"),
    "to read a SummarizedExperiment"
  )
  if (!"counts" %in% SummarizedExperiment::assayNames(x)) {
    nacho_abort(
      "{.arg x} must have a {.val counts} assay.",
      class = "bad_object"
    )
  }
  nacho_from_parts(
    counts = as.matrix(SummarizedExperiment::assay(x, "counts")),
    probes = as.data.frame(SummarizedExperiment::rowData(x), optional = TRUE),
    samples = as.data.frame(SummarizedExperiment::colData(x), optional = TRUE),
    metadata = S4Vectors::metadata(x)[["nacho"]],
    id_colname = id_colname
  )
}
