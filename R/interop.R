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
#' `as_nacho()` goes the other way, from a `SummarizedExperiment` or from a
#' `NanoStringRccSet` read by `NanoStringNCTools::readNanoStringRccSet()`.
#' The dates of a `NanoStringRccSet` are written as `YYYYMMDD`, as in RCC
#' files.
#' Its probe names lose a trailing numeric suffix such as `|0`, as in
#' [load_rcc()], so both give the same probe names for the same RCC files.
#' It computes the quality-control
#' metrics and the normalisation from the raw counts; settings and thresholds
#' come from `metadata()$nacho` when present, and are the defaults otherwise.
#' They are checked like the arguments of [normalise()].
#' The result always carries the current object schema.
#' A `nacho` object is checked and returned as is, and a NACHO 2 object gets a
#' hint to use [upgrade_nacho()].
#'
#' @param x A `nacho` object for `as_summarized_experiment()`; a
#'   `SummarizedExperiment` or a `NanoStringRccSet` for `as_nacho()`.
#' @param id_colname Name of the sample id column to create when `x` does not
#'   come from NACHO.
#'   When `metadata(x)$nacho$settings` exists, its `id_colname` wins and this
#'   argument is ignored.
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
  check_string(id_colname)
  if (inherits(x, "nacho") || S7::S7_inherits(x, nacho)) {
    check_nacho(x)
    return(x)
  }
  if (methods::is(x, "NanoStringRccSet")) {
    return(nacho_from_rccset(x, id_colname))
  }
  if (methods::is(x, "SummarizedExperiment")) {
    return(nacho_from_se(x, id_colname))
  }
  nacho_abort(
    "{.arg x} must be a {.cls SummarizedExperiment} or a {.cls NanoStringRccSet}, not {.obj_type_friendly {x}}.",
    class = "bad_object"
  )
}

check_raw_counts <- function(counts, call = rlang::caller_env()) {
  values <- if (is.numeric(counts)) counts[!is.na(counts)]
  if (
    !is.numeric(counts) ||
      any(values != round(values)) ||
      any(values < 0) ||
      any(values > .Machine[["integer.max"]])
  ) {
    nacho_abort(
      c(
        "The counts must be raw, non-negative whole numbers, or missing.",
        i = "They must also be at most {.val {(.Machine[['integer.max']])}}."
      ),
      class = "bad_object",
      call = call
    )
  }
  invisible(counts)
}

check_unique_names <- function(names, what, call = rlang::caller_env()) {
  if (is.null(names) || anyNA(names) || any(!nzchar(names))) {
    nacho_abort(
      "Every {what} must have a name.",
      class = "bad_object",
      call = call
    )
  }
  duplicated_names <- duplicated_values(names)
  if (length(duplicated_names) > 0) {
    nacho_abort(
      "The {what} names must be unique; {.val {utils::head(duplicated_names, 3)}} {?is/are} duplicated.",
      class = "bad_object",
      call = call
    )
  }
  invisible(names)
}

check_control_probes <- function(code_class, call = rlang::caller_env()) {
  missing_classes <- setdiff(c("Positive", "Negative"), code_class)
  if (length(missing_classes) > 0) {
    nacho_abort(
      c(
        "The probe data has no {.or {.val {missing_classes}}} control probes.",
        i = "Keep the control probes when you subset the rows."
      ),
      class = "bad_object",
      call = call
    )
  }
  invisible(code_class)
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
  check_control_probes(probes[["CodeClass"]], call = call)
  check_raw_counts(counts, call = call)
  if (!"Name" %in% names(probes)) {
    probes[["Name"]] <- rownames(counts)
  }
  check_unique_names(probes[["Name"]], "probe", call = call)
  check_unique_names(colnames(counts), "sample", call = call)
  storage.mode(counts) <- "integer"
  if (!"Accession" %in% names(probes)) {
    probes[["Accession"]] <- NA_character_
  }
  rownames(probes) <- NULL
  rownames(counts) <- probes[["Name"]]
  check_nacho_metadata(metadata, call = call)
  settings <- nacho_metadata_settings(
    metadata[["settings"]],
    probes,
    id_colname,
    call = call
  )
  id_colname <- settings[["id_colname"]]
  if (
    id_colname %in%
      names(samples) &&
      !identical(as.character(samples[[id_colname]]), colnames(counts))
  ) {
    hint <- if (is.null(metadata[["settings"]][["id_colname"]])) {
      "Pass another {.arg id_colname}, or rename that column."
    } else {
      "Rename that column, or remove {.field id_colname} from {.arg metadata(x)$nacho$settings}."
    }
    nacho_abort(
      c(
        "The sample data already has a column {.field {id_colname}} that differs from the sample names.",
        i = hint
      ),
      class = "bad_argument",
      call = call
    )
  }
  samples[[id_colname]] <- colnames(counts)
  samples <- column_first(samples, id_colname)
  rownames(samples) <- NULL
  provenance <- metadata[["provenance"]] %||%
    new_provenance(
      data_directory = NULL,
      file_version = samples[["Header.header_FileVersion"]],
      software_version = samples[["Header.header_SoftwareVersion"]]
    )
  provenance[["schema_version"]] <- nacho_schema_version
  build_nacho(
    counts = counts,
    probes = probes,
    samples = samples,
    settings = settings,
    thresholds = metadata[["thresholds"]] %||% default_thresholds(),
    rcc_type = metadata[["rcc_type"]] %||% "n1",
    provenance = provenance
  )
}

check_nacho_metadata <- function(metadata, call = rlang::caller_env()) {
  if (!is.null(metadata) && !(is.list(metadata) && rlang::is_named(metadata))) {
    nacho_abort(
      "{.arg metadata(x)$nacho} must be a named list, not {.obj_type_friendly {metadata}}.",
      class = "bad_argument",
      call = call
    )
  }
  for (field in c("settings", "provenance")) {
    value <- metadata[[field]]
    if (!is.null(value) && !(is.list(value) && rlang::is_named(value))) {
      nacho_abort(
        "{.arg metadata(x)$nacho${field}} must be a named list, not {.obj_type_friendly {value}}.",
        class = "bad_argument",
        call = call
      )
    }
  }
  if (!is.null(metadata[["thresholds"]])) {
    check_thresholds(
      metadata[["thresholds"]],
      arg = "metadata(x)$nacho$thresholds",
      call = call
    )
  }
  if (!is.null(metadata[["rcc_type"]])) {
    check_choice(
      metadata[["rcc_type"]],
      c("n1", "n8"),
      arg = "metadata(x)$nacho$rcc_type",
      call = call
    )
  }
  invisible(metadata)
}

#' Complete and check the settings saved in the metadata
#'
#' Saved settings override `default_settings()`, and saved housekeeping genes
#' that are no longer probes are dropped.
#'
#' @param settings `metadata(x)$nacho$settings`, a named list or `NULL`.
#' @param probes The probe table.
#' @param id_colname The sample id column to use when `settings` has none.
#' @inheritParams nacho_abort
#'
#' @return The complete settings list.
#'
#' @noRd
nacho_metadata_settings <- function(
  settings,
  probes,
  id_colname,
  call = rlang::caller_env()
) {
  merged <- default_settings(probes, id_colname)
  unknown <- setdiff(names(settings), names(merged))
  if (length(unknown) > 0) {
    nacho_abort(
      c(
        "{.arg metadata(x)$nacho$settings} has unknown setting{?s}: {.field {unknown}}.",
        i = "Known settings: {.field {names(merged)}}."
      ),
      class = "bad_argument",
      call = call
    )
  }
  merged[names(settings)] <- settings
  check_string(
    merged[["id_colname"]],
    arg = "metadata(x)$nacho$settings$id_colname",
    call = call
  )
  merged[["normalisation_method"]] <- check_settings(
    merged[["housekeeping_genes"]],
    merged[["housekeeping_predict"]],
    merged[["housekeeping_norm"]],
    merged[["normalisation_method"]],
    merged[["n_comp"]],
    arg_prefix = "metadata(x)$nacho$settings$",
    call = call
  )
  merged[["n_comp"]] <- as.integer(merged[["n_comp"]])
  housekeeping_genes <- merged[["housekeeping_genes"]]
  housekeeping_genes <- housekeeping_genes[
    housekeeping_genes %in% probes[["Name"]]
  ]
  if (length(housekeeping_genes) == 0) {
    merged["housekeeping_genes"] <- list(NULL)
  } else {
    merged[["housekeeping_genes"]] <- housekeeping_genes
  }
  merged[["housekeeping_norm"]] <- resolve_housekeeping_norm(
    probes[["CodeClass"]],
    merged[["housekeeping_genes"]],
    merged[["housekeeping_predict"]],
    merged[["housekeeping_norm"]]
  )
  merged
}

nacho_from_se <- function(x, id_colname, call = rlang::caller_env()) {
  check_bioconductor(
    c("SummarizedExperiment", "S4Vectors"),
    "to read a SummarizedExperiment",
    call = call
  )
  if (!"counts" %in% SummarizedExperiment::assayNames(x)) {
    nacho_abort(
      "{.arg x} must have a {.val counts} assay.",
      class = "bad_object",
      call = call
    )
  }
  nacho_from_parts(
    counts = as.matrix(SummarizedExperiment::assay(x, "counts")),
    probes = as.data.frame(SummarizedExperiment::rowData(x), optional = TRUE),
    samples = as.data.frame(SummarizedExperiment::colData(x), optional = TRUE),
    metadata = S4Vectors::metadata(x)[["nacho"]],
    id_colname = id_colname,
    call = call
  )
}

nacho_from_rccset <- function(x, id_colname, call = rlang::caller_env()) {
  check_bioconductor(
    c("Biobase", "NanoStringNCTools"),
    "to read a NanoStringRccSet",
    call = call
  )
  features <- Biobase::fData(x)
  probes <- data.frame(
    CodeClass = features[["CodeClass"]],
    Name = strip_probe_suffix(features[["GeneName"]]),
    Accession = features[["Accession"]]
  )
  protocol <- Biobase::pData(Biobase::protocolData(x))
  rcc_names <- c(
    FileVersion = "Header.header_FileVersion",
    SoftwareVersion = "Header.header_SoftwareVersion",
    SampleID = "Sample_Attributes.sample_ID",
    SampleOwner = "Sample_Attributes.sample_Owner",
    SampleComments = "Sample_Attributes.sample_Comments",
    SampleDate = "Sample_Attributes.sample_Date",
    SystemAPF = "Sample_Attributes.sample_SystemAPF",
    LaneID = "Lane_Attributes.lane_ID",
    FovCount = "Lane_Attributes.lane_FovCount",
    FovCounted = "Lane_Attributes.lane_FovCounted",
    ScannerID = "Lane_Attributes.lane_ScannerID",
    StagePosition = "Lane_Attributes.lane_StagePosition",
    BindingDensity = "Lane_Attributes.lane_BindingDensity",
    CartridgeID = "Lane_Attributes.lane_CartridgeID",
    CartridgeBarcode = "Lane_Attributes.lane_CartridgeBarcode"
  )
  renamed <- names(protocol) %in% names(rcc_names)
  names(protocol)[renamed] <- rcc_names[names(protocol)[renamed]]
  protocol[] <- lapply(protocol, function(column) {
    if (inherits(column, "Date")) {
      format(column, "%Y%m%d")
    } else {
      as.character(column)
    }
  })
  nacho_from_parts(
    counts = Biobase::exprs(x),
    probes = probes,
    samples = cbind(Biobase::pData(x), protocol),
    metadata = NULL,
    id_colname = id_colname,
    call = call
  )
}
