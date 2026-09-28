#' Produce a "nacho" object from RCC NanoString files
#'
#' This function is used to preprocess the data from NanoString nCounter.
#'
#' @param data_directory [[character]] A character string of the directory where the data are stored.
#' @param ssheet_csv [[character]] or [[data.frame]] Either a string with the name of the CSV
#'   of the samplesheet or the samplesheet as a `data.frame`.
#'   Should contain a column that matches the file names in the folder.
#' @param id_colname [[character]] Character string of the column in `ssheet_csv` that matches
#'   the file names in `data_directory`.
#' @param housekeeping_genes [[character]] A vector of names of the miRNAs/mRNAs
#'   that should be used as housekeeping genes. Default is `NULL`.
#' @param housekeeping_predict [[logical]] Boolean to indicate whether the housekeeping genes
#'   should be predicted (`TRUE`) or not (`FALSE`). Default is `FALSE`.
#' @param housekeeping_norm [[logical]] Boolean to indicate whether the housekeeping normalisation
#'   should be performed. Default is `TRUE`.
#' @param normalisation_method [[character]] Either `"GEO"` or `"GLM"`.
#'   Character string to indicate normalisation using the geometric mean (`"GEO"`)
#'   or a generalized linear model (`"GLM"`). Default is `"GEO"`.
#' @param n_comp [[numeric]] Number indicating the number of principal components to compute.
#'  Cannot be more than n-1 samples. Default is `10`.
#'
#' @return A `nacho` object; read its content with [nacho_counts()],
#'   [nacho_samples()], [nacho_probes()] and [nacho_qc()].
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   library(GEOquery)
#'   library(NACHO)
#'
#'   # Import data from GEO
#'   gse <- GEOquery::getGEO(GEO = "GSE74821")
#'   targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
#'   GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
#'   utils::untar(
#'     tarfile = file.path(tempdir(), "GSE74821", "GSE74821_RAW.tar"),
#'     exdir = file.path(tempdir(), "GSE74821")
#'   )
#'   targets$IDFILE <- list.files(
#'     path = file.path(tempdir(), "GSE74821"),
#'     pattern = ".RCC.gz$"
#'   )
#'   targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
#'   utils::write.csv(
#'     x = targets,
#'     file = file.path(tempdir(), "GSE74821", "Samplesheet.csv")
#'   )
#'
#'   # Read RCC files and format
#'   nacho <- load_rcc(
#'     data_directory = file.path(tempdir(), "GSE74821"),
#'     ssheet_csv = file.path(tempdir(), "GSE74821", "Samplesheet.csv"),
#'     id_colname = "IDFILE"
#'   )
#' }
#'
load_rcc <- function(
  data_directory,
  ssheet_csv,
  id_colname = NULL,
  housekeeping_genes = NULL,
  housekeeping_predict = FALSE,
  housekeeping_norm = TRUE,
  normalisation_method = "GEO",
  n_comp = 10
) {
  if (missing(data_directory) || missing(ssheet_csv)) {
    nacho_abort(
      "{.arg data_directory} and {.arg ssheet_csv} must both be provided.",
      class = "bad_argument"
    )
  }

  check_string(data_directory)
  if (!dir.exists(data_directory)) {
    nacho_abort(
      "The directory {.path {data_directory}} does not exist.",
      class = "missing_file"
    )
  }
  data_directory <- normalizePath(data_directory)
  normalisation_method <- check_settings(
    housekeeping_genes,
    housekeeping_predict,
    housekeeping_norm,
    normalisation_method,
    n_comp
  )

  if (is.character(ssheet_csv) && length(ssheet_csv) > 1) {
    if (is.null(names(ssheet_csv))) {
      ssheet_csv <- data.frame(IDFILE = ssheet_csv)
    } else {
      ssheet_csv <- utils::stack(ssheet_csv)
      names(ssheet_csv) <- c("IDFILE", "label")
    }
    id_colname <- "IDFILE"
  }
  if (rlang::is_string(ssheet_csv)) {
    if (!file.exists(ssheet_csv)) {
      nacho_abort(
        "The sample sheet {.file {ssheet_csv}} does not exist.",
        class = "missing_file"
      )
    }
    ssheet_csv <- data.table::fread(file = ssheet_csv, header = TRUE, sep = ",")
  }
  if (!is.data.frame(ssheet_csv)) {
    nacho_abort(
      "{.arg ssheet_csv} must be a data frame or the path to a CSV file, not {.obj_type_friendly {ssheet_csv}}.",
      class = "bad_argument"
    )
  }
  check_string(id_colname)
  check_column(id_colname, ssheet_csv)

  nacho_df <- data.table::as.data.table(ssheet_csv)
  nacho_df[["file_path"]] <- file.path(data_directory, nacho_df[[id_colname]])

  missing_files <- unique(nacho_df[[id_colname]][
    !file.exists(nacho_df[["file_path"]])
  ])
  if (length(missing_files) > 0) {
    nacho_abort(
      c(
        paste0(
          "{length(missing_files)} value{?s} of {.field {id_colname}} {?does/do} not match ",
          "an RCC file in {.path {data_directory}}."
        ),
        x = "Missing: {.file {utils::head(missing_files, 5)}}{if (length(missing_files) > 5) ', ...'}.",
        i = "Check that {.arg id_colname} holds file names, including the {.val .RCC} or {.val .RCC.gz} extension."
      ),
      class = "missing_file"
    )
  }
  files <- unique(nacho_df[["file_path"]])
  nacho_progress_step("Reading {length(files)} RCC files")
  parsed <- lapply(files, read_rcc)
  is_plexset <- vapply(
    parsed,
    function(p) is_plexset_classes(p[["code_summary"]][["CodeClass"]]),
    logical(1)
  )
  if (any(is_plexset) && !all(is_plexset)) {
    nacho_abort(
      c(
        "RCC files mix PlexSet and single-sample files.",
        i = "Load each kind of file separately."
      ),
      class = "mixed_rcc_types"
    )
  }
  rcc_type <- if (all(is_plexset)) "n8" else "n1"
  duplicated_ids <- duplicated_values(nacho_df[[id_colname]])
  if (rcc_type == "n8" && !"plexset_id" %in% names(nacho_df)) {
    if (length(duplicated_ids) > 0) {
      abort_duplicate_ids(duplicated_ids, id_colname)
    }
    nacho_df <- nacho_df[rep(seq_len(nrow(nacho_df)), each = 8), ]
    nacho_df[["plexset_id"]] <- rep(
      paste0("S", seq_len(8)),
      times = nrow(nacho_df) / 8
    )
  }
  if (rcc_type == "n8" && "plexset_id" %in% names(nacho_df)) {
    abort_bad_plexset_ids(nacho_df[["plexset_id"]])
    abort_duplicate_plexset_pairs(nacho_df, id_colname)
  }
  if (rcc_type == "n1" && length(duplicated_ids) > 0) {
    abort_duplicate_ids(duplicated_ids, id_colname)
  }

  versions <- unique(do.call(
    rbind,
    lapply(parsed, function(p) {
      data.frame(
        file = unname(p[["attributes"]]["Header.header_FileVersion"]),
        software = unname(p[["attributes"]]["Header.header_SoftwareVersion"])
      )
    })
  ))
  if (nrow(versions) > 1) {
    nacho_abort(
      c(
        "RCC files come from more than one NanoString file or software version.",
        "*" = "File versions: {.val {unique(versions[['file']])}}.",
        "*" = "Software versions: {.val {unique(versions[['software']])}}.",
        i = "Load each version separately."
      ),
      class = "mixed_versions"
    )
  }

  nacho_progress_step("Assembling {nrow(nacho_df)} sample{?s}")
  file_index <- match(nacho_df[["file_path"]], files)
  pieces <- lapply(parsed, rcc_samples)
  sample_codes <- lapply(seq_len(nrow(nacho_df)), function(k) {
    piece <- pieces[[file_index[k]]]
    if (rcc_type == "n8") piece[[nacho_df[["plexset_id"]][k]]] else piece[[1]]
  })
  if (rcc_type == "n8") {
    nacho_df[[id_colname]] <- paste(
      nacho_df[[id_colname]],
      nacho_df[["plexset_id"]],
      sep = "_"
    )
  }
  ids <- as.character(nacho_df[[id_colname]])

  probe_counts <- build_probe_counts(
    codes = data.table::rbindlist(sample_codes),
    column = rep(seq_along(sample_codes), vapply(sample_codes, nrow, 1L)),
    ids = ids,
    clash_message = c(
      "The same probe name has different code classes or accessions across RCC files.",
      x = "Probe{?s}: {.val {utils::head(clashes, 5)}}.",
      i = "Load files from the same CodeSet together."
    ),
    clash_class = "rcc_parse"
  )
  probes <- probe_counts[["probes"]]
  counts <- probe_counts[["counts"]]

  attributes <- data.table::rbindlist(
    lapply(parsed, function(p) {
      as.list(c(p[["attributes"]], Messages = p[["messages"]]))
    }),
    fill = TRUE
  )
  sheet <- as.data.frame(nacho_df)
  sheet <- sheet[, setdiff(names(sheet), "file_path"), drop = FALSE]
  samples <- cbind(
    column_first(sheet, id_colname),
    as.data.frame(attributes)[file_index, , drop = FALSE]
  )
  sample_order <- order(ids, method = "radix")
  samples <- samples[sample_order, , drop = FALSE]
  rownames(samples) <- NULL
  counts <- counts[, sample_order, drop = FALSE]

  housekeeping_norm <- resolve_housekeeping_norm(
    probes[["CodeClass"]],
    housekeeping_genes,
    housekeeping_predict,
    housekeeping_norm
  )

  nacho_progress_step("Computing quality-control metrics and normalising")
  build_nacho(
    counts = counts,
    probes = probes,
    samples = samples,
    settings = list(
      id_colname = id_colname,
      housekeeping_genes = housekeeping_genes,
      housekeeping_predict = housekeeping_predict,
      housekeeping_norm = housekeeping_norm,
      normalisation_method = normalisation_method,
      n_comp = as.integer(n_comp)
    ),
    thresholds = default_thresholds(),
    rcc_type = rcc_type,
    provenance = new_provenance(
      data_directory = data_directory,
      file_version = versions[["file"]],
      software_version = versions[["software"]]
    )
  )
}

#' Turn housekeeping normalisation off when no housekeeping gene is available
#'
#' @param code_class The code class of each probe.
#'
#' @return `housekeeping_norm`, set to `FALSE` with a warning when there are
#'   no `Housekeeping` probes, no `housekeeping_genes` and no prediction.
#'
#' @keywords internal
#' @noRd
resolve_housekeeping_norm <- function(
  code_class,
  housekeeping_genes,
  housekeeping_predict,
  housekeeping_norm
) {
  if (
    !any(grepl("Housekeeping", code_class)) &&
      is.null(housekeeping_genes) &&
      !housekeeping_predict &&
      housekeeping_norm
  ) {
    nacho_warn(
      c(
        "Housekeeping normalisation is off, because no housekeeping genes are available.",
        i = paste0(
          "There are no {.val Housekeeping} probes, {.arg housekeeping_genes} is {.code NULL} ",
          "and {.arg housekeeping_predict} is {.code FALSE}."
        )
      ),
      class = "no_housekeeping"
    )
    return(FALSE)
  }
  housekeeping_norm
}

#' Build the probe table and the counts matrix from long probe counts
#'
#' @param codes A data frame with the columns `CodeClass`, `Name`, `Accession`
#'   and `Count`, one row per probe and sample.
#' @param column The column of the counts matrix for each row of `codes`.
#' @param ids The sample identifiers, used as the column names.
#' @param clash_message The cli message raised when a probe name has more than
#'   one code class or accession. It can refer to `clashes`, the clashing names.
#' @param clash_class The error class, without the `nacho_error_` prefix.
#' @param call The call to report in the error.
#'
#' @return A list with `probes`, the unique probes ordered by code class then
#'   name, and `counts`, an integer matrix of probes by samples.
#'
#' @keywords internal
#' @noRd
build_probe_counts <- function(
  codes,
  column,
  ids,
  clash_message,
  clash_class,
  call = rlang::caller_env()
) {
  probe_columns <- c("CodeClass", "Name", "Accession")
  probes <- unique(data.table::as.data.table(codes), by = probe_columns)
  probes <- as.data.frame(probes)[, probe_columns]
  probes <- probes[
    order(probes[["CodeClass"]], probes[["Name"]], method = "radix"),
  ]
  rownames(probes) <- NULL
  # Used only inside the cli glue string of clash_message.
  clashes <- duplicated_values(probes[["Name"]]) # nolint: object_usage_linter.
  if (length(clashes) > 0) {
    nacho_abort(clash_message, class = clash_class, call = call)
  }
  counts <- matrix(
    NA_integer_,
    nrow = nrow(probes),
    ncol = length(ids),
    dimnames = list(probes[["Name"]], ids)
  )
  counts[cbind(match(codes[["Name"]], probes[["Name"]]), column)] <-
    as.integer(codes[["Count"]])
  list(probes = probes, counts = counts)
}

abort_duplicate_ids <- function(
  duplicated_ids,
  id_colname,
  call = rlang::caller_env()
) {
  nacho_abort(
    c(
      "{.field {id_colname}} contains duplicated values: {.val {utils::head(duplicated_ids, 3)}}.",
      i = "PlexSet RCC files hold 8 samples each; add a {.field plexset_id} column ({.val S1} to {.val S8}).",
      i = "For single-sample RCC files, make {.field {id_colname}} unique."
    ),
    class = "duplicate_id",
    call = call
  )
}

abort_bad_plexset_ids <- function(plexset_id, call = rlang::caller_env()) {
  valid_values <- paste0("S", seq_len(8))
  bad_values <- unique(plexset_id[!plexset_id %in% valid_values])
  if (length(bad_values) > 0) {
    nacho_abort(
      c(
        "{.field plexset_id} must be one of {.val {valid_values}}.",
        x = "Found: {.val {utils::head(bad_values, 5)}}."
      ),
      class = "bad_argument",
      call = call
    )
  }
  invisible(plexset_id)
}

abort_duplicate_plexset_pairs <- function(
  nacho_df,
  id_colname,
  call = rlang::caller_env()
) {
  pairs <- paste(nacho_df[[id_colname]], nacho_df[["plexset_id"]], sep = "\r")
  duplicated_ids <- unique(nacho_df[[id_colname]][duplicated(pairs)])
  if (length(duplicated_ids) > 0) {
    nacho_abort(
      c(
        paste0(
          "{.field {id_colname}} and {.field plexset_id} together contain duplicated pairs: ",
          "{.val {utils::head(duplicated_ids, 3)}}."
        ),
        i = "Each PlexSet sample needs a unique {.field {id_colname}}/{.field plexset_id} pair."
      ),
      class = "duplicate_id",
      call = call
    )
  }
  invisible(nacho_df)
}
