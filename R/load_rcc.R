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
#' @return [[list]] A list object of class `"nacho"`:
#' \describe{
#'   \item{`access`}{[[character]] Value passed to [`load_rcc()`] in `id_colname`.}
#'   \item{`housekeeping_genes`}{[[character]] Value passed to [`load_rcc()`].}
#'   \item{`housekeeping_predict`}{[[logical]] Value passed to [`load_rcc()`].}
#'   \item{`housekeeping_norm`}{[[logical]] Value passed to [`load_rcc()`].}
#'   \item{`normalisation_method`}{[[character]] Value passed to [`load_rcc()`].}
#'   \item{`remove_outliers`}{[[logical]] `FALSE`.}
#'   \item{`n_comp`}{[[numeric]] Value passed to [`load_rcc()`].}
#'   \item{`data_directory`}{[[character]] Value passed to [`load_rcc()`].}
#'   \item{`pc_sum`}{[[data.frame]] A `data.frame` with `n_comp` rows and four columns:
#'     "Standard deviation", "Proportion of Variance", "Cumulative Proportion" and "PC".}
#'   \item{`nacho`}{[[data.frame]] A `data.frame` with all columns from the sample sheet `ssheet_csv`
#'     and all computed columns, *i.e.*, quality-control metrics and counts, with one row per sample and probe.}
#'   \item{`outliers_thresholds`}{[[list]] A `list` of the (default) quality-control thresholds used.}
#' }
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
  file_path <- Code_Summary <- CodeClass <- NULL # no visible binding for global variable

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
  check_character(housekeeping_genes, allow_null = TRUE)
  check_bool(housekeeping_predict)
  check_bool(housekeeping_norm)
  normalisation_method <- check_choice(normalisation_method, c("GEO", "GLM"))
  check_count(n_comp)

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
  nacho_progress_step(
    "Reading {length(unique(nacho_df[['file_path']]))} RCC files"
  )

  is_plexset <- vapply(
    X = unique(nacho_df[["file_path"]]),
    FUN = is_plexset_rcc,
    FUN.VALUE = logical(1)
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
  has_duplicates <- anyDuplicated(nacho_df[[id_colname]]) != 0
  has_plexset_id <- "plexset_id" %in% colnames(nacho_df)
  if (has_duplicates && !(all(is_plexset) && has_plexset_id)) {
    # Used only inside the cli glue string below.
    dups <- unique(nacho_df[[id_colname]][duplicated(nacho_df[[id_colname]])]) # nolint: object_usage_linter.
    nacho_abort(
      c(
        "{.field {id_colname}} contains duplicated values: {.val {utils::head(dups, 3)}}.",
        i = "PlexSet RCC files hold 8 samples each; add a {.field plexset_id} column ({.val S1} to {.val S8}).",
        i = "For single-sample RCC files, make {.field {id_colname}} unique."
      ),
      class = "duplicate_id"
    )
  }
  if (all(is_plexset) && !has_plexset_id) {
    nacho_df <- nacho_df[rep(seq_len(nrow(nacho_df)), each = 8)]
    nacho_df[["plexset_id"]] <- rep(
      paste0("S", seq_len(8)),
      times = nrow(nacho_df) / 8
    )
  }

  if (all(is_plexset)) {
    type_set <- "n8"
    nacho_df <- merge(
      x = nacho_df,
      y = nacho_df[
        j = unique(.SD),
        .SDcols = c(id_colname, "file_path")
      ][
        j = data.table::rbindlist(lapply(X = file_path, FUN = read_rcc)),
        by = c(id_colname, "file_path")
      ],
      by = c(id_colname, "file_path", "plexset_id"),
      all.x = TRUE
    )[
      j = (id_colname) := apply(.SD, 1, paste, collapse = "_"),
      .SDcols = c(id_colname, "plexset_id")
    ]
    nacho_df <- nacho_df[
      j = unlist(Code_Summary, recursive = FALSE),
      by = setdiff(names(nacho_df), "Code_Summary")
    ][
      j = `:=`(CodeClass = sub("[0-8]+s$", "", CodeClass))
    ]
  } else {
    type_set <- "n1"
    nacho_df <- nacho_df[
      j = data.table::rbindlist(lapply(X = file_path, FUN = read_rcc)),
      by = c(unique(c(id_colname, "file_path", names(nacho_df))))
    ]
    nacho_df <- nacho_df[
      j = unlist(Code_Summary, recursive = FALSE),
      by = setdiff(names(nacho_df), "Code_Summary")
    ]
  }

  nanostring_versions <- nacho_df[
    j = unique(.SD),
    .SDcols = c("Header.header_FileVersion", "Header.header_SoftwareVersion")
  ]
  if (nrow(nanostring_versions) > 1) {
    nacho_abort(
      c(
        "RCC files come from more than one NanoString file or software version.",
        "*" = "File versions: {.val {unique(nanostring_versions[['Header.header_FileVersion']])}}.",
        "*" = "Software versions: {.val {unique(nanostring_versions[['Header.header_SoftwareVersion']])}}.",
        i = "Load each version separately."
      ),
      class = "mixed_versions"
    )
  }

  nacho_progress_step("Computing quality-control metrics")
  has_hkg <- any(grepl("Housekeeping", nacho_df[["CodeClass"]]))
  if (
    !has_hkg &&
      is.null(housekeeping_genes) &&
      !housekeeping_predict &&
      housekeeping_norm
  ) {
    nacho_warn(
      c(
        "Housekeeping normalisation is off, because no housekeeping genes are available.",
        i = paste0(
          "The RCC files have no {.val Housekeeping} probes, {.arg housekeeping_genes} is {.code NULL} ",
          "and {.arg housekeeping_predict} is {.code FALSE}."
        )
      ),
      class = "no_housekeeping"
    )
    housekeeping_norm <- FALSE
  }
  nacho_object <- qc_rcc(
    data_directory = data_directory,
    nacho_df = nacho_df,
    id_colname = id_colname,
    housekeeping_genes = housekeeping_genes,
    housekeeping_predict = housekeeping_predict,
    housekeeping_norm = housekeeping_norm,
    normalisation_method = normalisation_method,
    n_comp = n_comp
  )

  attributes(nacho_object) <- c(attributes(nacho_object), RCC_type = type_set)
  class(nacho_object) <- "nacho"

  ot <- list(
    BD = c(0.1, 2.25),
    FoV = 75,
    LoD = 2,
    PCL = 0.95,
    Positive_factor = c(1 / 4, 4),
    House_factor = c(1 / 11, 11)
  )
  nacho_object[["outliers_thresholds"]] <- ot
  nacho_object <- check_outliers(nacho_object)

  nacho_progress_step(
    paste0(
      "Normalising with the {.val {normalisation_method}} method ",
      "{if (housekeeping_norm) 'and' else 'without'} housekeeping genes"
    )
  )
  nacho_object[["nacho"]][["Count_Norm"]] <- normalise_counts(
    data = nacho_object[["nacho"]],
    housekeeping_norm = housekeeping_norm
  )

  nacho_object
}
