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
#'     and all computed columns, *i.e.*, quality-control metrics and counts, with one sample per row.}
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
  if (missing(data_directory) | missing(ssheet_csv)) {
    stop('[NACHO] "data_directory" and "ssheet_csv" must be provided.')
  }
  data_directory <- normalizePath(data_directory, mustWork = TRUE)
  if (is.vector(ssheet_csv, "character") & length(ssheet_csv) > 1) {
    if (is.null(names(ssheet_csv))) {
      ssheet_csv <- data.frame(IDFILE = ssheet_csv)
    } else {
      ssheet_csv <- utils::stack(ssheet_csv)
      names(ssheet_csv) <- c("IDFILE", "label")
    }
    id_colname <- "IDFILE"
  }

  if (
    is.null(id_colname) & (
      inherits(ssheet_csv, "data.frame") | (
        is.vector(ssheet_csv, "character") & length(ssheet_csv) == 1
      )
    )
  ) {
    stop('[NACHO] "id_colname" must be provided as a column of "ssheet_csv".')
  }

  message("[NACHO] Importing RCC files.")
  nacho_df <- switch(
    EXPR = paste(as.integer(inherits(ssheet_csv, c("data.frame", "character"), TRUE) > 0), collapse = ""),
    "10" = ssheet_csv,
    "01" = data.table::fread(file = ssheet_csv, header = TRUE, sep = ",", stringsAsFactors = FALSE),
    stop('[NACHO] "ssheet_csv" must be a "data.frame" or path to csv.')
  )

  nacho_df <- data.table::setDT(nacho_df)[
    j = `:=`(
      "file_path" = file.path(data_directory, nacho_df[[id_colname]])
    )
  ]

  if (nacho_df[j = !all(sapply(X = file_path, FUN = file.exists))]) {
    stop('[NACHO] Not all values from "id_colname" are mapped to a RCC file.')
  }

  if (anyDuplicated(nacho_df[[id_colname]]) != 0 & !"plexset_id" %in% colnames(nacho_df)) {
    stop(
      '[NACHO] "id_colname" contains duplicates and "plexset_id" was not provided.\n',
      '  For PlexSet RCC files, "plexset_id" column is required to identify samples.'
    )
  }

  if (anyDuplicated(nacho_df[[id_colname]]) != 0) {
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
  cat("\n")

  message("[NACHO] Performing QC and formatting data.")
  has_hkg <- any(grepl("Housekeeping", nacho_df[["CodeClass"]]))
  if (!has_hkg & is.null(housekeeping_genes) & !housekeeping_predict & housekeeping_norm) {
    message(paste(
      '[NACHO] "housekeeping_norm" has been set to FALSE.', "  Note:",
      if (has_hkg) "" else "  - No default housekeeping genes available in your data;",
      '  - "housekeeping_genes" is NULL;', '  - "housekeeping_predict" is FALSE.',
      sep = "\n"
    ))
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

  message(
    paste0(
      '[NACHO] Normalising data using "', normalisation_method, '" method ',
      if (housekeeping_norm) "with" else "without", " housekeeping genes."
    )
  )
  nacho_object[["nacho"]][["Count_Norm"]] <- normalise_counts(
    data = nacho_object[["nacho"]],
    housekeeping_norm = housekeeping_norm
  )

  message(paste(
    "[NACHO] Returning a list.",
    "  $ access              : character",
    "  $ housekeeping_genes  : character",
    "  $ housekeeping_predict: logical",
    "  $ housekeeping_norm   : logical",
    "  $ normalisation_method: character",
    "  $ remove_outliers     : logical",
    "  $ n_comp              : numeric",
    "  $ data_directory      : character",
    "  $ pc_sum              : data.frame",
    "  $ nacho               : data.frame",
    "  $ outliers_thresholds : list",
    sep = "\n"
  ))

  class(nacho_object) <- "nacho"

  nacho_object
}
