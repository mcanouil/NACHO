#' summarise
#'
#' @param data_directory [character] A character string of the directory where the data are stored.
#' @param ssheet_csv [character/data.frame] Either a string with the name of the CSV of the samplesheet
#'   or the samplesheet as a \code{data.frame}.
#'   Should contain a column that matches the file names in the folder.
#' @param id_colname [character] Character string of the column in \code{ssheet_csv} that matches
#'   the file names in \code{data_directory}.
#' @param housekeeping_genes [vector(character)] A vector of names of the miRNAs/mRNAs
#'   that should be used as housekeeping genes. Default is \code{NULL}.
#' @param housekeeping_predict [logical] Boolean to indicate whether the housekeeping genes
#'   should be predicted (\code{TRUE}) or not (\code{FALSE}). Default is \code{FALSE}.
#' @param housekeeping_norm [logical] Boolean to indicate whether the housekeeping normalisation should be performed.
#'   Default is \code{TRUE}.
#' @param normalisation_method [character] Either \code{"GEO"} or \code{"GLM"}.
#'   Character string to indicate normalisation using the geometric mean (\code{"GEO"})
#'   or a generalized linear model (\code{"GLM"}). Default is \code{"GEO"}.
#' @param n_comp [numeric] Number indicating the number of principal components to compute.
#'  Cannot be more than n-1 samples. Default is \code{10}.
#'
#' @return [list] A list containing parameters and data:
#' \describe{
#'   \item{access}{[character] Value passed to \code{\link{summarise}} in \code{id_colname}.}
#'   \item{housekeeping_genes}{[character] Value passed to \code{\link{summarise}}.}
#'   \item{housekeeping_predict}{[logical] Value passed to \code{\link{summarise}}.}
#'   \item{housekeeping_norm}{[logical] Value passed to \code{\link{summarise}}.}
#'   \item{normalisation_method}{[character] Value passed to \code{\link{summarise}}.}
#'   \item{remove_outliers}{[logical] \code{FALSE}.}
#'   \item{n_comp}{[ numeric] Value passed to \code{\link{summarise}}.}
#'   \item{data_directory}{[character] Value passed to \code{\link{summarise}}.}
#'   \item{pc_sum}{[data.frame] A \code{data.frame} with \code{n_comp} rows and four columns:
#'    "Standard deviation", "Proportion of Variance", "Cumulative Proportion" and "PC".}
#'   \item{nacho}{[data.frame] A \code{data.frame} with all columns from the sample sheet \code{ssheet_csv}
#'   and all computed columns, i.e., quality-control metrics and counts, with one sample per row.}
#'   \item{outliers_thresholds}{[list] A \code{list} of the default quality-control thresholds.}
#'   \item{raw_counts}{[data.frame] Raw counts with probes as rows and samples as columns.
#'   With \code{"CodeClass"} (first column), the type of the probes and
#'   \code{"Name"} (second column), the Name of the probes.}
#'   \item{normalised_counts}{[data.frame] Normalised counts with probes as rows and samples as columns.
#'   With \code{"CodeClass"} (first column)), the type of the probes and
#'   \code{"Name"} (second column), the name of the probes.}
#' }
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(GEOquery)
#'   library(NACHO)
#'
#'   # Import data from GEO
#'   gse <- GEOquery::getGEO(GEO = "GSE74821")
#'   targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
#'   GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
#'   utils::untar(
#'     tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"),
#'     exdir = paste0(tempdir(), "/GSE74821")
#'   )
#'   targets$IDFILE <- list.files(
#'     path = paste0(tempdir(), "/GSE74821"),
#'     pattern = ".RCC.gz$"
#'   )
#'   targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
#'   utils::write.csv(
#'     x = targets,
#'     file = paste0(tempdir(), "/GSE74821/Samplesheet.csv")
#'   )
#'
#'   # Read RCC files and format
#'   nacho <- summarise(
#'     data_directory = paste0(tempdir(), "/GSE74821"),
#'     ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
#'     id_colname = "IDFILE"
#'   )
#'
#' }
#'
summarise <- function(
  data_directory,
  ssheet_csv,
  id_colname,
  housekeeping_genes = NULL,
  housekeeping_predict = FALSE,
  housekeeping_norm = TRUE,
  normalisation_method = "GEO",
  n_comp = 10
) {
  if (missing(data_directory) | missing(ssheet_csv) | missing(id_colname)) {
    stop('[NACHO] "data_directory", "ssheet_csv" and "id_colname" must be provided.')
  }
  data_directory <- normalizePath(data_directory)

  message("[NACHO] Importing RCC files.")
  nacho_df <- switch(
    EXPR = class(ssheet_csv),
    "data.frame" = ssheet_csv,
    "character" = utils::read.csv(file = ssheet_csv, header = TRUE, sep = ",", stringsAsFactors = FALSE),
    stop('[NACHO] "ssheet_csv" must be a "data.frame" or path to csv.')
  )

  nacho_df <- tibble::as_tibble(nacho_df)
  nacho_df[["file_path"]] <- paste(data_directory, nacho_df[[id_colname]], sep = "/")
  nacho_df[["file_exists"]] <- sapply(X = nacho_df[["file_path"]], FUN = file.exists)
  progress <- dplyr::progress_estimated(length(nacho_df[["file_path"]]) + 2)
  nacho_df[["rcc_content"]] <- lapply(
    X = nacho_df[["file_path"]],
    FUN = function(ifile) {
      progress$tick()$print()
      read_rcc(file = ifile)
    }
  )
  progress$pause(0.05)$tick()$print()

  column_to_unnest <- c("rcc_content", "Code_Summary")
  nacho_df <- tidyr::unnest(data = nacho_df, rcc_content = get(column_to_unnest[1]), .drop = FALSE)
  nacho_df <- tidyr::unnest(data = nacho_df, Code_Summary = get(column_to_unnest[2]), .drop = FALSE)
  nacho_df[["CodeClass"]] <- gsub("Endogenous.*", "Endogenous", nacho_df[["CodeClass"]])

  if ("plexset_id" %in% colnames(nacho_df)) {
    type_set <- "n8"
    nacho_df <- tidyr::unite(data = nacho_df, col = !!id_colname, id_colname, "plexset_id")
  } else {
    type_set <- "n1"
  }
  progress$pause(0.05)$tick()$print()
  cat("\n")

  message("[NACHO] Performing QC and formatting data.")
  has_hkg <- any(grepl("Housekeeping", nacho_df[["CodeClass"]]))
  if (!has_hkg & is.null(housekeeping_genes) & !housekeeping_predict & housekeeping_norm) {
    message(
      paste(
        '[NACHO] "housekeeping_norm" has been set to FALSE.',
        "  Note:",
        if (has_hkg) {
          ""
        } else {
          "  - No default housekeeping genes available in your data;"
        },
        '  - "housekeeping_genes" is NULL;',
        '  - "housekeeping_predict" is FALSE.',
        sep = "\n"
      )
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

  nacho_object[["outliers_thresholds"]] <- list(
    BD = c(0.1, 2.25),
    FoV = 75,
    LoD = 2,
    PC = 0.95,
    Positive_factor = c(1/4, 4),
    House_factor = c(1/11, 11)
  )

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

  raw_counts <- format_counts(
    data = nacho_object[["nacho"]],
    id_colname = id_colname,
    count_column = "Count"
  )
  nacho_object[["raw_counts"]] <- raw_counts

  norm_counts <- format_counts(
    data = nacho_object[["nacho"]],
    id_colname = id_colname,
    count_column = "Count_Norm"
  )
  nacho_object[["normalised_counts"]] <- norm_counts

  attributes(nacho_object) <- c(attributes(nacho_object), RCC_type = type_set)

  message(paste(
    "[NACHO] Returning a list.",
    "  $ access              : character",
    "  $ housekeeping_genes  : character" ,
    "  $ housekeeping_predict: logical",
    "  $ housekeeping_norm   : logical",
    "  $ normalisation_method: character",
    "  $ remove_outliers     : logical",
    "  $ n_comp              : numeric",
    "  $ data_directory      : character",
    "  $ pc_sum              : data.frame",
    "  $ nacho               : data.frame",
    "  $ outliers_thresholds : list",
    "  $ raw_counts          : data.frame",
    "  $ normalised_counts   : data.frame",
    sep = "\n"
  ))

  nacho_object
}


#' @export
#' @rdname summarise
#' @usage NULL
summarize <- summarise