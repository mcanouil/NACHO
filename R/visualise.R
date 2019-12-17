#' Visualise quality-control metrics using a shiny app
#'
#' This function allows to visualise results from [load_rcc] or [normalise]
#' several quality-control metrics in an interactive [shiny] application,
#' in which thresholds can be customised and exported.
#'
#' @inheritParams normalise
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   data(GSE74821)
#'   # Must be run in an interactive R session!
#'   visualise(GSE74821)
#' }
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
#'   nacho <- load_rcc(
#'     data_directory = paste0(tempdir(), "/GSE74821"),
#'     ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
#'     id_colname = "IDFILE"
#'   )
#'   visualise(nacho)
#'
#'   # (re)Normalise data by removing outliers
#'   nacho_norm <- normalise(
#'     nacho_object = nacho,
#'     remove_outliers = TRUE
#'   )
#'   visualise(nacho_norm)
#'
#'   # (re)Normalise data with "GLM" method and removing outliers
#'   nacho_norm <- normalise(
#'     nacho_object = nacho,
#'     normalisation_method = "GLM",
#'     remove_outliers = TRUE
#'   )
#'   visualise(nacho_norm)
#' }
#'
visualise <- function(nacho_object) {
  if (!inherits(nacho_object, "nacho")) {
    stop(
      '[NACHO] "nacho_object" must be of class "nacho" from "load_rcc()" and/or "normalise()" !'
    )
  }
  if (missing(nacho_object)) {
    stop(
      '[NACHO] "nacho_object" is missing, results from "load_rcc()" and/or "normalise()" is mandatory!'
    )
  }
  mandatory_fields <- c(
    "access",
    "housekeeping_genes",
    "housekeeping_predict",
    "housekeeping_norm",
    "normalisation_method",
    "remove_outliers",
    "n_comp",
    "data_directory",
    "pc_sum",
    "nacho",
    "outliers_thresholds",
    "raw_counts",
    "normalised_counts"
  )
  if (!all(mandatory_fields%in%names(nacho_object))) {
    stop(
      '[NACHO] Mandatory fields are missing in "', substitute(nacho_object), '"!\n',
      '  "load_rcc()" and/or "normalise()" must be called before "visualise()".'
    )
  }

  nacho_object <- check_outliers(nacho_object)
  shiny::shinyOptions(nacho_object = nacho_object)
  on.exit(shiny::shinyOptions(nacho_object = NULL))

  if (!interactive()) {
    stop('[NACHO] Must be run in an interactive R session!')
  }

  shiny::runApp(system.file("app", package = "NACHO"))
}



#' @export
#' @rdname visualise
#' @usage NULL
visualize <- visualise
