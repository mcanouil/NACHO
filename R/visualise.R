#' Visualise quality-control metrics using a shiny app
#'
#' This function allows to visualise several quality-control metrics
#' in an interactive [shiny] application, in which thresholds can be customised
#' and exported to the global environment.
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
#'   nacho <- summarise(
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
      '  "summarise()" and/or "normalise()" must be called before "visualise()".'
    )
  }

  nacho_object <- check_outliers(nacho_object)

  # nocov start
  id_colname <- nacho_object[["access"]]
  housekeeping_genes <- nacho_object[["housekeeping_genes"]]
  housekeeping_norm <- nacho_object[["housekeeping_norm"]]
  pc_sum <- nacho_object[["pc_sum"]]
  nacho <- nacho_object[["nacho"]]
  save_path_default <- nacho_object[["data_directory"]]
  type_set <- attr(nacho_object, "RCC_type")
  outliers_env <- new.env()
  assign(x = "outliers_thresholds", value = nacho_object[["outliers_thresholds"]], envir = outliers_env)

  message(
    '[NACHO] Custom "outliers_thresholds" can be loaded for later use with:\n',
    '  outliers_thresholds <- readRDS("', tempdir(), '/outliers_thresholds.rds")'
  )

  # shiny::addResourcePath("logo", system.file("help", "figures", package = "NACHO"))

  if (!interactive()) {
    stop('[NACHO] Must be run in an interactive R session!')
  } else {
    shiny::runApp(system.file("app", package = "NACHO"))
  }
  # nocov end
}



#' @export
#' @rdname visualise
#' @usage NULL
visualize <- visualise
