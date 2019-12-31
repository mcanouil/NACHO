#' A `"nacho"` object containing 20 samples of GSE74821 dataset
#'
#' NanoString nCounter RUO-PAM50 Gene Expression Custom CodeSet
#'
#' @format A [[list]] object of class `"nacho"`.
#'
#' @source [GSE74821](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE74821)
"GSE74821"

# library(GEOquery)
# library(NACHO)
#
# gse <- GEOquery::getGEO(GEO = "GSE74821")
# targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
# GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
# utils::untar(file.path(tempdir(), "GSE74821", "GSE74821_RAW.tar"), exdir = file.path(tempdir(), "GSE74821"))
# targets$IDFILE <- list.files(path = file.path(tempdir(), "GSE74821"), pattern = ".RCC.gz$")
# targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
# select_cartridge <- unlist(lapply(
#   X = c(
#     "20111102_20111102-9741-1", "20111102_20111102-9741-2",
#     "20111230_20111228-9741-2-0044", "20111230_20111228-9741-4-0044"
#     ),
#   FUN = grep,
#   x = targets$IDFILE
# ))
#
# GSE74821 <- load_rcc(
#   data_directory = file.path(tempdir(), "GSE74821"),
#   ssheet_csv = targets[select_cartridge, ],
#   id_colname = "IDFILE",
#   housekeeping_genes = NULL,
#   housekeeping_predict = FALSE,
#   housekeeping_norm = TRUE,
#   normalisation_method = "GLM",
#   n_comp = 10
# )
# GSE74821$data_directory <- "~/"
# usethis::use_data(GSE74821, overwrite = TRUE)
