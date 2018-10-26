#' Presummarised data from GSE74821 (20 samples).
#'
#' NanoString nCounter RUO-PAM50 Gene Expression Custom CodeSet
#'
#' @format A 'nacho_set' object
#' @source \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE74821}
"GSE74821"

# library(GEOquery)
# gse <- getGEO(GEO = "GSE74821")
# targets <- pData(phenoData(gse[[1]]))
# getGEOSuppFiles(GEO = "GSE74821")
# untar(tarfile = "GSE74821/GSE74821_RAW.tar", exdir = "GSE74821")
# targets$IDFILE <- list.files(path = "./GSE74821/", pattern = ".RCC.gz$")
# targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
# write.csv(x = head(targets, 20), file = "GSE74821/Samplesheet.csv")
# library(NACHO)
# GSE74821 <- summarise(
#   data_directory = "GSE74821",
#   ssheet_csv = "GSE74821/Samplesheet.csv",
#   id_colname = "IDFILE",
#   housekeeping_genes = NULL,
#   housekeeping_predict = FALSE,
#   housekeeping_norm = TRUE,
#   normalisation_method = "GLM",
#   n_comp = 10
# )
# devtools::use_data(GSE74821, overwrite = TRUE)
