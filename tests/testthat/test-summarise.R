context("summarise()")

test_that("using GEO GSE74821", {
  # library(GEOquery)
  # library(NACHO) # devtools::load_all("NACHO")
  gse <- GEOquery::getGEO(GEO = "GSE74821")
  targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
  GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
  utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
  targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
  targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
  utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
  GSE74821 <- summarise(
    data_directory = paste0(tempdir(), "/GSE74821"),
    ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
    id_colname = "IDFILE",
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = TRUE,
    normalisation_method = "GLM",
    n_comp = 10
  )
  expect_identical(class(GSE74821), "list")
})

test_that("using GEO GSE74821 with prediction", {
  # library(GEOquery)
  # library(NACHO) # devtools::load_all("NACHO")
  gse <- GEOquery::getGEO(GEO = "GSE74821")
  targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
  GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
  utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
  targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
  targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
  utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
  GSE74821 <- summarise(
    data_directory = paste0(tempdir(), "/GSE74821"),
    ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
    id_colname = "IDFILE",
    housekeeping_genes = NULL,
    housekeeping_predict = TRUE,
    housekeeping_norm = TRUE,
    normalisation_method = "GLM",
    n_comp = 10
  )
  expect_identical(class(GSE74821), "list")
})


test_that("using GEO GSE70970", {
  # library(GEOquery)
  # library(NACHO) # devtools::load_all("NACHO")
  gse <- GEOquery::getGEO(GEO = "GSE70970")
  targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
  GEOquery::getGEOSuppFiles(GEO = "GSE70970", baseDir = tempdir())
  utils::untar(tarfile = paste0(tempdir(), "/GSE70970/GSE70970_RAW.tar"), exdir = paste0(tempdir(), "/GSE70970"))
  targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE70970"), pattern = ".RCC.gz$")
  targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
  utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE70970/Samplesheet.csv"))
  GSE70970 <- summarise(
    data_directory = paste0(tempdir(), "/GSE70970"),
    ssheet_csv = paste0(tempdir(), "/GSE70970/Samplesheet.csv"),
    id_colname = "IDFILE",
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = TRUE,
    normalisation_method = "GLM",
    n_comp = 10
  )
  expect_identical(class(GSE70970), "list")
})

test_that("using GEO GSE70970 with prediction", {
  # library(GEOquery)
  # library(NACHO) # devtools::load_all("NACHO")
  gse <- GEOquery::getGEO(GEO = "GSE70970")
  targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
  GEOquery::getGEOSuppFiles(GEO = "GSE70970", baseDir = tempdir())
  utils::untar(tarfile = paste0(tempdir(), "/GSE70970/GSE70970_RAW.tar"), exdir = paste0(tempdir(), "/GSE70970"))
  targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE70970"), pattern = ".RCC.gz$")
  targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
  utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE70970/Samplesheet.csv"))
  GSE70970 <- summarise(
    data_directory = paste0(tempdir(), "/GSE70970"),
    ssheet_csv = paste0(tempdir(), "/GSE70970/Samplesheet.csv"),
    id_colname = "IDFILE",
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = TRUE,
    normalisation_method = "GLM",
    n_comp = 10
  )
  expect_identical(class(GSE70970), "list")
})