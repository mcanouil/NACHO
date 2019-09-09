context("summarise()")

test_that("missing directory", {
  gse <- try({GEOquery::getGEO(GEO = "GSE74821")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_error(gse <- GEOquery::getGEO(GEO = "GSE74821"))
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    expect_error(summarise(
      # data_directory = paste0(tempdir(), "/GSE74821"),
      ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    ))
  }
})

test_that("missing sample sheet", {
  gse <- try({GEOquery::getGEO(GEO = "GSE74821")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_error(gse <- GEOquery::getGEO(GEO = "GSE74821"))
  } else {
    gse <- GEOquery::getGEO(GEO = "GSE74821")
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    expect_error(summarise(
      data_directory = paste0(tempdir(), "/GSE74821"),
      # ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    ))
  }
})


test_that("missing id_colname", {
  gse <- try({GEOquery::getGEO(GEO = "GSE74821")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_error(gse <- GEOquery::getGEO(GEO = "GSE74821"))
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    expect_error(summarise(
      data_directory = paste0(tempdir(), "/GSE74821"),
      ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
      # id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    ))
  }
})

test_that("no housekeeping norm", {
  gse <- try({GEOquery::getGEO(GEO = "GSE74821")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_error(gse <- GEOquery::getGEO(GEO = "GSE74821"))
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    GSE74821 <- summarise(
      data_directory = paste0(tempdir(), "/GSE74821"),
      ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = FALSE,
      normalisation_method = "GLM",
      n_comp = 10
    )
    expect_type(GSE74821, "list")
  }
})

test_that("no housekeeping norm and prediction", {
  gse <- try({GEOquery::getGEO(GEO = "GSE74821")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_error(gse <- GEOquery::getGEO(GEO = "GSE74821"))
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    GSE74821 <- summarise(
      data_directory = paste0(tempdir(), "/GSE74821"),
      ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = TRUE,
      housekeeping_norm = FALSE,
      normalisation_method = "GLM",
      n_comp = 10
    )
    expect_type(GSE74821, "list")
  }
})



test_that("using GEO GSE74821", {
  gse <- try({GEOquery::getGEO(GEO = "GSE74821")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_error(gse <- GEOquery::getGEO(GEO = "GSE74821"))
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
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
    expect_type(GSE74821, "list")
  }
})

test_that("using GEO GSE74821 with prediction", {
  gse <- try({GEOquery::getGEO(GEO = "GSE74821")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_error(gse <- GEOquery::getGEO(GEO = "GSE74821"))
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
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
    expect_type(GSE74821, "list")
  }
})


test_that("using GEO GSE70970", {
  gse <- try({GEOquery::getGEO(GEO = "GSE70970")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_error(gse <- GEOquery::getGEO(GEO = "GSE70970"))
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE70970", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE70970/GSE70970_RAW.tar"), exdir = paste0(tempdir(), "/GSE70970"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE70970"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE70970/Samplesheet.csv"))
    closeAllConnections()
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
    expect_type(GSE70970, "list")
  }
})

test_that("using GEO GSE70970 with prediction", {
  gse <- try({GEOquery::getGEO(GEO = "GSE70970")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_error(gse <- GEOquery::getGEO(GEO = "GSE70970"))
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE70970", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE70970/GSE70970_RAW.tar"), exdir = paste0(tempdir(), "/GSE70970"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE70970"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE70970/Samplesheet.csv"))
    closeAllConnections()
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
    expect_type(GSE70970, "list")
  }
})
