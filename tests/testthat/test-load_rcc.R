test_that("missing directory", {
  gse <- try({GEOquery::getGEO(GEO = "GSE74821")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_s3_class(gse, "try-error")
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    expect_error(load_rcc(
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
    expect_s3_class(gse, "try-error")
  } else {
    gse <- GEOquery::getGEO(GEO = "GSE74821")
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    expect_error(load_rcc(
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
    expect_s3_class(gse, "try-error")
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    expect_error(load_rcc(
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
    expect_s3_class(gse, "try-error")
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    GSE74821 <- load_rcc(
      data_directory = paste0(tempdir(), "/GSE74821"),
      ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = FALSE,
      normalisation_method = "GLM",
      n_comp = 10
    )
    expect_s3_class(GSE74821, "nacho")
  }
})

test_that("no housekeeping norm and prediction", {
  gse <- try({GEOquery::getGEO(GEO = "GSE74821")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_s3_class(gse, "try-error")
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    GSE74821 <- load_rcc(
      data_directory = paste0(tempdir(), "/GSE74821"),
      ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = TRUE,
      housekeeping_norm = FALSE,
      normalisation_method = "GLM",
      n_comp = 10
    )
    expect_s3_class(GSE74821, "nacho")
  }
})

test_that("using GEO GSE74821", {
  gse <- try({GEOquery::getGEO(GEO = "GSE74821")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_s3_class(gse, "try-error")
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    GSE74821 <- load_rcc(
      data_directory = paste0(tempdir(), "/GSE74821"),
      ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    )
    expect_s3_class(GSE74821, "nacho")
  }
})

test_that("using GEO GSE74821 with prediction", {
  gse <- try({GEOquery::getGEO(GEO = "GSE74821")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_s3_class(gse, "try-error")
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"), exdir = paste0(tempdir(), "/GSE74821"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE74821"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE74821/Samplesheet.csv"))
    closeAllConnections()
    GSE74821 <- load_rcc(
      data_directory = paste0(tempdir(), "/GSE74821"),
      ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = TRUE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    )
    expect_s3_class(GSE74821, "nacho")
  }
})


test_that("using GEO GSE70970", {
  gse <- try({GEOquery::getGEO(GEO = "GSE70970")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_s3_class(gse, "try-error")
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE70970", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE70970/GSE70970_RAW.tar"), exdir = paste0(tempdir(), "/GSE70970"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE70970"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE70970/Samplesheet.csv"))
    closeAllConnections()
    GSE70970 <- load_rcc(
      data_directory = paste0(tempdir(), "/GSE70970"),
      ssheet_csv = paste0(tempdir(), "/GSE70970/Samplesheet.csv"),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    )
    expect_s3_class(GSE70970, "nacho")
  }
})

test_that("using GEO GSE70970 with prediction", {
  gse <- try({GEOquery::getGEO(GEO = "GSE70970")}, silent = TRUE)
  if (class(gse)=="try-error") { # when GEOQUERY is down
    closeAllConnections()
    expect_s3_class(gse, "try-error")
  } else {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    GEOquery::getGEOSuppFiles(GEO = "GSE70970", baseDir = tempdir())
    utils::untar(tarfile = paste0(tempdir(), "/GSE70970/GSE70970_RAW.tar"), exdir = paste0(tempdir(), "/GSE70970"))
    targets$IDFILE <- list.files(path = paste0(tempdir(), "/GSE70970"), pattern = ".RCC.gz$")
    targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
    utils::write.csv(x = head(targets, 20), file = paste0(tempdir(), "/GSE70970/Samplesheet.csv"))
    closeAllConnections()
    GSE70970 <- load_rcc(
      data_directory = paste0(tempdir(), "/GSE70970"),
      ssheet_csv = paste0(tempdir(), "/GSE70970/Samplesheet.csv"),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    )
    expect_s3_class(GSE70970, "nacho")
  }
})

test_that("using RAW RCC multiplexed", {
  rcc_files_directory <- system.file("extdata", package = "NACHO")

  targets <- data.frame(stringsAsFactors = FALSE,
    name = list.files(rcc_files_directory),
    datapath = list.files(rcc_files_directory, full.names = TRUE)
  )

  targets$IDFILE <- basename(targets$datapath)
  targets$plexset_id <- rep(list(paste0("S", 1:8)), each = nrow(targets))
  targets_tidy <- as.data.frame(tidyr::unnest(targets, "plexset_id"))

  salmon <- load_rcc(
    data_directory = rcc_files_directory,
    ssheet_csv = targets_tidy,
    id_colname = "IDFILE"
  )
  expect_s3_class(salmon, "nacho")
})

test_that("using RAW RCC multiplexed without plexset_id", {
  rcc_files_directory <- system.file("extdata", package = "NACHO")

  targets <- data.frame(stringsAsFactors = FALSE,
    name = list.files(rcc_files_directory),
    datapath = list.files(rcc_files_directory, full.names = TRUE)
  )

  targets$IDFILE <- basename(targets$datapath)
  # targets$plexset_id <- rep(list(paste0("S", 1:8)), each = nrow(targets))
  # targets_tidy <- as.data.frame(tidyr::unnest(targets, "plexset_id"))
  targets_tidy <- targets[rep(1:nrow(targets), 8), ] # create all samples without plexset_id

  expect_error({
    load_rcc(
      data_directory = rcc_files_directory,
      ssheet_csv = targets_tidy,
      id_colname = "IDFILE"
    )
  })
})

test_that("using RAW RCC multiplexed without plexset_id", {
  rcc_files_directory <- system.file("extdata", package = "NACHO")

  targets <- data.frame(stringsAsFactors = FALSE,
    name = list.files(rcc_files_directory),
    datapath = list.files(rcc_files_directory, full.names = TRUE)
  )

  targets$IDFILE <- basename(targets$datapath)
  targets$plexset_id <- rep(list(paste0("S", 1:8)), each = nrow(targets))
  targets$IDFILE[1] <- "something_wrong.RCC" # wrong path
  targets_tidy <- as.data.frame(tidyr::unnest(targets, "plexset_id"))

  expect_error({
    load_rcc(
      data_directory = rcc_files_directory,
      ssheet_csv = targets_tidy,
      id_colname = "IDFILE"
    )
  })
})

test_that("deprecated summarise", {
  rcc_files_directory <- system.file("extdata", package = "NACHO")

  targets <- data.frame(stringsAsFactors = FALSE,
    name = list.files(rcc_files_directory),
    datapath = list.files(rcc_files_directory, full.names = TRUE)
  )

  targets$IDFILE <- basename(targets$datapath)
  targets$plexset_id <- rep(list(paste0("S", 1:8)), each = nrow(targets))
  targets_tidy <- as.data.frame(tidyr::unnest(targets, "plexset_id"))

  expect_warning({
    summarise(
      data_directory = rcc_files_directory,
      ssheet_csv = targets_tidy,
      id_colname = "IDFILE"
    )
  })
})

test_that("deprecated summarize", {
  rcc_files_directory <- system.file("extdata", package = "NACHO")

  targets <- data.frame(stringsAsFactors = FALSE,
    name = list.files(rcc_files_directory),
    datapath = list.files(rcc_files_directory, full.names = TRUE)
  )

  targets$IDFILE <- basename(targets$datapath)
  targets$plexset_id <- rep(list(paste0("S", 1:8)), each = nrow(targets))
  targets_tidy <- as.data.frame(tidyr::unnest(targets, "plexset_id"))

  expect_warning({
    summarize(
      data_directory = rcc_files_directory,
      ssheet_csv = targets_tidy,
      id_colname = "IDFILE"
    )
  })
})

test_that("Too high number of components", {
  rcc_files_directory <- system.file("extdata", package = "NACHO")

  targets <- data.frame(stringsAsFactors = FALSE,
    name = list.files(rcc_files_directory),
    datapath = list.files(rcc_files_directory, full.names = TRUE)
  )

  targets$IDFILE <- basename(targets$datapath)
  targets$plexset_id <- rep(list(paste0("S", 1:8)), each = nrow(targets))
  targets_tidy <- as.data.frame(tidyr::unnest(targets, "plexset_id"))

  expect_message({
    load_rcc(
      data_directory = rcc_files_directory,
      ssheet_csv = targets_tidy,
      id_colname = "IDFILE",
      n_comp = 1000
    )
  }, "nacho", "has been set to")
})
