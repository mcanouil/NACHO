test_that("missing directory", {
  expect_error(load_rcc(
    ssheet_csv = salmon_tidy,
    id_colname = "IDFILE",
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = TRUE,
    normalisation_method = "GLM",
    n_comp = 10
  ))
})

test_that("missing sample sheet", {
  expect_error(load_rcc(
    data_directory = "salmon_data",
    id_colname = "IDFILE",
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = TRUE,
    normalisation_method = "GLM",
    n_comp = 10
  ))
})

test_that("missing id_colname", {
  expect_error(load_rcc(
    data_directory = "salmon_data",
    ssheet_csv = salmon_tidy,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = TRUE,
    normalisation_method = "GLM",
    n_comp = 10
  ))
})

test_that("no housekeeping norm", {
  expect_s3_class(
    {
      load_rcc(
        data_directory = "salmon_data",
        ssheet_csv = salmon_tidy,
        id_colname = "IDFILE",
        housekeeping_genes = NULL,
        housekeeping_predict = FALSE,
        housekeeping_norm = FALSE,
        normalisation_method = "GLM",
        n_comp = 10
      )
    },
    "nacho"
  )
})

test_that("no housekeeping norm and prediction", {
  expect_s3_class(
    {
      load_rcc(
        data_directory = "salmon_data",
        ssheet_csv = salmon_tidy,
        id_colname = "IDFILE",
        housekeeping_genes = NULL,
        housekeeping_predict = TRUE,
        housekeeping_norm = FALSE,
        normalisation_method = "GLM",
        n_comp = 10
      )
    },
    "nacho"
  )
})

test_that("using GEO GSE74821", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("GEOquery")
  skip_if_not_installed("Biobase")
  gse <- try(GEOquery::getGEO(GEO = "GSE74821"), silent = TRUE)
  skip_if(inherits(gse, "try-error"), "GEO is unavailable.")
  targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
  geo_files <- try(
    GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir()),
    silent = TRUE
  )
  skip_if(inherits(geo_files, "try-error"), "GEO is unavailable.")
  utils::untar(
    file.path(tempdir(), "GSE74821", "GSE74821_RAW.tar"),
    exdir = file.path(tempdir(), "GSE74821")
  )
  targets$IDFILE <- list.files(
    path = file.path(tempdir(), "GSE74821"),
    pattern = ".RCC.gz$"
  )
  targets[] <- lapply(
    X = targets,
    FUN = iconv,
    from = "latin1",
    to = "ASCII"
  )

  # using GEO GSE74821
  expect_s3_class(
    {
      load_rcc(
        data_directory = file.path(tempdir(), "GSE74821"),
        ssheet_csv = head(targets, 20),
        id_colname = "IDFILE",
        housekeeping_genes = NULL,
        housekeeping_predict = FALSE,
        housekeeping_norm = TRUE,
        normalisation_method = "GLM",
        n_comp = 10
      )
    },
    "nacho"
  )

  # using GEO GSE74821 with prediction
  expect_s3_class(
    {
      load_rcc(
        data_directory = file.path(tempdir(), "GSE74821"),
        ssheet_csv = head(targets, 20),
        id_colname = "IDFILE",
        housekeeping_genes = NULL,
        housekeeping_predict = TRUE,
        housekeeping_norm = TRUE,
        normalisation_method = "GLM",
        n_comp = 10
      )
    },
    "nacho"
  )
})

test_that("using GEO GSE70970", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("GEOquery")
  skip_if_not_installed("Biobase")
  gse <- try(GEOquery::getGEO(GEO = "GSE70970"), silent = TRUE)
  skip_if(inherits(gse, "try-error"), "GEO is unavailable.")
  targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
  geo_files <- try(
    GEOquery::getGEOSuppFiles(GEO = "GSE70970", baseDir = tempdir()),
    silent = TRUE
  )
  skip_if(inherits(geo_files, "try-error"), "GEO is unavailable.")
  utils::untar(
    file.path(tempdir(), "GSE70970", "GSE70970_RAW.tar"),
    exdir = file.path(tempdir(), "GSE70970")
  )
  targets$IDFILE <- list.files(
    path = file.path(tempdir(), "GSE70970"),
    pattern = ".RCC.gz$"
  )
  targets[] <- lapply(
    X = targets,
    FUN = iconv,
    from = "latin1",
    to = "ASCII"
  )

  # using GEO GSE70970
  expect_s3_class(
    {
      load_rcc(
        data_directory = file.path(tempdir(), "GSE70970"),
        ssheet_csv = head(targets, 20),
        id_colname = "IDFILE",
        housekeeping_genes = NULL,
        housekeeping_predict = FALSE,
        housekeeping_norm = TRUE,
        normalisation_method = "GLM",
        n_comp = 10
      )
    },
    "nacho"
  )

  # using GEO GSE70970 with prediction
  expect_s3_class(
    {
      load_rcc(
        data_directory = file.path(tempdir(), "GSE70970"),
        ssheet_csv = head(targets, 20),
        id_colname = "IDFILE",
        housekeeping_genes = NULL,
        housekeeping_predict = TRUE,
        housekeeping_norm = TRUE,
        normalisation_method = "GLM",
        n_comp = 10
      )
    },
    "nacho"
  )

  # ssheet_csv as vector
  expect_s3_class(
    {
      load_rcc(
        data_directory = file.path(tempdir(), "GSE70970"),
        ssheet_csv = head(targets[["IDFILE"]], 20),
        id_colname = "IDFILE",
        housekeeping_predict = TRUE,
        housekeeping_norm = TRUE
      )
    },
    class = "nacho"
  )

  # ssheet_csv as vector without id_colname
  expect_s3_class(
    {
      load_rcc(
        data_directory = file.path(tempdir(), "GSE70970"),
        ssheet_csv = head(targets[["IDFILE"]], 20),
        housekeeping_predict = TRUE,
        housekeeping_norm = TRUE
      )
    },
    class = "nacho"
  )

  # ssheet_csv as a named vector
  expect_s3_class(
    {
      load_rcc(
        data_directory = file.path(tempdir(), "GSE70970"),
        ssheet_csv = `names<-`(
          head(targets[["IDFILE"]], 20),
          head(letters, 20)
        ),
        housekeeping_predict = TRUE,
        housekeeping_norm = TRUE
      )
    },
    class = "nacho"
  )

  # id_colname not defined when using df
  expect_error({
    load_rcc(
      data_directory = file.path(tempdir(), "GSE70970"),
      ssheet_csv = head(targets, 20),
      housekeeping_predict = TRUE,
      housekeeping_norm = TRUE
    )
  })
})

test_that("using RAW RCC multiplexed", {
  expect_s3_class(
    {
      load_rcc(
        data_directory = "salmon_data",
        ssheet_csv = salmon_tidy,
        id_colname = "IDFILE"
      )
    },
    "nacho"
  )
})

test_that("using RAW RCC multiplexed without plexset_id", {
  targets_tidy <- salmon_tidy
  targets_tidy$plexset_id <- NULL

  expect_error({
    load_rcc(
      data_directory = "salmon_data",
      ssheet_csv = targets_tidy,
      id_colname = "IDFILE"
    )
  })
})

test_that("using RAW RCC multiplexed with wrong path", {
  targets_tidy <- salmon_tidy
  targets_tidy$IDFILE[1] <- "something_wrong.RCC" # wrong path
  expect_error({
    load_rcc(
      data_directory = "salmon_data",
      ssheet_csv = targets_tidy,
      id_colname = "IDFILE"
    )
  })
})

test_that("Too high number of components", {
  expect_message(
    {
      load_rcc(
        data_directory = "salmon_data",
        ssheet_csv = salmon_tidy,
        id_colname = "IDFILE",
        n_comp = 1000
      )
    },
    "has been set to"
  )
})

test_that("plexset", {
  expect_s3_class(
    {
      load_rcc(
        data_directory = "plexset_data",
        ssheet_csv = plexset_tidy,
        id_colname = "IDFILE",
        housekeeping_predict = TRUE,
        housekeeping_norm = TRUE
      )
    },
    class = "nacho"
  )
})

test_that("heterogenous", {
  expect_error({
    load_rcc(
      data_directory = ".",
      ssheet_csv = plexset_salmon_tidy,
      id_colname = "IDFILE",
      housekeeping_predict = TRUE,
      housekeeping_norm = TRUE
    )
  })
})

test_that("PlexSet files are detected from their content", {
  unique_ids <- data.frame(IDFILE = basename(salmon_files))
  res <- suppressMessages(load_rcc(
    data_directory = test_path("salmon_data"),
    ssheet_csv = unique_ids,
    id_colname = "IDFILE"
  ))
  expect_identical(attr(res, "RCC_type"), "n8")
  expect_setequal(
    unique(res[["nacho"]][["IDFILE"]]),
    unique(salmon_nacho[["nacho"]][["IDFILE"]])
  )
  expect_false(all(res[["nacho"]][["is_outlier"]]))
})

test_that("PlexSet detection needs the exact PlexSet code classes", {
  single <- withr::local_tempfile(fileext = ".RCC")
  writeLines(c("<Code_Summary>", "Endogenous1,miR-1,MIMAT0000416,12"), single)
  plexset <- withr::local_tempfile(fileext = ".RCC")
  writeLines(
    c("<Code_Summary>", paste0("Endogenous", 1:8, "s,GENE,NM_1,12")),
    plexset
  )
  expect_false(NACHO:::is_plexset_rcc(single))
  expect_true(NACHO:::is_plexset_rcc(plexset))
})

test_that("PlexSet detection reads gzipped RCC files", {
  gz_file <- withr::local_tempfile(fileext = ".RCC.gz")
  connection <- gzfile(gz_file, "w")
  writeLines(readLines(salmon_files[[1]]), connection)
  close(connection)
  expect_true(NACHO:::is_plexset_rcc(gz_file))
})

test_that("load_rcc() leaves the caller's sample sheet unchanged", {
  sample_sheet <- salmon_tidy
  suppressMessages(load_rcc(
    data_directory = test_path("salmon_data"),
    ssheet_csv = sample_sheet,
    id_colname = "IDFILE"
  ))
  expect_identical(sample_sheet, salmon_tidy)
  expect_identical(class(sample_sheet), "data.frame")
})
