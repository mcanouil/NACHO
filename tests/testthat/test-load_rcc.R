test_that("missing directory", {
  expect_error(load_rcc(
    # data_directory = "salmon_data",
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
    # ssheet_csv = salmon_tidy,
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
    # id_colname = "IDFILE",
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = TRUE,
    normalisation_method = "GLM",
    n_comp = 10
  ))
})

test_that("no housekeeping norm", {
  expect_s3_class({
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
  }, "nacho")
})

test_that("no housekeeping norm and prediction", {
  expect_s3_class({
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
  }, "nacho")
})

test_that("using GEO", {
  skip_on_cran()
  gse <- try(GEOquery::getGEO(GEO = "GSE74821"), silent = TRUE)
  if (!inherits(gse, "try-error")) {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    geo_files <- try(GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir()), silent = TRUE)
    if (!inherits(geo_files, "try-error")) {
      utils::untar(file.path(tempdir(), "GSE74821", "GSE74821_RAW.tar"), exdir = file.path(tempdir(), "GSE74821"))
      targets$IDFILE <- list.files(path = file.path(tempdir(), "GSE74821"), pattern = ".RCC.gz$")
      targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")

      # test_that("using GEO GSE74821", {
        expect_s3_class({
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
        }, "nacho")
      # })

      # test_that("using GEO GSE74821 with prediction", {
        expect_s3_class({
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
        }, "nacho")
      # })
    }
  }
  closeAllConnections()

  gse <- try(GEOquery::getGEO(GEO = "GSE70970"), silent = TRUE)
  if (!inherits(gse, "try-error")) {
    targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
    geo_files <- try(GEOquery::getGEOSuppFiles(GEO = "GSE70970", baseDir = tempdir()), silent = TRUE)
    if (!inherits(geo_files, "try-error")) {
      utils::untar(file.path(tempdir(), "GSE70970", "GSE70970_RAW.tar"), exdir = file.path(tempdir(), "GSE70970"))
      targets$IDFILE <- list.files(path = file.path(tempdir(), "GSE70970"), pattern = ".RCC.gz$")
      targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")

      # test_that("using GEO GSE70970", {
        expect_s3_class({
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
        }, "nacho")
      # })

      # test_that("using GEO GSE70970 with prediction", {
        expect_s3_class({
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
        }, "nacho")
      # })

      # test_that("ssheet_csv as vector", {
        expect_s3_class({
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
      # })

      # test_that("ssheet_csv as vector without id_colname", {
        expect_s3_class({
            load_rcc(
              data_directory = file.path(tempdir(), "GSE70970"),
              ssheet_csv = head(targets[["IDFILE"]], 20),
              housekeeping_predict = TRUE,
              housekeeping_norm = TRUE
            )
          },
          class = "nacho"
        )
      # })

      # test_that("ssheet_csv as a named vector", {
        expect_s3_class({
            load_rcc(
              data_directory = file.path(tempdir(), "GSE70970"),
              ssheet_csv = `names<-`(
                head(targets[["IDFILE"]], 20),
                head(letters, 20)
              ),
              # id_colname = "IDFILE",
              housekeeping_predict = TRUE,
              housekeeping_norm = TRUE
            )
          },
          class = "nacho"
        )
      # })

      # test_that("id_colname not defined when using df", {
        expect_error({
          load_rcc(
            data_directory = file.path(tempdir(), "GSE70970"),
            ssheet_csv = head(targets, 20),
            housekeeping_predict = TRUE,
            housekeeping_norm = TRUE
          )
        })
      # })
    }
  }
  closeAllConnections()

  # test_that("using RAW RCC multiplexed", {
    expect_s3_class({
      load_rcc(
        data_directory = "salmon_data",
        ssheet_csv = salmon_tidy,
        id_colname = "IDFILE"
      )
    }, "nacho")
  # })

  # test_that("using RAW RCC multiplexed without plexset_id", {
    targets_tidy <- salmon_tidy
    targets_tidy$plexset_id <- NULL

    expect_error({
      load_rcc(
        data_directory = "salmon_data",
        ssheet_csv = targets_tidy,
        id_colname = "IDFILE"
      )
    })
  # })

  # test_that("using RAW RCC multiplexed with wrong path", {
    targets_tidy <- salmon_tidy
    targets_tidy$IDFILE[1] <- "something_wrong.RCC" # wrong path
    expect_error({
      load_rcc(
        data_directory = "salmon_data",
        ssheet_csv = targets_tidy,
        id_colname = "IDFILE"
      )
    })
  # })

  # test_that("Too high number of components", {
    expect_message({
      load_rcc(
        data_directory = "salmon_data",
        ssheet_csv = salmon_tidy,
        id_colname = "IDFILE",
        n_comp = 1000
      )
    }, "has been set to")
  # })

  # test_that("plexset", {
    expect_s3_class({
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
  # })

  # test_that("heterogenous", {
    expect_error({
        load_rcc(
          data_directory = ".",
          ssheet_csv = plexset_salmon_tidy,
          id_colname = "IDFILE",
          housekeeping_predict = TRUE,
          housekeeping_norm = TRUE
        )
      }
    )
  # })
})
