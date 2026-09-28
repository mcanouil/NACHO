test_that("missing directory", {
  expect_error(
    load_rcc(
      ssheet_csv = salmon_tidy,
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    ),
    class = "nacho_error_bad_argument"
  )
})

test_that("missing sample sheet", {
  expect_error(
    load_rcc(
      data_directory = "salmon_data",
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    ),
    class = "nacho_error_bad_argument"
  )
})

test_that("missing id_colname", {
  expect_error(
    load_rcc(
      data_directory = "salmon_data",
      ssheet_csv = salmon_tidy,
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    ),
    class = "nacho_error_bad_argument"
  )
})

test_that("load_rcc() checks its arguments before reading any file", {
  expect_error(
    load_rcc(
      test_path("plexset_data"),
      plexset_tidy,
      "IDFILE",
      normalisation_method = "geo"
    ),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    load_rcc(test_path("plexset_data"), plexset_tidy, "IDFILE", n_comp = 0),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    load_rcc(
      test_path("plexset_data"),
      plexset_tidy,
      "IDFILE",
      housekeeping_norm = NA
    ),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    load_rcc(test_path("plexset_data"), plexset_tidy, "NOT_A_COLUMN"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    load_rcc(file.path(tempdir(), "no-such-directory"), plexset_tidy, "IDFILE"),
    class = "nacho_error_missing_file"
  )
})

test_that("load_rcc() names the RCC files it cannot find", {
  sheet <- plexset_tidy
  sheet$IDFILE[1:8] <- "missing.RCC"
  expect_error(
    load_rcc(test_path("plexset_data"), sheet, "IDFILE"),
    class = "nacho_error_missing_file"
  )
  expect_snapshot(
    load_rcc(test_path("plexset_data"), sheet, "IDFILE"),
    error = TRUE,
    transform = function(x) {
      gsub(
        normalizePath(test_path("plexset_data")),
        "<data_directory>",
        x,
        fixed = TRUE
      )
    }
  )
})

test_that("load_rcc() reports its stages unless nacho.quiet is set", {
  withr::local_options(nacho.quiet = NULL, rlib_message_verbosity = NULL)
  messages <- capture_messages(load_rcc(
    test_path("plexset_data"),
    plexset_tidy,
    "IDFILE",
    housekeeping_norm = FALSE
  ))
  expect_match(messages, "Reading 12 RCC files", all = FALSE)
  withr::local_options(nacho.quiet = TRUE)
  expect_no_message(load_rcc(
    test_path("plexset_data"),
    plexset_tidy,
    "IDFILE",
    housekeeping_norm = FALSE
  ))
})

test_that("load_rcc() warns when it turns housekeeping normalisation off", {
  expect_warning(
    load_rcc(
      test_path("plexset_data"),
      plexset_tidy,
      "IDFILE",
      housekeeping_genes = NULL
    ),
    class = "nacho_warning_no_housekeeping"
  ) |>
    suppressMessages()
})

test_that("no housekeeping norm", {
  expect_true(S7::S7_inherits(
    load_rcc(
      data_directory = "salmon_data",
      ssheet_csv = salmon_tidy,
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = FALSE,
      normalisation_method = "GLM",
      n_comp = 10
    ),
    NACHO:::nacho
  ))
})

test_that("no housekeeping norm and prediction", {
  expect_true(S7::S7_inherits(
    load_rcc(
      data_directory = "salmon_data",
      ssheet_csv = salmon_tidy,
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = TRUE,
      housekeeping_norm = FALSE,
      normalisation_method = "GLM",
      n_comp = 10
    ),
    NACHO:::nacho
  ))
})

test_that("using GEO GSE74821", {
  skip_on_cran()
  skip_if_offline()
  suppressMessages(skip_if_not_installed("GEOquery"))
  skip_if_not_installed("Biobase")
  gse <- try(
    suppressMessages(GEOquery::getGEO(GEO = "GSE74821")),
    silent = TRUE
  )
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
  expect_true(S7::S7_inherits(
    load_rcc(
      data_directory = file.path(tempdir(), "GSE74821"),
      ssheet_csv = head(targets, 20),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    ),
    NACHO:::nacho
  ))

  # using GEO GSE74821 with prediction
  expect_true(S7::S7_inherits(
    load_rcc(
      data_directory = file.path(tempdir(), "GSE74821"),
      ssheet_csv = head(targets, 20),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = TRUE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    ),
    NACHO:::nacho
  ))
})

test_that("using GEO GSE70970", {
  skip_on_cran()
  skip_if_offline()
  suppressMessages(skip_if_not_installed("GEOquery"))
  skip_if_not_installed("Biobase")
  gse <- try(
    suppressMessages(GEOquery::getGEO(GEO = "GSE70970")),
    silent = TRUE
  )
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
  expect_true(S7::S7_inherits(
    load_rcc(
      data_directory = file.path(tempdir(), "GSE70970"),
      ssheet_csv = head(targets, 20),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    ),
    NACHO:::nacho
  ))

  # using GEO GSE70970 with prediction
  expect_true(S7::S7_inherits(
    load_rcc(
      data_directory = file.path(tempdir(), "GSE70970"),
      ssheet_csv = head(targets, 20),
      id_colname = "IDFILE",
      housekeeping_genes = NULL,
      housekeeping_predict = TRUE,
      housekeeping_norm = TRUE,
      normalisation_method = "GLM",
      n_comp = 10
    ),
    NACHO:::nacho
  ))

  # ssheet_csv as vector
  expect_true(S7::S7_inherits(
    load_rcc(
      data_directory = file.path(tempdir(), "GSE70970"),
      ssheet_csv = head(targets[["IDFILE"]], 20),
      id_colname = "IDFILE",
      housekeeping_predict = TRUE,
      housekeeping_norm = TRUE
    ),
    NACHO:::nacho
  ))

  # ssheet_csv as vector without id_colname
  expect_true(S7::S7_inherits(
    load_rcc(
      data_directory = file.path(tempdir(), "GSE70970"),
      ssheet_csv = head(targets[["IDFILE"]], 20),
      housekeeping_predict = TRUE,
      housekeeping_norm = TRUE
    ),
    NACHO:::nacho
  ))

  # ssheet_csv as a named vector
  expect_true(S7::S7_inherits(
    load_rcc(
      data_directory = file.path(tempdir(), "GSE70970"),
      ssheet_csv = `names<-`(
        head(targets[["IDFILE"]], 20),
        head(letters, 20)
      ),
      housekeeping_predict = TRUE,
      housekeeping_norm = TRUE
    ),
    NACHO:::nacho
  ))

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
  expect_true(S7::S7_inherits(
    load_rcc(
      data_directory = "salmon_data",
      ssheet_csv = salmon_tidy,
      id_colname = "IDFILE"
    ),
    NACHO:::nacho
  ))
})

test_that("using RAW RCC multiplexed without plexset_id", {
  targets_tidy <- salmon_tidy
  targets_tidy$plexset_id <- NULL

  expect_error(
    {
      load_rcc(
        data_directory = "salmon_data",
        ssheet_csv = targets_tidy,
        id_colname = "IDFILE"
      )
    },
    class = "nacho_error_duplicate_id"
  )
})

test_that("using RAW RCC multiplexed with wrong path", {
  targets_tidy <- salmon_tidy
  targets_tidy$IDFILE[1] <- "something_wrong.RCC" # wrong path
  expect_error(
    {
      load_rcc(
        data_directory = "salmon_data",
        ssheet_csv = targets_tidy,
        id_colname = "IDFILE"
      )
    },
    class = "nacho_error_missing_file"
  )
})

test_that("Too high number of components", {
  expect_warning(
    {
      load_rcc(
        data_directory = "salmon_data",
        ssheet_csv = salmon_tidy,
        id_colname = "IDFILE",
        n_comp = 1000
      )
    },
    class = "nacho_warning_n_comp_reduced"
  )
})

test_that("plexset", {
  expect_true(S7::S7_inherits(
    load_rcc(
      data_directory = "plexset_data",
      ssheet_csv = plexset_tidy,
      id_colname = "IDFILE",
      housekeeping_predict = TRUE,
      housekeeping_norm = TRUE
    ),
    NACHO:::nacho
  ))
})

test_that("heterogenous", {
  sheet <- transform(plexset_salmon_tidy, IDFILE = name)
  expect_error(
    {
      load_rcc(
        data_directory = test_path(),
        ssheet_csv = sheet,
        id_colname = "IDFILE",
        housekeeping_predict = TRUE,
        housekeeping_norm = TRUE
      )
    },
    class = "nacho_error_mixed_versions"
  )
  expect_snapshot(
    suppressMessages(load_rcc(
      data_directory = test_path(),
      ssheet_csv = sheet,
      id_colname = "IDFILE",
      housekeeping_predict = TRUE,
      housekeeping_norm = TRUE
    )),
    error = TRUE,
    transform = function(x) {
      gsub(normalizePath(test_path()), "<data_directory>", x, fixed = TRUE)
    }
  )
})

test_that("PlexSet files are detected from their content", {
  unique_ids <- data.frame(IDFILE = basename(salmon_files))
  res <- suppressMessages(load_rcc(
    data_directory = test_path("salmon_data"),
    ssheet_csv = unique_ids,
    id_colname = "IDFILE"
  ))
  expect_identical(res@rcc_type, "n8")
  expect_setequal(
    nacho_samples(res)[["IDFILE"]],
    nacho_samples(salmon_nacho)[["IDFILE"]]
  )
  expect_false(all(nacho_qc(res)[["is_outlier"]]))
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

test_that("load_rcc() refuses a mix of PlexSet and single-sample RCC files offline", {
  directory <- withr::local_tempdir()
  plexset <- list.files(
    test_path("plexset_data"),
    pattern = "\\.RCC$",
    full.names = TRUE
  )[1]
  single <- list.files(
    geo_fixture("GSE178516")[["dir"]],
    pattern = "\\.RCC\\.gz$",
    full.names = TRUE
  )[1]
  file.copy(c(plexset, single), directory)
  expect_error(
    suppressMessages(load_rcc(
      data_directory = directory,
      ssheet_csv = data.frame(IDFILE = basename(c(plexset, single))),
      id_colname = "IDFILE"
    )),
    class = "nacho_error_mixed_rcc_types"
  )
})

test_that("load_rcc() names plexset_id values outside S1 to S8", {
  sheet <- plexset_tidy
  sheet$plexset_id[1] <- "S9"
  sheet$plexset_id[2] <- "bad"
  expect_error(
    load_rcc(
      data_directory = test_path("plexset_data"),
      ssheet_csv = sheet,
      id_colname = "IDFILE"
    ),
    class = "nacho_error_bad_argument",
    regexp = "S9"
  )
})

test_that("load_rcc() rejects duplicated id/plexset_id pairs", {
  sheet <- plexset_tidy
  sheet$IDFILE[2] <- sheet$IDFILE[1]
  expect_error(
    load_rcc(
      data_directory = test_path("plexset_data"),
      ssheet_csv = sheet,
      id_colname = "IDFILE"
    ),
    class = "nacho_error_duplicate_id"
  )
})

test_that("load_rcc() reports a probe clash across single-sample RCC files", {
  source_files <- list.files(
    geo_fixture("GSE178516")[["dir"]],
    pattern = "\\.RCC\\.gz$",
    full.names = TRUE
  )[1:2]
  directory <- withr::local_tempdir()
  file.copy(source_files, directory)
  copied <- list.files(directory, pattern = "\\.RCC\\.gz$", full.names = TRUE)
  lines <- readLines(gzfile(copied[1]))
  lines <- sub("NM_021147.4", "NM_021147.5", lines, fixed = TRUE)
  con <- gzfile(copied[1], "w")
  writeLines(lines, con)
  close(con)
  expect_error(
    suppressMessages(load_rcc(
      data_directory = directory,
      ssheet_csv = data.frame(IDFILE = basename(copied)),
      id_colname = "IDFILE"
    )),
    class = "nacho_error_rcc_parse"
  )
})
