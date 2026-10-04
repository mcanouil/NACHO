skip_without_zip <- function() {
  testthat::skip_if(!nzchar(Sys.which("zip")), "zip is not available.")
}

expect_nacho <- function(object) {
  testthat::expect_true(S7::S7_inherits(object, NACHO:::nacho))
}

upload_table <- function(data_directory, sample_sheet = NULL, rcc_type = "") {
  rcc_files <- list.files(
    data_directory,
    pattern = "\\.(rcc|rcc\\.gz|zip)$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  upload_directory <- withr::local_tempdir(.local_envir = parent.frame())
  upload_paths <- file.path(upload_directory, seq_along(rcc_files))
  file.copy(rcc_files, upload_paths)
  uploaded <- data.frame(
    name = basename(rcc_files),
    size = file.size(rcc_files),
    type = rcc_type,
    datapath = upload_paths
  )
  if (!is.null(sample_sheet)) {
    sheet_path <- file.path(upload_directory, "sheet")
    utils::write.csv(sample_sheet, sheet_path, row.names = FALSE)
    uploaded <- rbind(
      uploaded,
      data.frame(
        name = "samplesheet.csv",
        size = file.size(sheet_path),
        type = "text/csv",
        datapath = sheet_path
      )
    )
  }
  uploaded
}

read_upload <- function(...) {
  withCallingHandlers(
    NACHO:::read_uploads(upload_table(...)),
    nacho_warning_metric_unavailable = function(cnd) {
      invokeRestart("muffleWarning")
    }
  )
}

test_that("loads uploaded RCC files without a sample sheet", {
  expect_nacho(read_upload("salmon_data"))
})

test_that("treats single-sample files that mention Endogenous8s as single-sample", {
  source_files <- list.files(
    geo_fixture("GSE178516")[["dir"]],
    pattern = "\\.RCC\\.gz$",
    full.names = TRUE
  )
  directory <- withr::local_tempdir()
  for (source_file in source_files) {
    lines <- readLines(source_file)
    endogenous <- grep("^Endogenous,", lines)[1]
    lines[endogenous] <- sub(
      "^Endogenous,[^,]*,",
      "Endogenous,Endogenous8s_like,",
      lines[endogenous]
    )
    writeLines(
      lines,
      file.path(directory, sub("\\.gz$", "", basename(source_file)))
    )
  }
  expect_warning(
    nacho <- read_upload(directory),
    class = "nacho_warning_n_comp_reduced"
  )
  expect_identical(nacho@rcc_type, "n1")
})

test_that("loads uploaded PlexSet RCC files", {
  expect_warning(
    nacho <- read_upload("plexset_data"),
    class = "nacho_warning_no_housekeeping"
  )
  expect_nacho(nacho)
  expect_false(nacho@settings[["housekeeping_norm"]])
  expect_type(nacho_samples(nacho)[["plexset_id"]], "character")
})

test_that("merges an uploaded sample sheet", {
  sample_sheet <- expand.grid(
    IDFILE = basename(list.files("salmon_data", pattern = "\\.RCC$")),
    plexset_id = paste0("S", seq_len(8)),
    stringsAsFactors = FALSE
  )
  sample_sheet[["group"]] <- "case"
  nacho <- read_upload("salmon_data", sample_sheet)
  expect_true("group" %in% names(nacho_samples(nacho)))
})

test_that("discards a sample sheet without IDFILE", {
  sample_sheet <- data.frame(file = "salmon_01_01.RCC", group = "case")
  expect_warning(
    nacho <- read_upload("salmon_data", sample_sheet),
    class = "nacho_warning_sample_sheet_discarded"
  )
  expect_nacho(nacho)
  expect_false("group" %in% names(nacho_samples(nacho)))
})

test_that("merges a sample sheet by IDFILE for single-sample RCC files", {
  single_directory <- geo_fixture("GSE178516")[["dir"]]
  rcc_files <- list.files(single_directory, pattern = "\\.RCC\\.gz$")

  expect_warning(
    merged <- read_upload(
      single_directory,
      data.frame(IDFILE = rcc_files, group = "case")
    ),
    class = "nacho_warning_n_comp_reduced"
  )
  expect_true("group" %in% names(nacho_samples(merged)))

  expect_warning(
    expect_warning(
      discarded <- read_upload(
        single_directory,
        data.frame(file = rcc_files, group = "case")
      ),
      class = "nacho_warning_n_comp_reduced"
    ),
    class = "nacho_warning_sample_sheet_discarded"
  )
  expect_nacho(discarded)
  expect_false("group" %in% names(nacho_samples(discarded)))
})

test_that("discards a PlexSet sample sheet without plexset_id", {
  sample_sheet <- data.frame(
    IDFILE = basename(list.files("salmon_data", pattern = "\\.RCC$")),
    group = "case"
  )
  expect_warning(
    nacho <- read_upload("salmon_data", sample_sheet),
    class = "nacho_warning_sample_sheet_discarded"
  )
  expect_nacho(nacho)
  expect_false("group" %in% names(nacho_samples(nacho)))
})

test_that("loads a zip archive whatever MIME type the browser sends", {
  skip_if(!nzchar(Sys.which("zip")), "zip is not available.")
  archive_directory <- withr::local_tempdir()
  withr::with_dir(
    test_path("salmon_data"),
    utils::zip(
      file.path(archive_directory, "salmon.zip"),
      list.files(pattern = "\\.RCC$"),
      flags = "-q"
    )
  )
  for (mime_type in c("application/zip", "application/x-zip-compressed")) {
    expect_nacho(read_upload(archive_directory, rcc_type = mime_type))
  }
})

test_that("matches file extensions regardless of case", {
  lower_directory <- withr::local_tempdir()
  rcc_files <- list.files("salmon_data", pattern = "\\.RCC$")
  file.copy(
    file.path("salmon_data", rcc_files),
    file.path(lower_directory, sub("\\.RCC$", ".rcc", rcc_files))
  )
  sample_sheet <- expand.grid(
    IDFILE = sub("\\.RCC$", ".rcc", rcc_files),
    plexset_id = paste0("S", seq_len(8)),
    stringsAsFactors = FALSE
  )
  sample_sheet[["group"]] <- "case"
  nacho <- read_upload(lower_directory, sample_sheet)
  expect_true("group" %in% names(nacho_samples(nacho)))
})

test_that("keeps gzipped RCC files next to a sample sheet", {
  gz_directory <- withr::local_tempdir()
  for (rcc in list.files("salmon_data", pattern = "\\.RCC$")) {
    connection <- gzfile(file.path(gz_directory, paste0(rcc, ".gz")), "w")
    writeLines(readLines(file.path("salmon_data", rcc)), connection)
    close(connection)
  }
  sample_sheet <- expand.grid(
    IDFILE = list.files(gz_directory),
    plexset_id = paste0("S", seq_len(8)),
    stringsAsFactors = FALSE
  )
  sample_sheet[["group"]] <- "case"
  nacho <- read_upload(gz_directory, sample_sheet)
  expect_true("group" %in% names(nacho_samples(nacho)))
})

test_that("an upload without RCC files is refused", {
  directory <- withr::local_tempdir()
  path <- file.path(directory, "notes.csv")
  writeLines("IDFILE\nx.RCC", path)
  files <- data.frame(
    name = "notes.csv",
    size = file.size(path),
    type = "text/csv",
    datapath = path
  )
  expect_error(
    NACHO:::read_uploads(files),
    class = "nacho_error_bad_upload"
  )
})

test_that("a discarded sample sheet is announced", {
  sample_sheet <- data.frame(file = "salmon_01_01.RCC", group = "case")
  expect_warning(
    nacho <- NACHO:::read_uploads(upload_table("salmon_data", sample_sheet)),
    "IDFILE",
    class = "nacho_warning_sample_sheet_discarded"
  )
  expect_false("group" %in% names(nacho_samples(nacho)))
})

test_that("a rejected upload keeps the previous data", {
  messages <- character()
  local_mocked_bindings(
    notify_user = function(message, type) {
      messages <<- c(messages, type)
    }
  )
  directory <- withr::local_tempdir()
  path <- file.path(directory, "notes.csv")
  writeLines("IDFILE\nx.RCC", path)
  bad <- data.frame(
    name = "notes.csv",
    size = file.size(path),
    type = "text/csv",
    datapath = path
  )
  shiny::testServer(
    NACHO:::mod_data_server,
    args = list(initial = GSE74821),
    {
      session$setInputs(files = bad, import = 1)
      expect_identical(session$returned(), GSE74821)
    }
  )
  expect_identical(messages, "error")
})

test_that("the module loads an upload", {
  shiny::testServer(NACHO:::mod_data_server, {
    expect_null(session$returned())
    session$setInputs(files = upload_table("salmon_data"), import = 1)
    expect_true(S7::S7_inherits(session$returned(), NACHO:::nacho))
  })
})

test_that("a corrupt zip shows an error and keeps the previous data", {
  types <- character()
  local_mocked_bindings(
    notify_user = function(message, type) types <<- c(types, type)
  )
  directory <- withr::local_tempdir()
  path <- file.path(directory, "0")
  writeLines("this is not a zip", path)
  bad <- data.frame(
    name = "broken.zip",
    size = file.size(path),
    type = "application/zip",
    datapath = path
  )
  shiny::testServer(
    NACHO:::mod_data_server,
    args = list(initial = GSE74821),
    {
      suppressWarnings(session$setInputs(files = bad, import = 1))
      expect_identical(session$returned(), GSE74821)
    }
  )
  expect_identical(types, "error")
})

test_that("a zip made from a folder is read", {
  skip_without_zip()
  source_files <- list.files(
    test_path("salmon_data"),
    pattern = "\\.rcc$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  skip_if(length(source_files) == 0)
  staging <- withr::local_tempdir()
  dir.create(file.path(staging, "run1"))
  file.copy(source_files, file.path(staging, "run1"))
  archive <- file.path(withr::local_tempdir(), "0")
  withr::with_dir(
    staging,
    utils::zip(archive, "run1", flags = "-rq")
  )
  archive <- paste0(archive, ".zip")
  zipped <- data.frame(
    name = "run1.zip",
    size = file.size(archive),
    type = "application/zip",
    datapath = archive
  )
  expect_nacho(
    withCallingHandlers(
      NACHO:::read_uploads(zipped),
      nacho_warning_metric_unavailable = function(cnd) {
        invokeRestart("muffleWarning")
      }
    )
  )
})

test_that("extra sample sheets are announced", {
  sample_sheet <- data.frame(
    IDFILE = "salmon_01_01.RCC",
    plexset_id = "S1",
    group = "case"
  )
  uploads <- upload_table("salmon_data", sample_sheet)
  second <- uploads[uploads$name == "samplesheet.csv", ]
  second$name <- "second.csv"
  second$datapath <- file.path(dirname(second$datapath), "second_sheet")
  file.copy(
    uploads$datapath[uploads$name == "samplesheet.csv"],
    second$datapath
  )
  expect_warning(
    withCallingHandlers(
      NACHO:::read_uploads(rbind(uploads, second)),
      nacho_warning_metric_unavailable = function(cnd) {
        invokeRestart("muffleWarning")
      },
      nacho_warning_n_comp_reduced = function(cnd) {
        invokeRestart("muffleWarning")
      }
    ),
    "second.csv",
    class = "nacho_warning_sample_sheet_discarded"
  )
})

zip_upload <- function(files, folder = NULL, name = "run.zip") {
  skip_without_zip()
  staging <- withr::local_tempdir(.local_envir = parent.frame())
  target <- if (is.null(folder)) staging else file.path(staging, folder)
  dir.create(target, showWarnings = FALSE)
  file.copy(files, target)
  archive <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "0")
  withr::with_dir(
    staging,
    utils::zip(archive, list.files(staging), flags = "-rq")
  )
  data.frame(
    name = name,
    size = file.size(paste0(archive, ".zip")),
    type = "application/zip",
    datapath = paste0(archive, ".zip")
  )
}

test_that("an empty zip is refused as an upload without RCC files", {
  skip_without_zip()
  staging <- withr::local_tempdir()
  dir.create(file.path(staging, "empty"))
  archive <- file.path(withr::local_tempdir(), "0")
  withr::with_dir(staging, utils::zip(archive, "empty", flags = "-rq"))
  empty <- data.frame(
    name = "empty.zip",
    size = 1,
    type = "application/zip",
    datapath = paste0(archive, ".zip")
  )
  expect_error(
    NACHO:::read_uploads(empty),
    "holds no RCC file",
    class = "nacho_error_bad_upload"
  )
})

test_that("a corrupt zip names the problem", {
  messages <- character()
  local_mocked_bindings(
    notify_user = function(message, type) messages <<- c(messages, message)
  )
  path <- file.path(withr::local_tempdir(), "0")
  writeLines("this is not a zip", path)
  bad <- data.frame(
    name = "broken.zip",
    size = file.size(path),
    type = "application/zip",
    datapath = path
  )
  shiny::testServer(NACHO:::mod_data_server, {
    suppressWarnings(session$setInputs(files = bad, import = 1))
  })
  expect_match(messages, "not a valid zip archive")
})

test_that("a sample sheet with bare file names matches files in a zip", {
  rcc <- list.files(
    test_path("salmon_data"),
    pattern = "\\.rcc$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  sheet <- data.frame(
    IDFILE = rep(basename(rcc), each = 8),
    plexset_id = paste0("S", 1:8),
    group = "case"
  )
  sheet_path <- file.path(withr::local_tempdir(), "sheet")
  utils::write.csv(sheet, sheet_path, row.names = FALSE)
  sheet_row <- data.frame(
    name = "sheet.csv",
    size = file.size(sheet_path),
    type = "text/csv",
    datapath = sheet_path
  )
  for (folder in list(NULL, "run1")) {
    fresh <- tempfile()
    file.copy(sheet_path, fresh)
    sheet_row$datapath <- fresh
    nacho <- withCallingHandlers(
      NACHO:::read_uploads(rbind(zip_upload(rcc, folder), sheet_row)),
      nacho_warning_metric_unavailable = function(cnd) {
        invokeRestart("muffleWarning")
      }
    )
    expect_true("group" %in% names(nacho_samples(nacho)))
  }
})

test_that("a sample sheet that matches no file is announced", {
  rcc <- list.files(
    test_path("salmon_data"),
    pattern = "\\.rcc$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  sheet <- data.frame(IDFILE = "other.RCC", plexset_id = "S1", group = "case")
  sheet_path <- file.path(withr::local_tempdir(), "sheet")
  utils::write.csv(sheet, sheet_path, row.names = FALSE)
  sheet_row <- data.frame(
    name = "sheet.csv",
    size = file.size(sheet_path),
    type = "text/csv",
    datapath = sheet_path
  )
  expect_warning(
    withCallingHandlers(
      NACHO:::read_uploads(rbind(zip_upload(rcc), sheet_row)),
      nacho_warning_metric_unavailable = function(cnd) {
        invokeRestart("muffleWarning")
      }
    ),
    "matches no",
    class = "nacho_warning_sample_sheet_discarded"
  )
})

test_that("only sample information reaches the samples table", {
  nacho <- read_upload("salmon_data")
  expect_false(any(
    c("datapath", "type", "name") %in% names(nacho_samples(nacho))
  ))
  sample_sheet <- data.frame(
    IDFILE = rep(list.files("salmon_data", pattern = "\\.RCC$"), each = 8),
    plexset_id = paste0("S", 1:8),
    group = "case"
  )
  with_sheet <- read_upload("salmon_data", sample_sheet)
  expect_true("group" %in% names(nacho_samples(with_sheet)))
  expect_false(any(
    c("datapath", "type", "name") %in% names(nacho_samples(with_sheet))
  ))
})

test_that("the example button loads GSE74821", {
  shiny::testServer(NACHO:::mod_data_server, {
    session$setInputs(example = 1)
    expect_identical(session$returned(), GSE74821)
  })
})

test_that("messages reach the user as toasts", {
  shown <- NULL
  local_mocked_bindings(
    show_toast = function(toast, ...) shown <<- toast,
    .package = "bslib"
  )
  NACHO:::notify_user("Hello", "warning")
  expect_s3_class(shown, "bslib_toast")
})

test_that("an error toast stays open until it is closed", {
  shown <- list()
  local_mocked_bindings(
    show_toast = function(toast, ...) shown[[length(shown) + 1]] <<- toast,
    .package = "bslib"
  )
  NACHO:::notify_user("Broken", "error")
  NACHO:::notify_user("Fine", "message")
  expect_false(unclass(shown[[1]])$autohide)
  expect_true(unclass(shown[[2]])$autohide)
})

test_that("a discarded sample sheet and other load warnings become warning toasts", {
  types <- character()
  local_mocked_bindings(
    notify_user = function(message, type) types <<- c(types, type)
  )
  sheet <- data.frame(file = "salmon_01_01.RCC", group = "case")
  shiny::testServer(NACHO:::mod_data_server, {
    session$setInputs(files = upload_table("salmon_data", sheet), import = 1)
    expect_true(S7::S7_inherits(session$returned(), NACHO:::nacho))
  })
  expect_true("warning" %in% types)
  expect_false("error" %in% types)
})

test_that("any load warning of the package is shown as a warning toast", {
  messages <- character()
  local_mocked_bindings(
    notify_user = function(message, type) messages <<- c(messages, message),
    read_uploads = function(files) {
      NACHO:::nacho_warn("Too few genes.", class = "n_comp_reduced")
      GSE74821
    }
  )
  shiny::testServer(NACHO:::mod_data_server, {
    session$setInputs(
      files = data.frame(name = "a", size = 1, type = "x", datapath = "a"),
      import = 1
    )
    expect_identical(session$returned(), GSE74821)
  })
  expect_identical(messages, "Too few genes.")
})
