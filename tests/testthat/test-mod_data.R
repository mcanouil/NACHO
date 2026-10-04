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
