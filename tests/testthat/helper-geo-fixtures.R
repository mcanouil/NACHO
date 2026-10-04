geo_fixture <- function(series) {
  dir <- system.file("extdata", series, package = "NACHO")
  list(
    dir = dir,
    samplesheet = utils::read.csv(file.path(dir, "samplesheet.csv"))
  )
}

first_fixture_file <- function(series) {
  list.files(
    geo_fixture(series)$dir,
    pattern = "\\.RCC\\.gz$",
    full.names = TRUE
  )[1]
}

mirna_fixture <- function(...) {
  fixture <- geo_fixture("GSE270837")
  suppressMessages(NACHO::load_rcc(
    fixture[["dir"]],
    fixture[["samplesheet"]],
    "IDFILE",
    instrument = "sprint",
    n_comp = 3,
    ...
  ))
}

extract_geo_archive_or_skip <- function(series, series_dir) {
  tarfile <- file.path(series_dir, paste0(series, "_RAW.tar"))
  skip_series <- function(reason) {
    testthat::skip(paste0(series, " could not be fetched from GEO: ", reason))
  }
  if (!file.exists(tarfile) || file.size(tarfile) == 0) {
    skip_series("the archive is missing or empty.")
  }
  status <- tryCatch(
    withCallingHandlers(
      utils::untar(tarfile = tarfile, exdir = series_dir),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) skip_series(conditionMessage(e))
  )
  if (!isTRUE(status == 0L)) {
    skip_series(paste0("untar exited with status ", status, "."))
  }
  files <- list.files(
    series_dir,
    pattern = "\\.RCC(\\.gz)?$",
    ignore.case = TRUE
  )
  if (length(files) == 0) {
    skip_series("the archive holds no RCC file.")
  }
  files
}

load_full_series <- function(series, instrument = NULL) {
  download_dir <- withr::local_tempdir(.local_envir = parent.frame())
  series_dir <- file.path(download_dir, series)
  tryCatch(
    suppressMessages(GEOquery::getGEOSuppFiles(
      GEO = series,
      baseDir = download_dir
    )),
    error = function(e) {
      testthat::skip(paste0(
        series,
        " could not be fetched from GEO: ",
        conditionMessage(e)
      ))
    }
  )
  files <- extract_geo_archive_or_skip(series, series_dir)
  suppressMessages(NACHO::load_rcc(
    data_directory = series_dir,
    ssheet_csv = data.frame(IDFILE = files),
    id_colname = "IDFILE",
    instrument = instrument
  ))
}
