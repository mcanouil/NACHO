# Builds the offline fixture in inst/extdata/GSE346526 from GEO.
# GSE346526: NSCLC FFPE samples before and after chemotherapy, 16 samples
# from 8 patients, RCC file version 1.7 written by nSolver 4.0.1.8, the most
# recent version found on GEO in October 2026.
series <- "GSE346526"
patients <- c("C", "E", "F")

download_dir <- file.path(tempdir(), series)
GEOquery::getGEOSuppFiles(GEO = series, baseDir = tempdir())
utils::untar(
  tarfile = file.path(download_dir, paste0(series, "_RAW.tar")),
  exdir = download_dir
)
rcc_files <- sort(list.files(
  download_dir,
  pattern = "\\.RCC(\\.gz)?$",
  ignore.case = TRUE,
  full.names = TRUE
))
stopifnot(length(rcc_files) == 16)

phenotypes <- Biobase::pData(GEOquery::getGEO(GEO = series)[[1]])
gsm <- sub("_.*$", "", basename(rcc_files))
patient <- sub("^Patient ([A-Z])_.*$", "\\1", phenotypes[gsm, "title"])
keep <- rcc_files[patient %in% patients]
stopifnot(length(keep) == 2 * length(patients))

fixture_dir <- file.path("inst", "extdata", series)
unlink(fixture_dir, recursive = TRUE)
dir.create(fixture_dir, recursive = TRUE)
for (file in keep) {
  target <- file.path(fixture_dir, sub("\\.gz$", "", basename(file)))
  lines <- readLines(file, warn = FALSE)
  connection <- gzfile(paste0(target, ".gz"), open = "w")
  writeLines(lines, connection)
  close(connection)
}

gsm <- sub("_.*$", "", basename(keep))
samplesheet <- data.frame(
  IDFILE = paste0(sub("\\.gz$", "", basename(keep)), ".gz"),
  geo_accession = gsm,
  title = phenotypes[gsm, "title"],
  treatment = phenotypes[gsm, "treatment:ch1"]
)
utils::write.csv(
  samplesheet,
  file.path(fixture_dir, "samplesheet.csv"),
  row.names = FALSE
)
cat(
  format(sum(file.size(list.files(fixture_dir, full.names = TRUE))) / 1024),
  "KB\n"
)
