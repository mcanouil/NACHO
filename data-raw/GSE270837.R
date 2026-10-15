# Builds the offline fixture in inst/extdata/GSE270837 from GEO.
# GSE270837: miRNA v3b panel on SPRINT, RCC FileVersion 2.0, 22 samples,
# Hofmann et al. 2024, Extracellular Vesicle 4.
series <- "GSE270837"
n_keep <- 6

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
stopifnot(length(rcc_files) == 22)

phenotypes <- Biobase::pData(GEOquery::getGEO(GEO = series)[[1]])
keep <- rcc_files[round(seq(1, length(rcc_files), length.out = n_keep))]

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
  title = phenotypes[gsm, "title"]
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
