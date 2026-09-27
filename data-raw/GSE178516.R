# Builds the offline fixture in inst/extdata/GSE178516 from GEO.
# GSE178516: PanCancer IO 360 panel on MAX/FLEX, 30 samples in 3 groups of 10
# over 5 cartridges (PMID 34863788).
series <- "GSE178516"
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
stopifnot(length(rcc_files) == 30)

phenotypes <- Biobase::pData(GEOquery::getGEO(GEO = series)[[1]])
# source_name_ch1 only has two levels (colorectal cancer tissue / colorectal
# mucosa tissue), not the three groups described above; the three groups
# (carrier mucosa, patient mucosa, carcinoma) live in the "group:ch1" column.
group <- phenotypes[sub("_.*$", "", basename(rcc_files)), "group:ch1"]
keep <- unlist(
  lapply(split(rcc_files, group), utils::head, 2),
  use.names = FALSE
)

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
