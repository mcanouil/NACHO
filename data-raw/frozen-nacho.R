# Writes the frozen schema-1 object that later NACHO versions must still
# read.
# Run once; never regenerate it, because its value is that it stays old.

# Build from an installed NACHO, not pkgload::load_all(), so the saved
# object carries no source references to this working tree or a temp
# install library.
Sys.setenv(R_KEEP_PKG_SOURCE = "no")
lib <- tempfile("nacho-install-")
dir.create(lib)
install_log <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-docs", "--no-help", paste0("--library=", lib), "."),
  stdout = TRUE,
  stderr = TRUE
)
if (!is.null(attr(install_log, "status")) && attr(install_log, "status") != 0) {
  cat(install_log, sep = "\n")
  stop("R CMD INSTALL failed while building nacho-schema-1.rds.")
}
library(NACHO, lib.loc = lib)

options(nacho.quiet = TRUE)
dir <- file.path("inst", "extdata", "GSE178516")
x <- load_rcc(dir, utils::read.csv(file.path(dir, "samplesheet.csv")), "IDFILE")
x <- suppressWarnings(x[seq_len(40), 1:4])
x@provenance[["data_directory"]] <- NULL
saveRDS(
  x,
  file.path("tests", "testthat", "fixtures", "nacho-schema-1.rds"),
  compress = "xz"
)
