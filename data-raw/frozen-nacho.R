# Writes the frozen schema-1 object that later NACHO versions must still
# read.
# Run once; never regenerate it, because its value is that it stays old.

# Build from an installed NACHO, not pkgload::load_all(), so the saved
# object carries no source references to this working tree or a temp
# install library.
source(file.path("data-raw", "install-nacho.R"))

build_frozen_nacho <- function() {
  # install_nacho() is defined by the source() call above.
  nacho <- install_nacho(".") # nolint: object_usage_linter.
  on.exit(nacho$cleanup(), add = TRUE)

  options(nacho.quiet = TRUE)
  dir <- file.path("inst", "extdata", "GSE178516")
  x <- NACHO::load_rcc(
    dir,
    utils::read.csv(file.path(dir, "samplesheet.csv")),
    "IDFILE"
  )
  x <- suppressWarnings(x[seq_len(40), 1:4])
  x@provenance[["data_directory"]] <- NULL
  saveRDS(
    x,
    file.path("tests", "testthat", "fixtures", "nacho-schema-1.rds"),
    compress = "xz"
  )
}
build_frozen_nacho()
