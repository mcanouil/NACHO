# Writes the frozen object of one schema that later NACHO versions must
# still read.
# Each schema's object is written once, when that schema is final, and never
# regenerated, because its value is that it stays old.
# Run from the repository root: Rscript data-raw/frozen-nacho.R

# Build from an installed NACHO, not pkgload::load_all(), so the saved
# object carries no source references to this working tree or a temp
# install library.
source(file.path("data-raw", "install-nacho.R"))

build_frozen_nacho <- function(schema) {
  path <- file.path(
    "tests",
    "testthat",
    "fixtures",
    sprintf("nacho-schema-%d.rds", schema)
  )
  if (file.exists(path)) {
    stop(
      "'",
      path,
      "' already exists; a frozen object is never regenerated.",
      call. = FALSE
    )
  }

  # install_nacho() is defined by the source() call above.
  nacho <- install_nacho(".") # nolint: object_usage_linter.
  on.exit(nacho$cleanup(), add = TRUE)

  installed_schema <- get("nacho_schema_version", envir = asNamespace("NACHO"))
  if (!identical(as.integer(schema), as.integer(installed_schema))) {
    stop(
      "The installed NACHO writes schema ",
      installed_schema,
      ", not schema ",
      schema,
      ".",
      call. = FALSE
    )
  }

  old_options <- options(nacho.quiet = TRUE)
  on.exit(options(old_options), add = TRUE)
  dir <- file.path("inst", "extdata", "GSE178516")
  x <- NACHO::load_rcc(
    dir,
    utils::read.csv(file.path(dir, "samplesheet.csv")),
    "IDFILE"
  )
  x <- suppressWarnings(x[seq_len(40), 1:4])
  x@provenance[["data_directory"]] <- NULL
  saveRDS(x, path, compress = "xz")
}
build_frozen_nacho(schema = 2L)
