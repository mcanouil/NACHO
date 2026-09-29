# Writes the NACHO 2 objects that upgrade_nacho() and read_nacho() are tested
# against, with the released NACHO 2.0.7 (git tag v2.0.7):
# - nacho-2-GSE74821-subset.rds, the first six samples of the GSE74821 data
#   shipped with NACHO 2.0.7;
# - nacho-2-plexset.rds, two PlexSet RCC files (16 samples) from
#   tests/testthat/plexset_data, loaded with the NACHO 2.0.7 load_rcc().
# Run from the repository root: Rscript data-raw/nacho-2-fixtures.R

source(file.path("data-raw", "install-nacho.R"))

run <- function(command, args, what) {
  output <- system2(command, args, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    cat(output, sep = "\n")
    stop(what, " failed.", call. = FALSE)
  }
  invisible(output)
}

strip_file_paths <- function(x, data_directory) {
  x[["data_directory"]] <- data_directory
  x[["nacho"]][["file_path"]] <- basename(x[["nacho"]][["file_path"]])
  x
}

write_fixtures <- function(source_ref = "v2.0.7") {
  source_dir <- tempfile("nacho-2-source-")
  run(
    "git",
    c("worktree", "add", "--detach", source_dir, source_ref),
    "Checking out NACHO 2.0.7"
  )
  on.exit(
    run(
      "git",
      c("worktree", "remove", "--force", source_dir),
      "Removing the NACHO 2.0.7 checkout"
    ),
    add = TRUE
  )
  # install_nacho() is defined by the source() call above.
  nacho <- install_nacho(source_dir) # nolint: object_usage_linter.
  on.exit(nacho$cleanup(), add = TRUE)
  stopifnot(utils::packageVersion("NACHO", lib.loc = nacho$lib) == "2.0.7")
  fixtures <- file.path("tests", "testthat", "fixtures")

  gse <- new.env()
  load(file.path(source_dir, "data", "GSE74821.rda"), envir = gse)
  gse <- gse[["GSE74821"]]
  ids <- unique(gse[["nacho"]][["IDFILE"]])[seq_len(6)]
  gse[["nacho"]] <- gse[["nacho"]][gse[["nacho"]][["IDFILE"]] %in% ids, ]
  saveRDS(
    strip_file_paths(gse, "~/"),
    file.path(fixtures, "nacho-2-GSE74821-subset.rds"),
    compress = "xz"
  )

  plexset_dir <- file.path("tests", "testthat", "plexset_data")
  files <- list.files(plexset_dir, pattern = "\\.RCC$")[seq_len(2)]
  sheet <- data.frame(
    IDFILE = rep(files, each = 8),
    plexset_id = rep(paste0("S", seq_len(8)), times = length(files))
  )
  plexset <- suppressMessages(NACHO::load_rcc(
    data_directory = plexset_dir,
    ssheet_csv = sheet,
    id_colname = "IDFILE"
  ))
  saveRDS(
    strip_file_paths(plexset, "plexset_data"),
    file.path(fixtures, "nacho-2-plexset.rds"),
    compress = "xz"
  )
}

write_fixtures()
