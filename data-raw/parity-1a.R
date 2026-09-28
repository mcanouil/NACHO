# Pins the Part 4 QC results so Part 5's parser rewrite cannot change them.
# Sheets mirror tests/testthat/helper-fixtures.R and helper-geo-fixtures.R
# exactly, so the parity test compares like with like.
pkgload::load_all(".", quiet = TRUE)
options(nacho.quiet = TRUE)

load_quiet <- function(...) {
  suppressWarnings(suppressMessages(NACHO::load_rcc(...)))
}

plexset_files <- list.files(
  "tests/testthat/plexset_data",
  full.names = TRUE,
  pattern = "\\.RCC$"
)
plexset_tidy <- data.frame(
  name = basename(plexset_files),
  datapath = plexset_files,
  IDFILE = basename(plexset_files),
  plexset_id = rep(paste0("S", seq_len(8)), each = length(plexset_files))
)

salmon_files <- list.files(
  "tests/testthat/salmon_data",
  full.names = TRUE,
  pattern = "\\.RCC$"
)
salmon_tidy <- data.frame(
  name = basename(salmon_files),
  datapath = salmon_files,
  IDFILE = basename(salmon_files),
  plexset_id = rep(paste0("S", seq_len(8)), each = length(salmon_files))
)

geo_fixture <- function(series) {
  dir <- file.path("inst", "extdata", series)
  list(dir = dir, sheet = utils::read.csv(file.path(dir, "samplesheet.csv")))
}
io360 <- geo_fixture("GSE178516")
mirna <- geo_fixture("GSE270837")

# GSE178516 and GSE270837 have 6 samples each: n_comp = 5 (their PCA ceiling)
# keeps load_rcc() quiet instead of triggering the n_comp_reduced warning that
# the default n_comp = 10 would raise.
objects <- list(
  plexset = load_quiet(
    data_directory = "tests/testthat/plexset_data",
    ssheet_csv = plexset_tidy,
    id_colname = "IDFILE",
    housekeeping_norm = FALSE
  ),
  salmon = load_quiet(
    data_directory = "tests/testthat/salmon_data",
    ssheet_csv = salmon_tidy,
    id_colname = "IDFILE"
  ),
  io360_geo = load_quiet(
    io360[["dir"]],
    io360[["sheet"]],
    "IDFILE",
    n_comp = 5
  ),
  io360_glm = load_quiet(
    io360[["dir"]],
    io360[["sheet"]],
    "IDFILE",
    normalisation_method = "GLM",
    n_comp = 5
  ),
  io360_predict = load_quiet(
    io360[["dir"]],
    io360[["sheet"]],
    "IDFILE",
    housekeeping_predict = TRUE,
    n_comp = 5
  ),
  mirna = load_quiet(
    mirna[["dir"]],
    mirna[["sheet"]],
    "IDFILE",
    n_comp = 5
  )
)

metrics <- c(
  "BD",
  "FoV",
  "PCL",
  "LoD",
  "MC",
  "MedC",
  "Positive_factor",
  "Negative_factor",
  "House_factor",
  "is_outlier"
)
parity <- lapply(objects, function(x) {
  samples <- x@samples
  list(
    counts = x@counts,
    normalised = x@normalised,
    metrics = samples[, c("IDFILE", intersect(metrics, names(samples)))],
    housekeeping_genes = x@settings[["housekeeping_genes"]],
    scores = abs(x@pca[["scores"]]),
    importance = x@pca[["importance"]]
  )
})
saveRDS(parity, "tests/testthat/fixtures/parity-1a.rds", compress = "xz")
