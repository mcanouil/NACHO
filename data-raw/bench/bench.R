# Rscript data-raw/bench/bench.R
pkgload::load_all(".", quiet = TRUE)
source(file.path("data-raw", "bench", "gen.R"))
options(nacho.quiet = TRUE)
root <- file.path(tempdir(), "nacho-bench")
for (n in c(48, 192, 768)) {
  dir <- generate_rcc(file.path(root, paste0("n", n)), n)
  timing <- system.time(
    x <- suppressWarnings(NACHO::load_rcc(
      dir,
      file.path(dir, "samplesheet.csv"),
      "IDFILE"
    ))
  )[["elapsed"]]
  size <- as.numeric(utils::object.size(x)) / 1024^2
  cat(sprintf("n = %4d: load_rcc %.2f s, object %.1f MB\n", n, timing, size))
  if (n == 768) {
    cat(sprintf(
      "targets at 768 samples: load_rcc under 3 s %s, object under 10 MB %s\n",
      if (timing < 3) "PASS" else "FAIL",
      if (size < 10) "PASS" else "FAIL"
    ))
  }
}
