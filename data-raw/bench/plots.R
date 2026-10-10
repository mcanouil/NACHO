# Rscript data-raw/bench/plots.R
pkgload::load_all(".", quiet = TRUE)
source(file.path("data-raw", "bench", "gen.R"))
options(nacho.quiet = TRUE)
root <- file.path(tempdir(), "nacho-bench-plots")
draw <- function(plot) {
  file <- tempfile(fileext = ".png")
  ragg::agg_png(file, width = 1200, height = 700)
  print(plot)
  grDevices::dev.off()
}
for (n in c(48, 192, 768)) {
  dir <- generate_rcc(file.path(root, paste0("n", n)), n)
  x <- suppressWarnings(NACHO::load_rcc(
    dir,
    file.path(dir, "samplesheet.csv"),
    "IDFILE"
  ))
  for (type in c("RLE", "NORM", "PN")) {
    seconds <- system.time(draw(autoplot(x, type = type)))[["elapsed"]]
    cat(sprintf("n = %4d: %-4s %.2f s\n", n, type, seconds))
  }
}
