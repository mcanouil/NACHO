test_that("each plot type keeps its look in light mode", {
  skip_if_not_installed("vdiffr")
  x <- flagged_gse()
  for (type in names(NACHO:::nacho_plot_registry)) {
    plot <- suppressWarnings(autoplot(x, type = type))
    withr::with_seed(1, vdiffr::expect_doppelganger(paste("light", type), plot))
  }
})

test_that("plots keep their look in dark mode", {
  skip_if_not_installed("vdiffr")
  x <- flagged_gse()
  for (type in c("BD", "PCA12", "NORM", "PCBatch")) {
    plot <- suppressWarnings(autoplot(x, type = type, dark = TRUE))
    withr::with_seed(1, vdiffr::expect_doppelganger(paste("dark", type), plot))
  }
})
