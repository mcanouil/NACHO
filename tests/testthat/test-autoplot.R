context("autoplot()")

metrics <- c(
  "BD", "FoV", "PC", "LoD",
  "Positive", "Negative", "Housekeeping",
  "PN", "ACBD", "ACMC",
  "PCA12", "PCAi", "PCA",
  "PFNF", "HF", "NORM"
)
for (imetric in metrics) {
  test_that(paste("Default parameters for", imetric), {
    expect_s3_class(
      object = autoplot.nacho(object = GSE74821, x = imetric),
      class = "ggplot"
    )
  })
}

for (imetric in metrics) {
  test_that(paste("show_legend to FALSE parameters for", imetric), {
    expect_s3_class(
      object = autoplot.nacho(object = GSE74821, x = imetric, show_legend = FALSE),
      class = "ggplot"
    )
  })
}
