context("plot()")

metrics <- c(
  "BD", "FoV", "PC", "LoD",
  "Positive", "Negative", "Housekeeping",
  "PN", "ACBD", "ACMC",
  "PCA12", "PCAi", "PCA",
  "PFBT", "HF", "NORM"
)
for (imetric in metrics) {
  test_that(paste("Default parameters for", imetric), {
    expect_s3_class(
      object = plot(x = imetric, nacho_object = GSE74821),
      class = "ggplot"
    )
  })
}

for (imetric in metrics) {
  test_that(paste("show_legend to FALSE parameters for", imetric), {
    expect_s3_class(
      object = plot(x = imetric, nacho_object = GSE74821, show_legend = FALSE),
      class = "ggplot"
    )
  })
}
