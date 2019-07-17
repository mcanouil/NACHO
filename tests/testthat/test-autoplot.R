metrics <- c(
  "BD", "FoV", "PC", "LoD",
  "Positive", "Negative", "Housekeeping",
  "PN", "ACBD", "ACMC",
  "PCA12", "PCAi", "PCA",
  "PFNF", "HF", "NORM"
)

for (imetric in metrics) {
  test_that(paste(imetric, "Default parameters", sep = " - "), {
    expect_s3_class(
      object = autoplot(object = GSE74821, x = "BD"),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "show_legend to FALSE parameters", sep = " - "), {
    expect_s3_class(
      object = autoplot(object = GSE74821, x = "BD", show_legend = FALSE),
      class = "ggplot"
    )
  })
}
