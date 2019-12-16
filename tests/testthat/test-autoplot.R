test_that('Missing "x"', {
  expect_error(autoplot(object = GSE74821))
})

test_that('Wrong "x"', {
  expect_error(autoplot(object = GSE74821, x = "FAKE_VALUE"))
})

metrics <- c(
  "BD", "FoV", "PCL", "LoD",
  "Positive", "Negative", "Housekeeping",
  "PN", "ACBD", "ACMC",
  "PCA12", "PCAi", "PCA",
  "PFNF", "HF", "NORM"
)

for (imetric in metrics) {
  test_that(paste(imetric, "Default parameters", sep = " - "), {
    expect_s3_class(
      object = autoplot(object = GSE74821, x = imetric),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "show_legend to FALSE parameters", sep = " - "), {
    expect_s3_class(
      object = autoplot(object = GSE74821, x = imetric, show_legend = FALSE),
      class = "ggplot"
    )
  })
}
