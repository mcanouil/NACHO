test_that('Missing "object"', {
  expect_error(autoplot.nacho())
})

test_that('Missing "x"', {
  expect_error(autoplot(object = GSE74821))
})

test_that('Null "x"', {
  expect_error(autoplot(object = GSE74821, x = NULL))
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

  test_that(paste(imetric, "show outliers and labels", sep = " - "), {
    expect_s3_class(
      object = autoplot(
        object = GSE74821,
        x = imetric,
        show_legend = FALSE,
        show_outliers = TRUE,
        outliers_factor = 1,
        outliers_labels = TRUE
      ),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "hide outliers", sep = " - "), {
    expect_s3_class(
      object = autoplot(
        object = GSE74821,
        x = imetric,
        show_legend = FALSE,
        show_outliers = FALSE,
        outliers_factor = 1.2,
        outliers_labels = NULL
      ),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "[salmon] Default parameters", sep = " - "), {
    expect_s3_class(
      object = autoplot(object = salmon_nacho, x = imetric),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "[salmon] show_legend to FALSE parameters", sep = " - "), {
    expect_s3_class(
      object = autoplot(object = salmon_nacho, x = imetric, show_legend = FALSE),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "[salmon] show outliers and labels", sep = " - "), {
    expect_s3_class(
      object = autoplot(
        object = salmon_nacho,
        x = imetric,
        show_legend = FALSE,
        show_outliers = TRUE,
        outliers_factor = 1,
        outliers_labels = TRUE
      ),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "[salmon] hide outliers", sep = " - "), {
    expect_s3_class(
      object = autoplot(
        object = salmon_nacho,
        x = imetric,
        show_legend = FALSE,
        show_outliers = FALSE,
        outliers_factor = 1.2,
        outliers_labels = NULL
      ),
      class = "ggplot"
    )
  })

  if (imetric == "NORM") {
    test_that(paste(imetric, "[salmon] NORM without housekeeping genes ", sep = " - "), {
      salmon2 <- salmon_nacho
      salmon2$housekeeping_genes <- NULL
      expect_s3_class(
        object = autoplot(salmon2, x = imetric),
        class = "ggplot"
      )
    })
  }
}

test_that(paste("HF", "Default parameters", sep = " - "), {
  expect_s3_class(
    object = autoplot(object = plexset_nacho, x = "HF"),
    class = "ggplot"
  )
})

test_that(paste("Housekeeping", "no genes", sep = " - "), {
  plexset_nacho$housekeeping_genes <- NULL
  expect_s3_class(
    object = autoplot(object = plexset_nacho, x = "Housekeeping"),
    class = "ggplot"
  )
})
