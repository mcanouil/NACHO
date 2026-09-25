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
  "BD",
  "FoV",
  "PCL",
  "LoD",
  "Positive",
  "Negative",
  "Housekeeping",
  "PN",
  "ACBD",
  "ACMC",
  "PCA12",
  "PCAi",
  "PCA",
  "PFNF",
  "HF",
  "NORM"
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

  test_that(
    paste(imetric, "[salmon] show_legend to FALSE parameters", sep = " - "),
    {
      expect_s3_class(
        object = autoplot(
          object = salmon_nacho,
          x = imetric,
          show_legend = FALSE
        ),
        class = "ggplot"
      )
    }
  )

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
    test_that(
      paste(imetric, "[salmon] NORM without housekeeping genes ", sep = " - "),
      {
        salmon2 <- salmon_nacho
        salmon2$housekeeping_genes <- NULL
        expect_s3_class(
          object = autoplot(salmon2, x = imetric),
          class = "ggplot"
        )
      }
    )
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

for (imetric in metrics) {
  test_that(paste(imetric, "builds without warnings", sep = " - "), {
    expect_no_warning(
      ggplot2::ggplot_build(autoplot(
        object = GSE74821,
        x = imetric,
        colour = "Date"
      ))
    )
  })
}

n_samples <- length(unique(GSE74821[["nacho"]][[GSE74821[["access"]]]]))

for (metric in c("BD", "FoV", "PCL", "LoD", "PN")) {
  test_that(paste(metric, "plot keeps one x value per sample"), {
    plot <- autoplot(GSE74821, x = metric)
    expect_length(unique(plot[["data"]][[GSE74821[["access"]]]]), n_samples)
  })
}

for (metric in c("Positive", "Negative", "Housekeeping")) {
  test_that(paste(metric, "plot keeps every sample and probe"), {
    nacho_df <- data.table::as.data.table(GSE74821[["nacho"]])
    expected <- unique(nacho_df[
      nacho_df[["CodeClass"]] == metric,
      c(GSE74821[["access"]], "Name"),
      with = FALSE
    ])
    plot <- autoplot(GSE74821, x = metric)
    expect_identical(nrow(plot[["data"]]), nrow(expected))
  })
}

test_that("lane-level plots of PlexSet data keep one x value per RCC file", {
  plot <- autoplot(salmon_nacho, x = "BD")
  expect_length(unique(plot[["data"]][["IDFILE"]]), length(salmon_files))
})
