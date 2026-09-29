test_that('Missing "object"', {
  expect_error(NACHO:::autoplot_nacho(), class = "nacho_error_bad_object")
})

test_that("autoplot() needs a known plot type", {
  expect_error(autoplot(GSE74821), class = "nacho_error_bad_argument")
  expect_error(
    autoplot(GSE74821, type = "PFB"),
    class = "nacho_error_bad_argument"
  )
  expect_snapshot(autoplot(GSE74821, type = "bd"), error = TRUE)
})

test_that("autoplot() points NACHO 2 callers to type", {
  expect_error(autoplot(GSE74821, x = "BD"), class = "nacho_error_bad_argument")
  expect_snapshot(autoplot(GSE74821, x = "BD"), error = TRUE)
})

test_that("autoplot() checks colour and outliers_labels columns", {
  expect_error(
    autoplot(GSE74821, type = "BD", colour = "nope"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    autoplot(GSE74821, type = "BD", outliers_labels = "nope"),
    class = "nacho_error_bad_argument"
  )
})

test_that("every plot type builds", {
  for (type in names(NACHO:::nacho_plot_registry)) {
    expect_s3_class(autoplot(GSE74821, type = type), "ggplot")
  }
})

test_that("every plot type of a toy object builds without warnings", {
  toy <- toy_nacho(6L)
  for (type in names(NACHO:::nacho_plot_registry)) {
    expect_no_warning(ggplot2::ggplot_build(autoplot(toy, type = type)))
  }
})

test_that("flagged samples of a toy object get their own layers", {
  toy <- toy_nacho(6L)
  samples <- toy@samples
  samples[["BD"]][1] <- 5
  samples[["is_outlier"]][1] <- TRUE
  toy@samples <- samples
  for (type in c("BD", "Positive", "ACBD", "PFNF", "HF")) {
    plot <- autoplot(toy, type = type, outliers_labels = "IDFILE")
    expect_no_warning(ggplot2::ggplot_build(plot))
    expect_true(any(vapply(
      plot[["layers"]],
      function(layer) inherits(layer[["geom"]], "GeomLabelRepel"),
      logical(1)
    )))
  }
})

test_that("PCL and LoD plots of a PlexSet toy object are not available", {
  toy <- toy_nacho(6L)
  toy@rcc_type <- "n8"
  for (type in c("PCL", "LoD")) {
    expect_not_available(toy, type)
  }
  expect_no_warning(ggplot2::ggplot_build(autoplot(toy, type = "BD")))
})

test_that("the Housekeeping plot of a toy object without housekeeping genes is not available", {
  toy <- toy_nacho(6L)
  probes <- toy@probes
  probes[["is_housekeeping"]] <- FALSE
  toy@probes <- probes
  expect_warning(
    plot <- autoplot(toy, type = "Housekeeping"),
    class = "nacho_warning_metric_unavailable"
  )
  expect_length(plot[["layers"]], 1)
  expect_no_warning(ggplot2::ggplot_build(autoplot(toy, type = "NORM")))
})

test_that("PCL and LoD plots of PlexSet data warn that the metric is unavailable", {
  for (type in c("PCL", "LoD")) {
    expect_not_available(plexset_nacho, type)
  }
})

muffle_unavailable <- function(expr) {
  withCallingHandlers(
    expr,
    nacho_warning_metric_unavailable = function(cnd) {
      invokeRestart("muffleWarning")
    }
  )
}

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
      object = autoplot(GSE74821, type = imetric),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "show_legend to FALSE parameters", sep = " - "), {
    expect_s3_class(
      object = autoplot(GSE74821, type = imetric, show_legend = FALSE),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "show outliers and labels", sep = " - "), {
    expect_s3_class(
      object = autoplot(
        GSE74821,
        type = imetric,
        show_legend = FALSE,
        show_outliers = TRUE,
        outliers_factor = 1,
        outliers_labels = "CartridgeID"
      ),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "hide outliers", sep = " - "), {
    expect_s3_class(
      object = autoplot(
        GSE74821,
        type = imetric,
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
      object = muffle_unavailable(autoplot(salmon_nacho, type = imetric)),
      class = "ggplot"
    )
  })

  test_that(
    paste(imetric, "[salmon] show_legend to FALSE parameters", sep = " - "),
    {
      expect_s3_class(
        object = muffle_unavailable(autoplot(
          salmon_nacho,
          type = imetric,
          show_legend = FALSE
        )),
        class = "ggplot"
      )
    }
  )

  test_that(paste(imetric, "[salmon] show outliers and labels", sep = " - "), {
    expect_s3_class(
      object = muffle_unavailable(autoplot(
        salmon_nacho,
        type = imetric,
        show_legend = FALSE,
        show_outliers = TRUE,
        outliers_factor = 1,
        outliers_labels = "CartridgeID"
      )),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "[salmon] hide outliers", sep = " - "), {
    expect_s3_class(
      object = muffle_unavailable(autoplot(
        salmon_nacho,
        type = imetric,
        show_legend = FALSE,
        show_outliers = FALSE,
        outliers_factor = 1.2,
        outliers_labels = NULL
      )),
      class = "ggplot"
    )
  })

  if (imetric == "NORM") {
    test_that(
      paste(imetric, "[salmon] NORM without housekeeping genes ", sep = " - "),
      {
        salmon2 <- salmon_nacho
        probes <- salmon2@probes
        probes[["is_housekeeping"]] <- FALSE
        salmon2@probes <- probes
        expect_s3_class(
          object = autoplot(salmon2, type = imetric),
          class = "ggplot"
        )
      }
    )
  }
}

test_that(paste("HF", "Default parameters", sep = " - "), {
  expect_s3_class(
    object = muffle_unavailable(autoplot(plexset_nacho, type = "HF")),
    class = "ggplot"
  )
})

test_that(paste("Housekeeping", "no genes", sep = " - "), {
  probes <- plexset_nacho@probes
  probes[["is_housekeeping"]] <- FALSE
  plexset_nacho@probes <- probes
  expect_s3_class(
    object = muffle_unavailable(autoplot(
      plexset_nacho,
      type = "Housekeeping"
    )),
    class = "ggplot"
  )
})

for (imetric in metrics) {
  test_that(paste(imetric, "builds without warnings", sep = " - "), {
    expect_no_warning(
      ggplot2::ggplot_build(autoplot(
        GSE74821,
        type = imetric,
        colour = "Date"
      ))
    )
  })
}

for (metric in c("BD", "FoV", "PCL", "LoD", "PN")) {
  test_that(paste(metric, "plot keeps one x value per sample"), {
    id <- names(nacho_samples(GSE74821))[1]
    n_samples <- length(unique(nacho_samples(GSE74821)[[id]]))
    plot <- autoplot(GSE74821, type = metric)
    expect_length(unique(plot[["data"]][[id]]), n_samples)
  })
}

for (metric in c("Positive", "Negative", "Housekeeping")) {
  test_that(paste(metric, "plot keeps every sample and probe"), {
    n_samples <- nrow(nacho_samples(GSE74821))
    n_probes <- sum(nacho_probes(GSE74821)[["CodeClass"]] == metric)
    plot <- autoplot(GSE74821, type = metric)
    expect_identical(nrow(plot[["data"]]), n_samples * n_probes)
  })
}

test_that("lane-level plots of PlexSet data keep one x value per RCC file", {
  plot <- autoplot(salmon_nacho, type = "BD")
  expect_length(unique(plot[["data"]][["IDFILE"]]), length(salmon_files))
})

test_that("PCA plots with fewer than two components are not available", {
  expect_warning(
    two_samples <- GSE74821[, 1:2],
    class = "nacho_warning_n_comp_reduced"
  )
  for (type in c("PCA12", "PCA")) {
    expect_warning(
      plot <- autoplot(two_samples, type = type),
      class = "nacho_warning_metric_unavailable"
    )
    expect_length(plot[["layers"]], 1)
    expect_identical(
      ggplot2::ggplot_build(plot)[["data"]][[1]][["label"]],
      "Not available!"
    )
  }
})

test_that("open threshold bounds draw no line", {
  x <- GSE74821
  thresholds <- x@thresholds
  thresholds$House_factor <- c(1 / 11, Inf)
  thresholds$Positive_factor <- c(-Inf, 4)
  thresholds$BD <- c(-Inf, 2.25)
  thresholds$LoD <- -Inf
  x@thresholds <- thresholds
  line_values <- function(plot) {
    lines <- Filter(
      function(layer) {
        inherits(layer$geom, "GeomHline") || inherits(layer$geom, "GeomVline")
      },
      plot$layers
    )
    unlist(lapply(lines, function(layer) layer$data$value), use.names = FALSE)
  }
  expected <- list(
    HF = c(1 / 11, 4),
    PFNF = 4,
    BD = 2.25,
    LoD = numeric(0)
  )
  for (type in names(expected)) {
    plot <- muffle_unavailable(autoplot(x, type = type))
    expect_identical(line_values(plot), expected[[type]], info = type)
    expect_no_warning(ggplot2::ggplot_build(plot))
  }
})
