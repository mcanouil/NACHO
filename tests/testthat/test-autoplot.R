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
  for (type in setdiff(names(NACHO:::nacho_plot_registry), "Stability")) {
    expect_no_warning(ggplot2::ggplot_build(autoplot(toy, type = type)))
  }
})

test_that("flagged samples of a toy object get their own layers", {
  toy <- toy_nacho(6L)
  samples <- toy@samples
  samples[["BD"]][1] <- 5
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

test_that("PCBatch leaves tiles without a value unlabelled", {
  x <- mirna_fixture()
  plot <- autoplot(x, type = "PCBatch")
  expect_true(anyNA(plot$data$r_squared))
  built <- ggplot2::ggplot_build(plot)
  labels <- built$data[[2]]$label
  expect_false("NA" %in% labels)
  expect_identical(
    nrow(built$data[[2]]),
    sum(!is.na(plot$data$r_squared))
  )
})

test_that("PCBatch labels sit on a paper box in ink", {
  for (dark in c(FALSE, TRUE)) {
    colours <- NACHO:::plot_colours(dark)
    built <- ggplot2::ggplot_build(autoplot(
      GSE74821,
      type = "PCBatch",
      dark = dark
    ))
    labels <- built$data[[2]]
    expect_gt(nrow(labels), 0L)
    expect_true(all(labels$label != ""))
    expect_true(all(labels$fill == colours[["paper"]]))
    expect_true(all(labels$colour == colours[["ink"]]))
  }
})

test_that("ink on paper keeps the plot text readable", {
  skip_if_not_installed("colorspace")
  for (dark in c(FALSE, TRUE)) {
    colours <- NACHO:::plot_colours(dark)
    expect_gte(
      colorspace::contrast_ratio(colours[["ink"]], colours[["paper"]]),
      4.5
    )
  }
})

test_that("PCBatch is not available without principal components", {
  toy <- toy_nacho(6L)
  toy@pca[["scores"]] <- toy@pca[["scores"]][, 0, drop = FALSE]
  expect_not_available(toy, "PCBatch")
})

test_that("PCBatch is not available without batch columns", {
  toy <- toy_nacho(6L)
  toy@samples <- toy@samples[,
    setdiff(names(toy@samples), c("CartridgeID", "Date")),
    drop = FALSE
  ]
  expect_warning(
    plot <- autoplot(toy, type = "PCBatch", colour = "IDFILE"),
    class = "nacho_warning_metric_unavailable"
  )
  expect_identical(
    ggplot2::ggplot_build(plot)$data[[1]]$label,
    "Not available!"
  )
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

test_that("the Stability plot needs a detection limit", {
  toy <- toy_nacho(6L)
  expect_not_available(toy, "Stability")
})

test_that("the Stability plot orders the genes by geNorm rank", {
  expect_no_warning(plot <- autoplot(GSE74821, type = "Stability"))
  expect_identical(
    levels(plot[["data"]][["Name"]]),
    housekeeping_stability(GSE74821)[["ranking"]][["Name"]]
  )
})

test_that("the RLE plot centres each gene on its median", {
  plot <- autoplot(GSE74821, type = "RLE")
  data <- plot$data
  expect_true(all(c("sample", "rle") %in% names(data)))
  genes <- nacho_probes(GSE74821)$Name[
    grepl("Endogenous", nacho_probes(GSE74821)$CodeClass)
  ]
  expect_setequal(unique(data$Name), genes)
  medians <- tapply(data$rle, data$Name, stats::median)
  expect_equal(as.vector(medians), rep(0, length(medians)), tolerance = 1e-8)
  gene <- genes[3]
  sample <- as.character(data$sample[data$Name == gene][2])
  log_gene <- log2(nacho_counts(GSE74821, normalised = TRUE)[gene, ] + 1)
  expect_equal(
    data$rle[data$Name == gene & data$sample == sample],
    unname(log_gene[sample] - stats::median(log_gene))
  )
})

test_that("PCBatch shows one tile per component and batch variable", {
  plot <- autoplot(GSE74821, type = "PCBatch")
  expect_identical(nrow(plot$data), ncol(GSE74821@pca$scores) * 2L)
})

test_that("the RLE plot orders samples by a numeric colour numerically", {
  object <- GSE74821
  n <- nrow(object@samples)
  object@samples[["rank"]] <- rev(seq_len(n)) * 1
  object@samples[["rank"]][1:2] <- c(10, 2)
  plot <- autoplot(object, type = "RLE", colour = "rank")
  ordered <- object@samples[["IDFILE"]][order(object@samples[["rank"]])]
  expect_identical(levels(plot$data[["sample"]]), ordered)
})

test_that("RLE and BatchFactors build with one box per sample or cartridge", {
  rle <- ggplot2::ggplot_build(autoplot(GSE74821, type = "RLE"))
  expect_length(
    unique(rle$data[[2]]$x),
    nrow(nacho_samples(GSE74821))
  )
  factors <- ggplot2::ggplot_build(autoplot(GSE74821, type = "BatchFactors"))
  n_factors <- length(intersect(
    c("Positive_factor", "Negative_factor", "House_factor"),
    names(nacho_samples(GSE74821))
  ))
  n_cartridges <- length(unique(nacho_samples(GSE74821)$CartridgeID))
  expect_length(unique(factors$data[[1]]$PANEL), n_factors)
  expect_identical(
    nrow(unique(factors$data[[1]][c("PANEL", "x")])),
    n_factors * n_cartridges
  )
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
  "NORM",
  "Stability",
  "RLE",
  "BatchFactors",
  "PCBatch"
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

for (metric in c("PN", "NORM")) {
  test_that(paste(metric, "builds without the geom_smooth() formula message"), {
    expect_no_message(ggplot2::ggplot_build(autoplot(GSE74821, type = metric)))
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

test_that("only the failing sample is flagged in the sample and probe tables", {
  toy <- toy_nacho(6L)
  toy@thresholds[["BD"]] <- c(0.1, 0.5)
  toy@samples[["BD"]][-1] <- 0.3
  id <- toy@settings[["id_colname"]]
  failing <- toy@samples[[id]][1]

  samples <- NACHO:::plot_samples(toy, "CartridgeID")
  expect_identical(samples[["flagged"]], c(TRUE, rep(FALSE, 5)))

  long <- NACHO:::plot_probes(toy, NULL, "CartridgeID")
  expect_true(all(long[["flagged"]][long[[id]] == failing]))
  expect_false(any(long[["flagged"]][long[[id]] != failing]))
})

test_that("the outlier layer of a plot holds only the failing sample", {
  toy <- toy_nacho(6L)
  toy@thresholds[["BD"]] <- c(0.1, 0.5)
  toy@samples[["BD"]][-1] <- 0.3
  plot <- autoplot(toy, type = "BD")
  layers <- lapply(seq_along(plot$layers), \(i) ggplot2::layer_data(plot, i))
  failing_value <- toy@samples[["BD"]][1]
  draws_failing <- vapply(
    layers,
    \(data) "y" %in% names(data) && any(data[["y"]] == failing_value),
    logical(1)
  )
  expect_identical(sum(draws_failing), 1L)
  expect_identical(nrow(layers[[which(draws_failing)]]), 1L)
  expect_true(all(layers[[which(draws_failing)]][["y"]] == failing_value))
})

test_that("BatchFactors drops a factor column the samples lack", {
  toy <- toy_nacho(6L)
  toy@samples[["Negative_factor"]] <- NULL
  plot <- autoplot(toy, type = "BatchFactors")
  expect_setequal(
    as.character(unique(plot$data[["factor"]])),
    c("Positive_factor", "House_factor")
  )
  built <- ggplot2::ggplot_build(plot)
  expect_length(unique(built$data[[1]]$PANEL), 2L)
})

test_that("flagged samples are triangles in the accent colour", {
  x <- flagged_gse()
  light <- flagged_points(autoplot(x, type = "FoV"))
  dark <- flagged_points(autoplot(x, type = "FoV", dark = TRUE))
  expect_gt(nrow(light), 0)
  expect_true(all(light$colour == "#B64326"))
  expect_true(all(dark$colour == "#FCB448"))
})

test_that("dark plots draw nothing in black or the old red", {
  x <- flagged_gse()
  for (type in names(NACHO:::nacho_plot_registry)) {
    built <- suppressWarnings(ggplot2::ggplot_build(
      autoplot(x, type = type, dark = TRUE)
    ))
    colours <- toupper(unlist(lapply(built$data, function(d) {
      c(d$colour, d$fill)
    })))
    expect_false(
      any(colours %in% c("#000000", "BLACK", "#B22222", "FIREBRICK")),
      info = type
    )
    theme <- ggplot2::complete_theme(built$plot$theme)
    expect_identical(
      ggplot2::calc_element("plot.background", theme)$fill,
      "#111821",
      info = type
    )
  }
})

test_that("dark labels of flagged samples take the paper fill", {
  x <- flagged_gse()
  built <- ggplot2::ggplot_build(
    autoplot(x, type = "FoV", dark = TRUE, outliers_labels = "CartridgeID")
  )
  labelled <- Filter(function(d) "label" %in% names(d), built$data)
  expect_length(labelled, 1L)
  expect_true(all(labelled[[1]]$fill == "#111821"))
})

test_that("the PCBatch missing-value fill follows light and dark mode", {
  x <- flagged_gse()
  na_fill <- function(dark) {
    built <- ggplot2::ggplot_build(autoplot(x, type = "PCBatch", dark = dark))
    built$plot$scales$get_scales("fill")$na.value
  }
  expect_false(identical(na_fill(TRUE), na_fill(FALSE)))
  expect_false(identical(na_fill(FALSE), "grey90"))
})

test_that("numeric and missing colour columns plot without warnings", {
  x <- GSE74821
  samples <- x@samples
  samples[["dose"]] <- seq_len(nrow(samples))
  samples[["batch"]] <- rep(c("a", NA), length.out = nrow(samples))
  x@samples <- samples
  for (column in c("dose", "batch")) {
    expect_no_warning(ggplot2::ggplot_build(autoplot(
      x,
      type = "BD",
      colour = column
    )))
  }
  built <- ggplot2::ggplot_build(autoplot(x, type = "BD", colour = "dose"))
  colours <- Filter(
    function(v) length(v) > 1,
    lapply(built$data, function(d) unique(d$colour))
  )[[1]]
  expect_setequal(colours, scales::pal_viridis(end = 0.85)(nrow(samples)))
})

test_that("autoplot() checks dark", {
  expect_error(
    autoplot(GSE74821, type = "BD", dark = NA),
    class = "nacho_error_bad_argument"
  )
})

test_that("interactive plots carry the sample id and the metric", {
  skip_if_not_installed("ggiraph")
  x <- flagged_gse()
  plot <- NACHO:::app_plot(x, "FoV", list(), dark = FALSE, interactive = TRUE)
  built <- ggplot2::ggplot_build(plot)
  points <- Filter(function(d) "data_id" %in% names(d), built$data)
  expect_gt(length(points), 0)
  ids <- unlist(lapply(points, function(d) d$data_id))
  expect_setequal(ids, colnames(x@counts))
  tooltips <- unlist(lapply(points, function(d) d$tooltip))
  expect_match(tooltips[1], "^GSM[^\n]+\nFoV: ")
})

test_that("autoplot() stays static", {
  plot <- autoplot(GSE74821, type = "FoV")
  layers <- vapply(plot$layers, function(l) class(l$geom)[1], character(1))
  expect_false(any(grepl("Interactive", layers)))
})
