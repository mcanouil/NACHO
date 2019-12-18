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

rcc_files_directory <- "salmon_data"
targets <- data.frame(stringsAsFactors = FALSE,
  name = list.files(rcc_files_directory),
  datapath = list.files(rcc_files_directory, full.names = TRUE)
)
targets$IDFILE <- basename(targets$datapath)
targets$plexset_id <- rep(list(paste0("S", 1:8)), each = nrow(targets))
targets_tidy <- as.data.frame(tidyr::unnest(targets, "plexset_id"))
salmon <- load_rcc(
  data_directory = rcc_files_directory,
  ssheet_csv = targets_tidy,
  id_colname = "IDFILE"
)
rm(list = c("targets_tidy", "targets", "rcc_files_directory"))

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
      object = autoplot(object = salmon, x = imetric),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "[salmon] show_legend to FALSE parameters", sep = " - "), {
    expect_s3_class(
      object = autoplot(object = salmon, x = imetric, show_legend = FALSE),
      class = "ggplot"
    )
  })

  test_that(paste(imetric, "[salmon] show outliers and labels", sep = " - "), {
    expect_s3_class(
      object = autoplot(
        object = salmon,
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
        object = salmon,
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
      salmon2 <- salmon
      salmon2$housekeeping_genes <- NULL
      expect_s3_class(
        object = autoplot(salmon2, x = imetric),
        class = "ggplot"
      )
    })
  }
}

rcc_files_directory <- "plexset_data"
targets <- data.frame(stringsAsFactors = FALSE,
  name = list.files(rcc_files_directory),
  datapath = list.files(rcc_files_directory, full.names = TRUE)
)
targets$IDFILE <- basename(targets$datapath)
targets$plexset_id <- rep(list(paste0("S", 1:8)), each = nrow(targets))
targets_tidy <- as.data.frame(tidyr::unnest(targets, "plexset_id"))
plexset <- load_rcc(
  data_directory = rcc_files_directory,
  ssheet_csv = targets_tidy,
  id_colname = "IDFILE"
)
rm(list = c("targets_tidy", "targets", "rcc_files_directory"))
test_that(paste("HF", "Default parameters", sep = " - "), {
  expect_s3_class(
    object = autoplot(object = plexset, x = "HF"),
    class = "ggplot"
  )
})