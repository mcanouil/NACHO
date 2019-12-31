test_that("default settings", {
  res <- normalise(
    nacho_object = GSE74821
  )
  expect_s3_class(res, "nacho")
})

test_that("missing nacho", {
  expect_error(normalise())
})

test_that("missing field", {
  GSE74821$nacho <- NULL
  expect_error(normalise(GSE74821))
})

test_that("No POS_E", {
  GSE74821$nacho <- GSE74821$nacho[GSE74821$nacho$Name!="POS_E(0.5)", ]
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = FALSE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("genes not null", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = c("RPLP0", "ACTB"),
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = FALSE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("predict TRUE", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = TRUE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = FALSE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("norm TRUE", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = TRUE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = FALSE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("method GLM", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GLM",
    n_comp = 10,
    remove_outliers = FALSE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("n_comp 2", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 2,
    remove_outliers = FALSE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("n_comp 10", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = FALSE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("outliers TRUE", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = FALSE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})


test_that("Test outliers", {
  res <- normalise(
    nacho_object = GSE74821,
    remove_outliers = TRUE,
    outliers_thresholds = list(
      BD = c(0.15, 2.25),
      FoV = 95,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})


test_that("All LoD to zero", {
  GSE74821$nacho$LoD <- 0
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = c("RPLP0", "ACTB"),
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = FALSE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("All PC to zero", {
  GSE74821$nacho$PC <- 0
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = c("RPLP0", "ACTB"),
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = FALSE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("housekeeping_norm to FALSE and remove_outliers to TRUE", {
  GSE74821$nacho$PC <- 0
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = TRUE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("housekeeping_norm to TRUE and remove_outliers to TRUE", {
  GSE74821$nacho$PC <- 0
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = TRUE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = TRUE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("housekeeping_norm to TRUE and remove_outliers to TRUE", {
  GSE74821$nacho$PC <- 0
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = TRUE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = TRUE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PCL = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
  expect_s3_class(res, "nacho")
})

test_that("housekeeping_norm to TRUE and remove_outliers to TRUE", {
  attr(GSE74821, "RCC_type") <- "something"
  expect_error(normalise(GSE74821))
})


test_that("Missing values in counts", {
  index <- sample(which(GSE74821$nacho$CodeClass=="Endogenous"), size = 25)
  GSE74821$nacho[index, "Count"] <- NA
  GSE74821$nacho[index, "Count_Norm"] <- NA
  expect_message(
    object = normalise(GSE74821, normalisation_method = "GEO"),
    regexp = "Missing values have been replaced with zeros for PCA"
  )
})

test_that("plexset", {
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
  expect_s3_class(
    object = {
      normalise(plexset, housekeeping_predict = TRUE, housekeeping_norm = TRUE)
    },
    class = "nacho"
  )
})


test_that("plexset GLM", {
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
  expect_s3_class(
    object = {
      normalise(plexset, housekeeping_predict = TRUE, housekeeping_norm = TRUE, normalisation_method = "GLM")
    },
    class = "nacho"
  )
})

test_that("salmon GLM", {
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
  expect_s3_class(
    object = {
      normalise(plexset, housekeeping_predict = TRUE, housekeeping_norm = TRUE, normalisation_method = "GLM")
    },
    class = "nacho"
  )
})