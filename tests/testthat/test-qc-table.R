qc_samples <- function() {
  data.frame(
    IDFILE = c("a", "b", "c"),
    ID = c("1", "2", "3"),
    CartridgeID = "C1",
    BD = c(1, 2.5, NA),
    FoV = c(90, 60, 90),
    PCL = c(0.99, 0.99, 0.5),
    LoD = c(3, 3, 3),
    MC = 1,
    MedC = 1,
    Positive_factor = c(1, 1, 1),
    Negative_factor = 1,
    Background = NA_real_,
    House_factor = c(1, 1, 20)
  )
}

test_that("qc_table() gives a status per metric, n_flags, status and reason", {
  qc <- NACHO:::qc_table(qc_samples(), nacho_thresholds(), "n1", "IDFILE")
  expect_identical(qc$BD_status, c("pass", "fail", NA))
  expect_identical(qc$FoV_status, c("pass", "fail", "pass"))
  expect_identical(qc$n_flags, c(0L, 2L, 2L))
  expect_identical(qc$status, c("pass", "fail", "fail"))
  expect_identical(qc$reason[1], NA_character_)
  expect_identical(qc$reason[2], "BD 2.5 above 2.25; FoV 60 below 75")
  expect_identical(qc$reason[3], "PCL 0.5 below 0.95; House_factor 20 above 10")
  expect_identical(names(qc)[1:3], c("IDFILE", "lane", "CartridgeID"))
  expect_false("lane_status" %in% names(qc))
})

test_that("a sample with only missing metrics has a missing status", {
  samples <- qc_samples()[1, ]
  samples[, c(
    "BD",
    "FoV",
    "PCL",
    "LoD",
    "Positive_factor",
    "House_factor"
  )] <- NA_real_
  expect_identical(
    NACHO:::qc_table(samples, nacho_thresholds(), "n1", "IDFILE")$status,
    NA_character_
  )
})

test_that("PlexSet lane failure reaches every sample of the lane", {
  samples <- data.frame(
    IDFILE = sprintf("f_S%d", 1:4),
    ID = c("1", "1", "2", "2"),
    CartridgeID = "C1",
    BD = c(3, 3, 1, 1),
    FoV = 90,
    PCL = 0.1,
    LoD = 0,
    MC = 1,
    MedC = 1,
    Positive_factor = 1,
    Negative_factor = 1,
    Background = NA_real_
  )
  qc <- NACHO:::qc_table(samples, nacho_thresholds(), "n8", "IDFILE")
  expect_identical(qc$lane_status, c("fail", "fail", "pass", "pass"))
  expect_identical(qc$status, c("fail", "fail", "pass", "pass"))
  expect_true(all(is.na(qc$PCL_status)))
  expect_true(all(is.na(qc$LoD_status)))
})

test_that("a lane failure propagates to a sample that passes on its own", {
  samples <- data.frame(
    IDFILE = sprintf("f_S%d", 1:4),
    ID = c("1", "1", "2", "2"),
    CartridgeID = "C1",
    BD = c(3, NA, 1, 1),
    FoV = 90,
    PCL = 0.1,
    LoD = 0,
    MC = 1,
    MedC = 1,
    Positive_factor = 1,
    Negative_factor = 1,
    Background = NA_real_
  )
  qc <- NACHO:::qc_table(samples, nacho_thresholds(), "n8", "IDFILE")
  expect_identical(qc$lane_status, c("fail", "fail", "pass", "pass"))
  expect_identical(qc$status, c("fail", "fail", "pass", "pass"))
  expect_identical(qc$n_flags, c(1L, 0L, 0L, 0L))
  expect_identical(qc$reason[1], "BD 3 above 2.25")
  expect_identical(qc$reason[2], "lane fails BD")
  expect_identical(qc$reason[3], NA_character_)
})

test_that("nacho_qc() on real PlexSet data has lane_status", {
  qc <- nacho_qc(plexset_nacho)
  expect_true("lane_status" %in% names(qc))
  expect_identical(nrow(qc), ncol(plexset_nacho))
})

test_that("changing thresholds changes the status without a rebuild", {
  x <- GSE74821
  x@thresholds$BD <- c(-Inf, 0)
  expect_true(all(nacho_qc(x)$BD_status == "fail"))
  expect_identical(
    nacho_counts(x, normalised = TRUE),
    nacho_counts(GSE74821, normalised = TRUE)
  )
})

test_that("exclude_outliers() drops the samples whose status is fail", {
  x <- GSE74821
  x@thresholds$BD <- c(0, stats::median(x@samples$BD))
  failing <- nacho_qc(x)$status %in% "fail"
  skip_if(!any(failing) || all(failing))
  kept <- suppressMessages(exclude_outliers(x))
  expect_identical(ncol(kept), sum(!failing))
})
