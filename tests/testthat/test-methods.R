test_that("print() shows a short summary and returns the object invisibly", {
  x <- toy_nacho()
  expect_snapshot(print(x))
  utils::capture.output(expect_invisible(print(x)))
  utils::capture.output(visible <- withVisible(print(x)))
  expect_identical(visible$value, x)
})

test_that("format() gives the lines print() shows", {
  expect_type(format(toy_nacho()), "character")
  expect_match(format(toy_nacho())[1], "4 samples")
})

test_that("dim() gives probes by samples", {
  expect_identical(dim(toy_nacho()), c(11L, 4L))
  expect_identical(ncol(toy_nacho()), 4L)
})

test_that("summary() counts failures per metric", {
  x <- toy_nacho()
  samples <- x@samples
  samples$BD[1] <- 5
  x@samples <- samples
  summary_table <- summary(x)
  expect_identical(summary_table$n_fail[summary_table$metric == "BD"], 1L)
})

test_that("as.data.frame() gives the samples or the long layout", {
  x <- toy_nacho()
  expect_identical(as.data.frame(x), nacho_samples(x))
  long <- as.data.frame(x, long = TRUE)
  expect_identical(nrow(long), 44L)
  expect_true(all(
    c("IDFILE", "CodeClass", "Name", "Count", "Count_Norm", "PC01") %in%
      names(long)
  ))
  expect_identical(
    long$Count[long$IDFILE == "S02.RCC" & long$Name == "GENE1"],
    x@counts["GENE1", "S02.RCC"]
  )
})

test_that("as.data.frame(long = TRUE) works when the PCA has zero components", {
  x <- toy_nacho()
  x@pca <- list(
    scores = x@pca$scores[, 0, drop = FALSE],
    importance = x@pca$importance[0, ]
  )
  long <- as.data.frame(x, long = TRUE)
  expect_identical(nrow(long), 44L)
  expect_false(any(c("PC01", "PC02") %in% names(long)))
  expect_true(all(
    c("IDFILE", "CodeClass", "Name", "Count", "Count_Norm") %in% names(long)
  ))
})

test_that("x[, j] subsets samples and recomputes PCA and flags", {
  x <- toy_nacho(6L)
  thresholds <- x@thresholds
  thresholds$BD <- c(0.1, 0.5)
  x@thresholds <- thresholds
  samples <- x@samples
  samples$is_outlier <- NACHO:::compute_outliers(samples, thresholds, "n1")
  x@samples <- samples
  sub <- x[, 2:4]
  expect_identical(colnames(nacho_counts(sub)), colnames(nacho_counts(x))[2:4])
  expect_identical(nrow(nacho_samples(sub)), 3L)
  expect_identical(nacho_qc(sub)$is_outlier, rep(TRUE, 3))
  expect_identical(sub@thresholds, thresholds)
  expect_identical(dim(sub@pca$scores), c(3L, 2L))
})

test_that("x[i, ] subsets probes by name, position or logical", {
  x <- toy_nacho()
  expect_identical(nrow(x[c("GENE1", "GENE2"), ]), 2L)
  expect_identical(nrow(x[1:3, ]), 3L)
  expect_identical(nrow(x[nacho_probes(x)$CodeClass == "Positive", ]), 6L)
})

test_that("x[, j] accepts sample ids and logical vectors", {
  x <- toy_nacho()
  expect_warning(
    sub_ids <- x[, c("S01.RCC", "S03.RCC")],
    class = "nacho_warning_n_comp_reduced"
  )
  expect_identical(ncol(sub_ids), 2L)
  expect_warning(
    sub_logical <- x[, c(TRUE, FALSE, TRUE, FALSE)],
    class = "nacho_warning_n_comp_reduced"
  )
  expect_identical(ncol(sub_logical), 2L)
})

test_that("subsetting to two samples keeps a valid object", {
  x <- toy_nacho()
  expect_warning(sub <- x[, 1:2], class = "nacho_warning_n_comp_reduced")
  expect_identical(ncol(sub@pca$scores), 1L)
  expect_true(S7::S7_inherits(sub, NACHO:::nacho))
})

test_that("subsetting refuses unknown or repeated ids", {
  x <- toy_nacho()
  expect_error(x[, "nope"], class = "nacho_error_bad_argument")
  expect_error(x[, c(1, 1)], class = "nacho_error_bad_argument")
  expect_error(x[, 99], class = "nacho_error_bad_argument")
})

test_that("a logical index must match the dimension length", {
  x <- toy_nacho()
  expect_error(x[, c(TRUE, FALSE)], class = "nacho_error_bad_argument")
  expect_error(x[c(TRUE, FALSE), ], class = "nacho_error_bad_argument")
})

test_that("a logical index cannot contain NA", {
  x <- toy_nacho()
  expect_error(
    x[, c(TRUE, NA, FALSE, TRUE)],
    class = "nacho_error_bad_argument"
  )
})

test_that("x[i] without a comma is refused", {
  x <- toy_nacho()
  expect_error(x[1:2], class = "nacho_error_bad_argument")
})

test_that("a named single subscript is unambiguous", {
  x <- toy_nacho()
  expect_identical(dim(x[i = 1:2]), c(2L, 4L))
  expect_warning(
    sub <- x[j = 1:2],
    class = "nacho_warning_n_comp_reduced"
  )
  expect_identical(dim(sub), c(11L, 2L))
})

test_that("subsetting to one probe keeps a valid object", {
  x <- toy_nacho()
  expect_warning(sub <- x[1, ], class = "nacho_warning_n_comp_reduced")
  expect_identical(dim(sub), c(1L, 4L))
  expect_identical(ncol(sub@pca$scores), 1L)
  expect_true(S7::S7_inherits(sub, NACHO:::nacho))
})

test_that("package code runs data.table expressions without data.table attached", {
  expect_false("package:data.table" %in% search())
  long <- as.data.frame(GSE74821, long = TRUE)
  expect_identical(nrow(long), as.integer(prod(dim(GSE74821))))
  expect_s3_class(autoplot(GSE74821, type = "PCA"), "ggplot")
})

test_that("print() and format() of a NACHO 2 object point to upgrade_nacho()", {
  old <- readRDS(test_path("fixtures", "nacho-2-GSE74821-subset.rds"))
  expect_match(format(old), "upgrade_nacho()", fixed = TRUE)
  output <- utils::capture.output(visible <- withVisible(print(old)))
  expect_match(paste(output, collapse = "\n"), "upgrade_nacho()", fixed = TRUE)
  expect_false(visible$visible)
  expect_identical(visible$value, old)
})

test_that("the other methods refuse a NACHO 2 object", {
  old <- readRDS(test_path("fixtures", "nacho-2-GSE74821-subset.rds"))
  calls <- list(
    summary = function() summary(old),
    dim = function() dim(old),
    as.data.frame = function() as.data.frame(old),
    as.data.frame_arguments = function() {
      as.data.frame(old, row.names = NULL, optional = FALSE)
    },
    subset = function() old[1, ]
  )
  for (name in names(calls)) {
    error <- expect_error(calls[[name]](), class = "nacho_error_bad_object")
    expect_match(
      conditionMessage(error),
      "upgrade_nacho",
      fixed = TRUE,
      info = name
    )
  }
})

call_from_outside <- function(fun, x, ...) {
  env <- new.env(parent = emptyenv())
  env$x <- x
  eval(as.call(c(fun, quote(x), list(...))), env)
}

test_that("the NACHO 2 methods dispatch from outside the namespace", {
  old <- readRDS(test_path("fixtures", "nacho-2-GSE74821-subset.rds"))
  output <- utils::capture.output(call_from_outside(base::print, old))
  expect_match(output, "upgrade_nacho()", fixed = TRUE)
  expect_match(
    call_from_outside(base::format, old),
    "upgrade_nacho()",
    fixed = TRUE
  )
  refusals <- list(
    base::summary,
    base::dim,
    base::as.data.frame,
    ggplot2::autoplot
  )
  for (fun in refusals) {
    expect_error(call_from_outside(fun, old), class = "nacho_error_bad_object")
  }
  expect_error(
    call_from_outside(base::`[`, old, 1, quote(expr = )),
    class = "nacho_error_bad_object"
  )
})

test_that("autoplot() refuses a NACHO 2 object", {
  old <- readRDS(test_path("fixtures", "nacho-2-GSE74821-subset.rds"))
  expect_error(autoplot(old), class = "nacho_error_bad_object")
})

test_that("NACHO 2 method errors name the argument", {
  old <- readRDS(test_path("fixtures", "nacho-2-GSE74821-subset.rds"))
  expect_snapshot(dim(old), error = TRUE)
  expect_snapshot(summary(old), error = TRUE)
})

test_that("the NACHO 2 guard stops on an object check_nacho() accepts", {
  error <- expect_error(
    NACHO:::abort_nacho_v2(GSE74821),
    class = "nacho_error_internal"
  )
  expect_no_match(conditionMessage(error), "upgrade_nacho", fixed = TRUE)
  expect_match(conditionMessage(error), "check_nacho()", fixed = TRUE)
})

test_that("format() of a list with only the NACHO 2 class points to load_rcc()", {
  expect_match(
    format(structure(list(), class = "nacho")),
    "load_rcc()",
    fixed = TRUE
  )
})
