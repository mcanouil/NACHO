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
