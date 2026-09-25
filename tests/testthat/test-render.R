test_that("Default parameters", {
  if (rmarkdown::pandoc_available()) {
    expect_null(render(
      nacho_object = GSE74821,
      output_dir = tempdir(),
      clean = FALSE
    ))
  } else {
    expect_error(render(nacho_object = GSE74821, output_dir = tempdir()))
  }
})

test_that("with legend", {
  if (rmarkdown::pandoc_available()) {
    expect_null(render(
      nacho_object = GSE74821,
      output_dir = tempdir(),
      show_legend = TRUE
    ))
  } else {
    expect_error(render(nacho_object = GSE74821, output_dir = tempdir()))
  }
})

test_that("with Rmd", {
  if (rmarkdown::pandoc_available()) {
    expect_null(render(
      nacho_object = GSE74821,
      output_dir = tempdir(),
      clean = FALSE
    ))
  } else {
    expect_error(render(nacho_object = GSE74821, output_dir = tempdir()))
  }
})

test_that("missing object", {
  if (rmarkdown::pandoc_available()) {
    expect_error(render())
  } else {
    expect_error(render())
  }
})

test_that("wrong attribute", {
  attr(GSE74821, "RCC_type") <- "something_wrong"
  if (rmarkdown::pandoc_available()) {
    expect_error(render(
      nacho_object = GSE74821,
      output_dir = tempdir(),
      clean = FALSE
    ))
  } else {
    expect_error(render(nacho_object = GSE74821, output_dir = tempdir()))
  }
})

test_that("report logo resolves in installed and source packages", {
  expect_true(file.exists(NACHO:::logo_path()))
})

test_that("render() accepts a column name for outliers_labels", {
  skip_if_not(rmarkdown::pandoc_available())
  output_dir <- withr::local_tempdir()
  render(GSE74821, output_dir = output_dir, outliers_labels = "CartridgeID")
  expect_true(file.exists(file.path(output_dir, "NACHO_QC.html")))
})

test_that("render() accepts a colour column whose name holds quotes", {
  skip_if_not(rmarkdown::pandoc_available())
  output_dir <- withr::local_tempdir()
  quoted <- GSE74821
  quoted[["nacho"]][["batch \"A\""]] <- quoted[["nacho"]][["CartridgeID"]]
  render(quoted, output_dir = output_dir, colour = "batch \"A\"")
  expect_true(file.exists(file.path(output_dir, "NACHO_QC.html")))
})
