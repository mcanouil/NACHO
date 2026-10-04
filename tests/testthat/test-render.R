test_that("Default parameters", {
  if (rmarkdown::pandoc_available()) {
    expect_null(render(
      nacho_object = GSE74821,
      output_dir = tempdir()
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

test_that("missing object", {
  expect_error(render(), class = "nacho_error_bad_object")
})

test_that("not a nacho object", {
  expect_error(
    render(
      nacho_object = list(nacho = data.frame()),
      output_dir = tempdir()
    ),
    class = "nacho_error_bad_object"
  )
})

test_that("the brand logo resolves in installed and source packages", {
  expect_true(file.exists(NACHO:::brand_path("nacho_hex.png")))
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
  quoted@samples[["batch \"A\""]] <- quoted@samples[["CartridgeID"]]
  render(quoted, output_dir = output_dir, colour = "batch \"A\"")
  expect_true(file.exists(file.path(output_dir, "NACHO_QC.html")))
})

test_that("render() keeps a folder named tmp_nacho in output_dir", {
  skip_if_not(rmarkdown::pandoc_available())
  output_dir <- withr::local_tempdir()
  user_folder <- file.path(output_dir, "tmp_nacho")
  dir.create(user_folder)
  writeLines("keep me", file.path(user_folder, "notes.txt"))
  render(GSE74821, output_dir = output_dir)
  expect_true(file.exists(file.path(output_dir, "NACHO_QC.html")))
  expect_true(file.exists(file.path(user_folder, "notes.txt")))
})

test_that("render() leaves no working folder in tempdir()", {
  skip_if_not(rmarkdown::pandoc_available())
  output_dir <- withr::local_tempdir()
  render(GSE74821, output_dir = output_dir)
  expect_length(
    list.files(tempdir(), pattern = "^nacho-report-", include.dirs = TRUE),
    0
  )
})
