test_that("the tour dependencies point at files of the package", {
  for (dependency in NACHO:::tour_dependencies()) {
    directory <- system.file(dependency$src$file, package = dependency$package)
    files <- c(dependency$script, dependency$stylesheet)
    expect_true(
      all(file.exists(file.path(directory, files))),
      info = dependency$name
    )
  }
})

test_that("the vendored driver.js files name their version and licence", {
  version <- NACHO:::driver_dependency()$version
  for (file in c("driver.js", "driver.css")) {
    header <- readLines(
      system.file("tour", file, package = "NACHO"),
      n = 12,
      warn = FALSE
    )
    expect_true(
      any(grepl(paste("driver.js", version), header, fixed = TRUE)),
      info = file
    )
    expect_true(any(grepl("MIT licence", header, fixed = TRUE)), info = file)
    expect_true(
      any(grepl("github.com/kamranahmedse/driver.js", header, fixed = TRUE)),
      info = file
    )
  }
  copyrights <- readLines(system.file("COPYRIGHTS", package = "NACHO"))
  expect_true(any(grepl("driver.js 1.9.0", copyrights, fixed = TRUE)))
  expect_true(any(grepl(
    "Copyright (c) Kamran Ahmed",
    copyrights,
    fixed = TRUE
  )))
})
