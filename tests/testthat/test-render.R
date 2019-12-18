test_that("Default parameters", {
  if (rmarkdown::pandoc_available()) {
    expect_null(render(nacho_object = GSE74821, clean = FALSE))
  } else {
    expect_error(render(nacho_object = GSE74821))
  }
})

test_that("with legend", {
  if (rmarkdown::pandoc_available()) {
    expect_null(render(nacho_object = GSE74821, show_legend = TRUE))
  } else {
    expect_error(render(nacho_object = GSE74821))
  }
})

test_that("with Rmd", {
  if (rmarkdown::pandoc_available()) {
    expect_null(render(nacho_object = GSE74821, clean = FALSE))
  } else {
    expect_error(render(nacho_object = GSE74821))
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
    expect_error(render(nacho_object = GSE74821, clean = FALSE))
  } else {
    expect_error(render(nacho_object = GSE74821))
  }
})