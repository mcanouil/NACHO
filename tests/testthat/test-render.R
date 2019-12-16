test_that("Default parameters", {
  if (rmarkdown::pandoc_available()) {
    out <- render(nacho_object = GSE74821, clean = FALSE)
    expect_null(out)
  } else {
    expect_error(render(nacho_object = GSE74821))
  }
})

test_that("with legend", {
  if (rmarkdown::pandoc_available()) {
    out <- render(nacho_object = GSE74821, show_legend = TRUE)
    expect_null(out)
  } else {
    expect_error(render(nacho_object = GSE74821))
  }
})

test_that("with Rmd", {
  if (rmarkdown::pandoc_available()) {
    out <- render(nacho_object = GSE74821, clean = FALSE)
    expect_null(out)
  } else {
    expect_error(render(nacho_object = GSE74821))
  }
})
