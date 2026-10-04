test_that("the outliers module lists failing samples only", {
  x <- flagged_gse()
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(object = shiny::reactiveVal(x)),
    {
      expect_identical(failures(), NACHO:::qc_failures(x))
      expect_match(output$failures, "FoV")
    }
  )
})
