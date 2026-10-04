test_that("app_overview() counts samples, cartridges and reasons", {
  x <- flagged_gse()
  overview <- NACHO:::app_overview(x)
  expect_identical(overview$samples, 12L)
  expect_identical(overview$flagged, sum(NACHO:::flagged_samples(x)))
  expect_identical(names(overview$reasons), "FoV")
  expect_identical(overview$method, "GLM")
  expect_identical(NACHO:::app_overview(GSE74821)$reasons, integer())
})

test_that("the overview module shows the counts", {
  shiny::testServer(
    NACHO:::mod_overview_server,
    args = list(object = shiny::reactiveVal(flagged_gse())),
    {
      expect_identical(output$samples, "12")
      expect_match(output$flagged, "^[1-9]")
    }
  )
})
