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

test_that("app_overview() reports exact counts for a flagged object", {
  x <- flagged_gse()
  overview <- NACHO:::app_overview(x)
  expect_identical(overview$cartridges, 1L)
  expect_identical(overview$preset, "nsolver")
  expect_identical(overview$flagged, 2L)
  expect_identical(overview$reasons, c(FoV = overview$flagged))
})

test_that("app_overview() ignores metrics without failures on PlexSet data", {
  overview <- NACHO:::app_overview(plexset_nacho)
  expect_identical(overview$samples, 96L)
  expect_identical(overview$cartridges, 1L)
  expect_identical(overview$flagged, 0L)
  expect_identical(overview$reasons, integer())
})

test_that("the overview module formats every field", {
  shiny::testServer(
    NACHO:::mod_overview_server,
    args = list(object = shiny::reactiveVal(flagged_gse())),
    {
      expect_identical(output$cartridges, "1")
      expect_identical(output$method, "GLM, nsolver preset")
      expect_match(output$flagged, "^2 \\(FoV: 2\\)$")
    }
  )
})

test_that("the overview module shows no brackets without failures", {
  shiny::testServer(
    NACHO:::mod_overview_server,
    args = list(object = shiny::reactiveVal(GSE74821)),
    {
      expect_identical(output$flagged, "0")
    }
  )
})
