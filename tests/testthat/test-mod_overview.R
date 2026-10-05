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
    args = list(
      object = shiny::reactiveVal(flagged_gse()),
      qc = shiny::reactive(nacho_qc(flagged_gse()))
    ),
    {
      expect_identical(output$samples, "12")
      expect_match(output$flagged_count, "^[1-9]")
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
    args = list(
      object = shiny::reactiveVal(flagged_gse()),
      qc = shiny::reactive(nacho_qc(flagged_gse()))
    ),
    {
      expect_identical(output$cartridges, "1")
      expect_identical(output$method, "GLM")
      expect_identical(output$preset, "nsolver preset")
      expect_identical(output$flagged_count, "2")
      expect_identical(output$reasons, "FoV: 2")
    }
  )
})

test_that("the overview module shows no reasons without failures", {
  shiny::testServer(
    NACHO:::mod_overview_server,
    args = list(
      object = shiny::reactiveVal(GSE74821),
      qc = shiny::reactive(nacho_qc(GSE74821))
    ),
    {
      expect_identical(output$flagged_count, "0")
      expect_identical(output$reasons, "None")
    }
  )
})

test_that("the overview is four value boxes with decorative icons", {
  html <- htmltools::renderTags(NACHO:::mod_overview_ui("overview"))$html
  boxes <- gregexpr('class="[^"]*\\bbslib-value-box( |")', html)
  expect_length(regmatches(html, boxes)[[1]], 4L)
  icons <- regmatches(html, gregexpr("<i [^>]*>", html))[[1]]
  expect_length(icons, 4L)
  expect_true(all(grepl('aria-hidden="true"', icons, fixed = TRUE)))
})

test_that("app_overview() does not count a missing cartridge", {
  x <- GSE74821
  x@samples[["CartridgeID"]][1:2] <- NA
  expect_identical(NACHO:::app_overview(x)$cartridges, 4L)
})
