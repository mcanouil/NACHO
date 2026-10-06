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
      expect_identical(output$preset, "nSolver preset")
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

test_that("the overview is one summary strip with labelled items", {
  html <- htmltools::renderTags(NACHO:::mod_overview_ui("overview"))$html
  expect_match(html, 'class="nacho-summary"', fixed = TRUE)
  expect_match(html, 'role="group"', fixed = TRUE)
  expect_match(html, 'aria-label="Summary of the data"', fixed = TRUE)
  for (id in c(
    "samples",
    "units",
    "flagged_count",
    "reasons",
    "method",
    "preset"
  )) {
    expect_match(html, paste0('id="overview-', id, '"'), fixed = TRUE)
  }
  expect_false(grepl("bslib-value-box", html, fixed = TRUE))
  icons <- regmatches(html, gregexpr("<i [^>]*>", html))[[1]]
  expect_gt(length(icons), 3)
  expect_true(all(grepl('aria-hidden="true"', icons, fixed = TRUE)))
})

test_that("the flagged reasons have a named trigger and hidden text", {
  html <- htmltools::renderTags(NACHO:::mod_overview_ui("overview"))$html
  expect_match(html, 'aria-label="Why samples are flagged"', fixed = TRUE)
  expect_match(html, 'class="visually-hidden"', fixed = TRUE)
})

test_that("app_overview() counts lanes on PlexSet data", {
  qc <- nacho_qc(plexset_nacho)
  qc[["lane"]] <- rep(1:12, length.out = nrow(qc))
  overview <- NACHO:::app_overview(plexset_nacho, qc)
  expect_identical(overview$unit, "Lanes")
  expect_identical(
    overview$units,
    nrow(unique(qc[c("CartridgeID", "lane")]))
  )
  expect_identical(NACHO:::app_overview(GSE74821)$unit, "Cartridges")
})

test_that("the strip wraps on a narrow screen", {
  expect_match(NACHO:::summary_rules, "flex-wrap: wrap", fixed = TRUE)
})

test_that("app_overview() does not count a missing cartridge", {
  x <- GSE74821
  x@samples[["CartridgeID"]][1:2] <- NA
  expect_identical(NACHO:::app_overview(x)$cartridges, 4L)
})

test_that("the overview names presets as the sidebar does", {
  expect_identical(NACHO:::preset_label("nsolver"), "nSolver")
  expect_identical(NACHO:::preset_label("legacy"), "NACHO 2")
})
