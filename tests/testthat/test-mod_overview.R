test_that("app_overview() counts samples, units and reasons", {
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
  expect_identical(overview$unit, "Cartridges")
  expect_identical(overview$units, 1L)
  expect_identical(overview$preset, "nsolver")
  expect_identical(overview$flagged, 2L)
  expect_identical(overview$reasons, c(FoV = overview$flagged))
})

test_that("app_overview() ignores metrics without failures on PlexSet data", {
  overview <- NACHO:::app_overview(plexset_nacho)
  expect_identical(overview$samples, 96L)
  expect_identical(overview$units, 12L)
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
      expect_identical(output$units, "1")
      expect_identical(output$method, "GLM")
      expect_identical(output$preset, "nSolver preset")
      expect_identical(output$flagged_count, "2")
      expect_identical(output$reasons, "Flagged because: FoV: 2")
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
      expect_identical(output$reasons, "No sample is flagged.")
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
  expect_length(icons, 5L)
  expect_true(all(grepl('aria-hidden="true"', icons, fixed = TRUE)))
  expect_false(any(grepl("aria-label", icons, fixed = TRUE)))
})

test_that("the flagged reasons are part of the trigger's name", {
  html <- htmltools::renderTags(NACHO:::mod_overview_ui("overview"))$html
  button <- regmatches(
    html,
    regexpr("(?s)<button[^>]*nacho-summary-info.*?</button>", html, perl = TRUE)
  )
  expect_length(button, 1L)
  expect_no_match(button, "aria-label|aria-describedby", perl = TRUE)
  expect_match(
    button,
    paste0(
      '(?s)<span class="visually-hidden">\\s*Why samples are flagged\\.',
      '\\s*<[^>]* id="overview-reasons"'
    ),
    perl = TRUE
  )
})

test_that("app_overview() counts lanes on PlexSet data", {
  qc <- nacho_qc(plexset_nacho)
  overview <- NACHO:::app_overview(plexset_nacho, qc)
  expect_identical(overview$unit, "Lanes")
  expect_identical(
    overview$units,
    nrow(unique(qc[c("CartridgeID", "lane")]))
  )
  expect_identical(NACHO:::app_overview(GSE74821)$unit, "Cartridges")
})

test_that("app_overview() does not count a missing cartridge", {
  x <- GSE74821
  x@samples[["CartridgeID"]][1:2] <- NA
  expect_identical(NACHO:::app_overview(x)$units, 4L)
})

test_that("the overview names presets as the sidebar does", {
  expect_identical(NACHO:::preset_label("nsolver"), "nSolver")
  expect_identical(NACHO:::preset_label("legacy"), "NACHO 2")
})
