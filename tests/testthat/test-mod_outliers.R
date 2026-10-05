test_that("the outliers module lists failing samples only", {
  x <- flagged_gse()
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(x),
      qc = shiny::reactive(nacho_qc(x))
    ),
    {
      expect_identical(failures(), NACHO:::qc_failures(x))
      expect_match(output$failures, "FoV")
    }
  )
})

test_that("the outliers module returns the failing ids only", {
  x <- flagged_gse()
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(x),
      qc = shiny::reactive(nacho_qc(x))
    ),
    {
      qc <- NACHO::nacho_qc(x)
      expect_identical(
        failures()[[x@settings[["id_colname"]]]],
        qc[[x@settings[["id_colname"]]]][qc$status %in% "fail"]
      )
      expect_identical(nrow(failures()), 2L)
    }
  )
})

test_that("the outliers module returns zero rows without failures", {
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(GSE74821),
      qc = shiny::reactive(nacho_qc(GSE74821))
    ),
    {
      expect_identical(nrow(failures()), 0L)
      expect_match(
        as.character(output$body$html),
        "No sample is flagged.",
        fixed = TRUE
      )
    }
  )
})

test_that("the outliers module shows the table when samples are flagged", {
  x <- flagged_gse()
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(x),
      qc = shiny::reactive(nacho_qc(x))
    ),
    {
      body <- as.character(output$body$html)
      expect_no_match(body, "No sample is flagged.", fixed = TRUE)
      expect_match(body, "failures", fixed = TRUE)
    }
  )
})

test_that("the keyboard route highlights a sample", {
  selected <- shiny::reactiveVal(character())
  x <- flagged_gse()
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(x),
      qc = shiny::reactive(nacho_qc(x)),
      selected = selected
    ),
    {
      id <- colnames(x@counts)[2]
      session$setInputs(highlight = id)
      expect_identical(selected(), id)
      expect_match(
        output$samples,
        paste0("<strong>", id, "</strong>"),
        fixed = TRUE
      )
      session$setInputs(highlight = "")
      expect_identical(selected(), character())
    }
  )
  html <- htmltools::renderTags(NACHO:::mod_outliers_ui("outliers"))$html
  expect_match(html, "<select[^>]*id=\"outliers-highlight\"")
  expect_match(
    html,
    "<label[^>]*for=\"outliers-highlight\"[^>]*>Highlight a sample",
    perl = TRUE
  )
})
