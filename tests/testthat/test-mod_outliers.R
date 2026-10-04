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
    }
  )
})
