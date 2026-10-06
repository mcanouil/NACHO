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

test_that("the samples table escapes cell text", {
  x <- flagged_gse()
  selected <- shiny::reactiveVal(character())
  qc <- nacho_qc(x)
  qc[[x@settings[["id_colname"]]]][2] <- "a<b>&"
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(x),
      qc = shiny::reactive(qc),
      selected = selected
    ),
    {
      session$flushReact()
      selected("a<b>&")
      session$flushReact()
      expect_match(
        output$samples,
        "<strong>a&lt;b&gt;&amp;</strong>",
        fixed = TRUE
      )
      expect_no_match(output$samples, "a<b>", fixed = TRUE)
    }
  )
})

test_that("the samples table escapes the id column name", {
  x <- flagged_gse()
  samples <- x@samples
  names(samples)[names(samples) == x@settings[["id_colname"]]] <- "a<b"
  settings <- x@settings
  settings[["id_colname"]] <- "a<b"
  x <- S7::set_props(x, samples = samples, settings = settings)
  qc <- nacho_qc(x)
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(object = shiny::reactiveVal(x), qc = shiny::reactive(qc)),
    {
      session$flushReact()
      expect_match(output$samples, "a&lt;b", fixed = TRUE)
      expect_no_match(output$samples, "<th>a<b", fixed = TRUE)
    }
  )
})

test_that("the select follows the selection and the object", {
  x <- flagged_gse()
  object <- shiny::reactiveVal(x)
  selected <- shiny::reactiveVal(character())
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = object,
      qc = shiny::reactive(nacho_qc(object())),
      selected = selected
    ),
    {
      id <- colnames(x@counts)[2]
      selected(id)
      session$flushReact()
      expect_identical(selected(), id)
      object(plexset_nacho)
      session$flushReact()
      expect_identical(selected(), character())
    }
  )
})

test_that("the hint is tied to the select and static apps omit the plot claim", {
  html <- htmltools::renderTags(NACHO:::mod_outliers_ui("o"))$html
  expect_match(html, "aria-describedby=\"o-highlight-hint\"", fixed = TRUE)
  expect_no_match(html, "outlined in the plots", fixed = TRUE)
  html <- htmltools::renderTags(NACHO:::mod_outliers_ui("o", TRUE))$html
  expect_match(html, "outlined in the plots", fixed = TRUE)
})

test_that("an empty selection sends an empty string to the select", {
  x <- flagged_gse()
  sent <- list()
  local_mocked_bindings(
    updateSelectInput = function(session, inputId, ..., selected = NULL) {
      sent[[length(sent) + 1L]] <<- selected
    },
    .package = "shiny"
  )
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(x),
      qc = shiny::reactive(nacho_qc(x))
    ),
    {
      session$flushReact()
      expect_identical(sent[[1]], "")
    }
  )
})
