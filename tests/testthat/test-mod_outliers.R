local_no_reactable <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    has_package = function(package) package != "reactable",
    .package = "NACHO",
    .env = env
  )
}

test_that("the outliers module lists failing samples only", {
  local_no_reactable()
  x <- flagged_gse()
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(x),
      qc = shiny::reactive(nacho_qc(x))
    ),
    {
      expect_identical(failures(), NACHO:::qc_failures(x))
      expect_match(output$samples, "FoV")
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
      expect_identical(output$message, "No sample is flagged.")
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
      expect_identical(output$message, "")
    }
  )
})

test_that("the keyboard route highlights a sample", {
  local_no_reactable()
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
  local_no_reactable()
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
  local_no_reactable()
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

test_that("the samples table lists flagged samples first", {
  x <- flagged_gse()
  qc <- nacho_qc(x)
  data <- NACHO:::samples_table_data(x, qc)
  id <- x@settings[["id_colname"]]
  expect_identical(names(data), NACHO:::qc_table_columns(x, qc))
  flagged <- qc[[id]][qc[["status"]] %in% "fail"]
  expect_setequal(data[[id]][seq_along(flagged)], flagged)
  expect_identical(nrow(data), nrow(qc))
  expect_false(is.unsorted(rev(data[["n_flags"]][seq_along(flagged)])))
})

test_that("the flagged only switch keeps the flagged samples", {
  local_no_reactable()
  x <- flagged_gse()
  table <- nacho_qc(x)
  id <- x@settings[["id_colname"]]
  passed <- table[[id]][table[["status"]] %in% "pass"][1]
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(object = shiny::reactiveVal(x), qc = shiny::reactive(table)),
    {
      session$setInputs(flagged_only = FALSE)
      expect_match(output$samples, passed, fixed = TRUE)
      session$setInputs(flagged_only = TRUE)
      expect_no_match(output$samples, passed, fixed = TRUE)
      expect_identical(
        lengths(regmatches(output$samples, gregexpr("<tr>", output$samples))),
        sum(table[["status"]] %in% "fail") + 1L
      )
    }
  )
})

test_that("the status reads as a coloured word", {
  expect_identical(
    NACHO:::status_badge(c("fail", "pass", NA)),
    c(
      '<span class="badge text-bg-danger">fail</span>',
      '<span class="badge text-bg-success">pass</span>',
      '<span class="badge text-bg-secondary">not assessed</span>'
    )
  )
})

test_that("choosing a row of the reactable table selects the sample", {
  skip_if_not_installed("reactable")
  x <- flagged_gse()
  qc <- nacho_qc(x)
  selected <- shiny::reactiveVal(character())
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(x),
      qc = shiny::reactive(qc),
      selected = selected
    ),
    {
      session$setInputs(flagged_only = FALSE)
      session$setInputs(samples__reactable__selected = 2L)
      expect_identical(selected(), rows()[[1]][2])
    }
  )
})

test_that("a sample chosen elsewhere is selected in the reactable table", {
  skip_if_not_installed("reactable")
  sent <- list()
  local_mocked_bindings(
    updateReactable = function(outputId, ..., selected = NULL, session = NULL) {
      sent[[length(sent) + 1L]] <<- selected
    },
    .package = "reactable"
  )
  x <- flagged_gse()
  qc <- nacho_qc(x)
  selected <- shiny::reactiveVal(character())
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(x),
      qc = shiny::reactive(qc),
      selected = selected
    ),
    {
      session$setInputs(flagged_only = FALSE)
      selected(rows()[[1]][3])
      session$flushReact()
      expect_identical(sent[[length(sent)]], 3L)
      session$setInputs(samples__reactable__selected = 3L)
      selected(character())
      session$flushReact()
      expect_identical(sent[[length(sent)]], NA)
      session$setInputs(samples__reactable__selected = NULL)
      count <- length(sent)
      session$setInputs(samples__reactable__selected = 2L)
      expect_identical(selected(), rows()[[1]][2])
      expect_length(sent, count)
    }
  )
})

test_that("the samples card holds a reactable table only when it is installed", {
  local_no_reactable()
  html <- htmltools::renderTags(NACHO:::mod_outliers_ui("o"))$html
  expect_match(html, "id=\"o-flagged_only\"", fixed = TRUE)
  expect_no_match(html, "reactable", fixed = TRUE)
  skip_if_not_installed("reactable")
  local_mocked_bindings(has_package = function(package) TRUE)
  html <- htmltools::renderTags(NACHO:::mod_outliers_ui("o"))$html
  expect_match(html, "reactable", fixed = TRUE)
})

test_that("deselecting the row of a visible sample clears the selection", {
  skip_if_not_installed("reactable")
  x <- flagged_gse()
  table <- nacho_qc(x)
  selected <- shiny::reactiveVal(character())
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(x),
      qc = shiny::reactive(table),
      selected = selected
    ),
    {
      session$setInputs(flagged_only = FALSE)
      passed <- rows()[[1]][nrow(rows())]
      session$setInputs(samples__reactable__selected = nrow(rows()))
      expect_identical(selected(), passed)
      session$setInputs(samples__reactable__selected = NULL)
      expect_identical(selected(), character())
      session$setInputs(samples__reactable__selected = nrow(rows()))
      session$setInputs(flagged_only = TRUE)
      session$setInputs(samples__reactable__selected = NULL)
      expect_identical(selected(), passed)
    }
  )
})

test_that("the reactable table shows the labels and the status badges", {
  skip_if_not_installed("reactable")
  for (x in list(flagged_gse(), plexset_nacho)) {
    table <- nacho_qc(x)
    shiny::testServer(
      NACHO:::mod_outliers_server,
      args = list(object = shiny::reactiveVal(x), qc = shiny::reactive(table)),
      {
        session$setInputs(flagged_only = FALSE)
        widget <- paste(unlist(output$samples), collapse = "")
        expect_match(widget, "Cartridge", fixed = TRUE)
        expect_match(widget, "badge text-bg-", fixed = TRUE)
        if ("lane_status" %in% names(table)) {
          expect_match(widget, "Lane status", fixed = TRUE)
        }
      }
    )
  }
})

test_that("the plain table renders empty when no sample is flagged", {
  local_no_reactable()
  expect_identical(NACHO:::status_badge(character()), character())
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(GSE74821),
      qc = shiny::reactive(nacho_qc(GSE74821))
    ),
    {
      session$setInputs(flagged_only = TRUE)
      expect_match(output$samples, "<table", fixed = TRUE)
    }
  )
})

test_that("a row index past the end of the table is ignored", {
  skip_if_not_installed("reactable")
  x <- flagged_gse()
  table <- nacho_qc(x)
  selected <- shiny::reactiveVal(character())
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(x),
      qc = shiny::reactive(table),
      selected = selected
    ),
    {
      session$setInputs(flagged_only = TRUE)
      session$setInputs(samples__reactable__selected = nrow(rows()) + 5L)
      expect_identical(selected(), character())
      session$setInputs(samples__reactable__selected = 1L)
      chosen <- selected()
      session$setInputs(samples__reactable__selected = nrow(rows()) + 5L)
      expect_identical(selected(), chosen)
    }
  )
})

test_that("the reactable table renders empty when no sample is flagged", {
  skip_if_not_installed("reactable")
  shiny::testServer(
    NACHO:::mod_outliers_server,
    args = list(
      object = shiny::reactiveVal(GSE74821),
      qc = shiny::reactive(nacho_qc(GSE74821))
    ),
    {
      session$setInputs(flagged_only = TRUE)
      widget <- paste(unlist(output$samples), collapse = "")
      expect_match(widget, "\"name\":\"Reactable\"", fixed = TRUE)
      expect_match(widget, "\"IDFILE\":[]", fixed = TRUE)
    }
  )
})
