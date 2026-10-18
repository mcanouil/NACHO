test_that("the batch page shows the design before the plots", {
  html <- htmltools::renderTags(NACHO:::mod_batch_ui("batch"))$html
  design <- regexpr("batch-design", html, fixed = TRUE)
  plot <- regexpr("BatchFactors", html, fixed = TRUE)
  expect_lt(design, plot)
})

with_groups <- function(x) {
  x@samples[["biology"]] <- rep(c("a", "b"), length.out = nrow(x@samples))
  x@samples[["by cartridge"]] <- paste0(
    "g",
    match(x@samples[["CartridgeID"]], unique(x@samples[["CartridgeID"]])) %% 2
  )
  x
}

test_that("only text columns with a few levels are offered as groups", {
  samples <- nacho_samples(with_groups(GSE74821))
  choices <- NACHO:::group_choices(samples)
  expect_true(all(c("biology", "by cartridge") %in% choices))
  expect_false(any(c("BD", "PC01", "title") %in% choices))
  expect_false("tissue type:ch1" %in% choices)
})

test_that("the design needs a group and flags confounding", {
  shiny::testServer(
    NACHO:::mod_batch_server,
    args = list(object = shiny::reactiveVal(with_groups(GSE74821))),
    {
      session$setInputs(group = "")
      expect_match(output$design_note, "Choose the column")
      session$setInputs(group = "by cartridge")
      expect_true(any(design()$confounded))
      session$setInputs(group = "biology")
      expect_false(any(design()$confounded))
    }
  )
})

test_that("the design card works when the samples have no Date", {
  x <- with_groups(GSE74821)
  x@samples[["Date"]] <- NULL
  shiny::testServer(
    NACHO:::mod_batch_server,
    args = list(object = shiny::reactiveVal(x)),
    {
      session$setInputs(group = "biology")
      expect_identical(design()$batch, "CartridgeID")
      expect_match(output$table_CartridgeID, "<table", fixed = TRUE)
      expect_match(
        as.character(output$crosstab_Date$html),
        "Date is not in these data.",
        fixed = TRUE
      )
    }
  )
})

test_that("the design note says when there is no batch column", {
  x <- with_groups(GSE74821)
  x@samples[["CartridgeID"]] <- NULL
  x@samples[["Date"]] <- NULL
  shiny::testServer(
    NACHO:::mod_batch_server,
    args = list(object = shiny::reactiveVal(x)),
    {
      session$setInputs(group = "biology")
      expect_match(output$design_note, "no CartridgeID or Date column")
    }
  )
})

test_that("the batch columns are not offered as groups", {
  samples <- nacho_samples(with_groups(GSE74821))
  expect_true(all(c("CartridgeID", "Date") %in% names(samples)))
  choices <- NACHO:::group_choices(samples)
  expect_false(any(c("CartridgeID", "Date") %in% choices))
})

test_that("renamed copies of a batch column are not offered as groups", {
  x <- with_groups(GSE74821)
  x@samples[["scanner copy"]] <- paste0("c", x@samples[["CartridgeID"]])
  x@samples[["day copy"]] <- paste0("d", x@samples[["Date"]])
  choices <- NACHO:::group_choices(nacho_samples(x))
  expect_false(any(c("scanner copy", "day copy") %in% choices))
  expect_true("biology" %in% choices)
})
