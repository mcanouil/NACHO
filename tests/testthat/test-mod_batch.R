test_that("the batch page shows the design before the plots", {
  html <- htmltools::renderTags(NACHO:::mod_batch_ui("batch"))$html
  design <- regexpr("batch-design", html, fixed = TRUE)
  plot <- regexpr("BatchFactors", html, fixed = TRUE)
  expect_lt(design, plot)
})

test_that("the design needs a group and flags confounding", {
  shiny::testServer(
    NACHO:::mod_batch_server,
    args = list(object = shiny::reactiveVal(GSE74821)),
    {
      session$setInputs(group = "")
      expect_match(output$design_note, "Choose the column")
      session$setInputs(group = "tissue type:ch1")
      expect_true(all(design()$confounded))
    }
  )
})

test_that("the design card works when the samples have no Date", {
  x <- GSE74821
  x@samples[["Date"]] <- NULL
  shiny::testServer(
    NACHO:::mod_batch_server,
    args = list(object = shiny::reactiveVal(x)),
    {
      session$setInputs(group = "tissue type:ch1")
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
