test_that("the data downloads hold the tuned object", {
  x <- flagged_gse()
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(x), quarto = FALSE),
    {
      qc <- utils::read.csv(output$qc_csv)
      expect_identical(nrow(qc), 12L)
      thresholds <- yaml::read_yaml(output$thresholds_yaml)
      expect_identical(thresholds$FoV, 99.9)
      expect_identical(thresholds$preset, "nsolver")
      expect_identical(readRDS(output$object_rds), x)
    }
  )
})

test_that("open bounds survive the YAML export", {
  x <- GSE74821
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(x), quarto = FALSE),
    {
      thresholds <- yaml::read_yaml(output$thresholds_yaml)
      expect_identical(thresholds$Ligation_NEG, c(-Inf, 0))
    }
  )
})

test_that("the report button hides without Quarto", {
  html <- htmltools::renderTags(NACHO:::mod_export_ui(
    "export",
    quarto = FALSE
  ))$html
  expect_false(grepl("export-render", html, fixed = TRUE))
  expect_match(html, "Quarto", fixed = TRUE)
})

test_that("the report renders in a task and offers a download", {
  withr::defer(unlink(
    list.files(tempdir(), "^nacho-report-", full.names = TRUE),
    recursive = TRUE
  ))
  local_mocked_bindings(
    has_package = function(package) package != "mirai",
    render = function(x, format, output_dir) {
      dir.create(output_dir, recursive = TRUE)
      path <- file.path(output_dir, "nacho-report.html")
      writeLines("<html></html>", path)
      path
    }
  )
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(GSE74821), quarto = TRUE),
    {
      session$setInputs(format = "html", render = 1)
      session$flushReact()
      for (i in 1:50) {
        if (identical(task$status(), "success")) {
          break
        }
        later::run_now(0.1)
        session$flushReact()
      }
      expect_identical(basename(output$report), "nacho-report.html")
    }
  )
})
