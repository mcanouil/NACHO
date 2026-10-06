test_that("the data downloads hold the tuned object", {
  x <- flagged_gse()
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(x), quarto = FALSE),
    {
      qc <- utils::read.csv(output$qc_csv)
      expect_identical(nrow(qc), 12L)
      expect_identical(sum(qc$FoV_status == "fail"), 2L)
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

test_that("the report card says when rendering blocks the app", {
  ui <- function() {
    htmltools::renderTags(NACHO:::mod_export_ui("export", quarto = TRUE))$html
  }
  local_mocked_bindings(has_package = function(package) FALSE)
  expect_match(ui(), "install mirai to render in the background", fixed = TRUE)
  local_mocked_bindings(has_package = function(package) TRUE)
  expect_no_match(ui(), "install mirai", fixed = TRUE)
})

stub_render <- function(x, format, output_dir) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(output_dir, "nacho-report.html")
  writeLines(format(Sys.time(), "%H:%M:%OS6"), path)
  path
}

wait_for_task <- function(task, session, status = "success") {
  for (i in 1:50) {
    session$flushReact()
    if (identical(task$status(), status)) {
      break
    }
    later::run_now(0.1)
  }
}

test_that("the report renders in a task and offers a download", {
  local_mocked_bindings(
    has_package = function(package) package != "mirai",
    render = stub_render
  )
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(GSE74821), quarto = TRUE),
    {
      session$setInputs(format = "html", render = 1)
      wait_for_task(task, session)
      expect_identical(basename(output$report), "nacho-report.html")
    }
  )
})

test_that("changing the object withdraws the report", {
  local_mocked_bindings(
    has_package = function(package) package != "mirai",
    render = stub_render
  )
  object <- shiny::reactiveVal(GSE74821)
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = object, quarto = TRUE),
    {
      session$setInputs(format = "html", render = 1)
      wait_for_task(task, session)
      expect_false(is.null(report_path()))
      withdrawn <- report_path()
      object(flagged_gse())
      session$flushReact()
      expect_null(report_path())
      expect_false(file.exists(withdrawn))
      expect_error(output$report)
      session$setInputs(render = 2)
      wait_for_task(task, session)
      expect_false(is.null(report_path()))
    }
  )
})

test_that("one report folder per session holds one report and goes away", {
  local_mocked_bindings(
    has_package = function(package) package != "mirai",
    render = stub_render
  )
  folder <- NULL
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(GSE74821), quarto = TRUE),
    {
      session$setInputs(format = "html", render = 1)
      wait_for_task(task, session)
      folder <<- dirname(report_path())
      session$setInputs(render = 2)
      wait_for_task(task, session)
      expect_identical(dirname(report_path()), folder)
      expect_length(list.files(folder), 1L)
      session$close()
    }
  )
  expect_false(dir.exists(folder))
})

test_that("a report finished for an old object is deleted", {
  object <- shiny::reactiveVal(GSE74821)
  finished <- NULL
  local_mocked_bindings(
    has_package = function(package) package != "mirai",
    render = function(x, format, output_dir) {
      object(flagged_gse())
      finished <<- stub_render(x, format, output_dir)
    }
  )
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = object, quarto = TRUE),
    {
      session$setInputs(format = "html", render = 1)
      wait_for_task(task, session)
      session$flushReact()
      expect_false(is.null(finished))
      expect_false(file.exists(finished))
      expect_null(report_path())
    }
  )
})

test_that("a failed render reaches the user as an error toast", {
  toasts <- list()
  local_mocked_bindings(
    has_package = function(package) package != "mirai",
    render = function(x, format, output_dir) stop("Quarto exploded."),
    notify_user = function(message, type) {
      toasts[[length(toasts) + 1L]] <<- list(message = message, type = type)
    }
  )
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(GSE74821), quarto = TRUE),
    {
      expect_warning(
        {
          session$setInputs(format = "html", render = 1)
          wait_for_task(task, session, "error")
        },
        "Quarto exploded."
      )
      session$flushReact()
      expect_length(toasts, 1L)
      expect_identical(toasts[[1]]$type, "error")
      expect_match(toasts[[1]]$message, "Quarto exploded.", fixed = TRUE)
    }
  )
})

test_that("the background render hands the library paths to the daemon", {
  skip_if_not_installed("mirai")
  captured <- NULL
  local_mocked_bindings(has_package = function(package) TRUE)
  local_mocked_bindings(
    mirai = function(.expression, ...) {
      captured <<- list(
        expression = paste(deparse(substitute(.expression)), collapse = " "),
        args = list(...)
      )
      "task"
    },
    .package = "mirai"
  )
  expect_identical(
    NACHO:::render_in_background(GSE74821, "html", "folder"),
    "task"
  )
  expect_match(captured$expression, ".libPaths(libs)", fixed = TRUE)
  expect_match(captured$expression, "NACHO::render", fixed = TRUE)
  expect_setequal(
    names(captured$args),
    c("object", "format", "output_dir", "libs")
  )
  expect_identical(captured$args$libs, .libPaths())
})

test_that("the CSV export defuses spreadsheet formulas", {
  x <- flagged_gse()
  id <- x@settings[["id_colname"]]
  qc <- nacho_qc(x)
  qc[[id]][1:4] <- c("=1+1", "+a", "-b", "@c")
  safe <- NACHO:::spreadsheet_safe(qc)
  expect_identical(safe[[id]][1:4], c("'=1+1", "'+a", "'-b", "'@c"))
  expect_identical(safe[[id]][5:12], qc[[id]][5:12])
  expect_identical(safe$FoV, qc$FoV)
})

test_that("the background render uses an installed NACHO with the new render", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  libs <- .libPaths()
  daemon <- mirai::mirai(
    {
      .libPaths(libs)
      names(formals(NACHO::render))
    },
    libs = libs
  )
  expect_true("format" %in% daemon[])
})

test_that("ending the session stops a running background render", {
  skip_if_not_installed("mirai")
  stopped <- 0L
  local_mocked_bindings(
    has_package = function(package) TRUE,
    render_in_background = function(object, format, output_dir) {
      mirai::mirai(Sys.sleep(30))
    }
  )
  local_mocked_bindings(
    stop_mirai = function(...) stopped <<- stopped + 1L,
    .package = "mirai"
  )
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(GSE74821), quarto = TRUE),
    {
      session$setInputs(format = "html", render = 1)
      session$flushReact()
      session$close()
    }
  )
  expect_identical(stopped, 1L)
})
