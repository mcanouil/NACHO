test_that("the data downloads hold the tuned object", {
  x <- flagged_gse()
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(x), quarto = FALSE),
    {
      qc <- utils::read.csv(output$qc_csv)
      expect_identical(nrow(qc), 12L)
      expect_identical(sum(qc$FoV_status == "fail"), 2L)
      thresholds <- read_thresholds(output$thresholds_yaml)
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
      thresholds <- read_thresholds(output$thresholds_yaml)
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

stub_render <- function(x, format, output_dir, title = NULL, author = NULL) {
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
    render = function(x, format, output_dir, ...) {
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
    render = function(x, format, output_dir, ...) stop("Quarto exploded."),
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
    mirai = function(.expression, ..., .compute = NULL) {
      captured <<- list(
        expression = paste(deparse(substitute(.expression)), collapse = " "),
        args = list(...),
        compute = .compute
      )
      "task"
    },
    .package = "mirai"
  )
  expect_identical(
    NACHO:::render_in_background(GSE74821, "html", "folder", "profile"),
    "task"
  )
  expect_identical(captured$compute, "profile")
  expect_match(captured$expression, ".libPaths(libs)", fixed = TRUE)
  expect_match(captured$expression, "NACHO::render", fixed = TRUE)
  expect_setequal(
    names(captured$args),
    c("object", "format", "output_dir", "title", "author", "libs")
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

test_that("the CSV export defuses tab and carriage return but not letters", {
  plain <- c("tumour_01", "t1", "r", "\\x")
  risky <- c("=1", "+1", "-1", "@x", "\tfoo", "\rbar")
  safe <- NACHO:::spreadsheet_safe(data.frame(id = c(plain, risky)))
  expect_identical(safe$id, c(plain, paste0("'", risky)))
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
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("mirai")
  pid_file <- withr::local_tempfile()
  local_mocked_bindings(
    has_package = function(package) TRUE,
    render_in_background = function(object, format, output_dir, compute, ...) {
      mirai::mirai(
        {
          writeLines(as.character(Sys.getpid()), pid_file)
          Sys.sleep(30)
        },
        pid_file = pid_file,
        .compute = compute
      )
    }
  )
  started <- Sys.time()
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(GSE74821), quarto = TRUE),
    {
      session$setInputs(format = "html", render = 1)
      for (i in 1:100) {
        if (file.exists(pid_file) && length(readLines(pid_file)) == 1) {
          break
        }
        later::run_now(0.1)
      }
      pid <- as.integer(readLines(pid_file))
      expect_true(tools::pskill(pid, 0L))
      session$close()
      for (i in 1:50) {
        if (!tools::pskill(pid, 0L)) {
          break
        }
        later::run_now(0.1)
      }
      expect_false(tools::pskill(pid, 0L))
      expect_no_condition(later::run_now(0.5))
    }
  )
  expect_lt(as.numeric(Sys.time() - started, units = "secs"), 20)
})

test_that("a render stopped after the session ends never settles", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  mirai::daemons(1, dispatcher = TRUE, .compute = "nacho-settle")
  withr::defer(mirai::daemons(0, .compute = "nacho-settle"))
  outcome <- function(ended) {
    settled <- "pending"
    running <- mirai::mirai(Sys.sleep(30), .compute = "nacho-settle")
    promises::then(
      NACHO:::settle_while_open(running, function() ended),
      onFulfilled = function(value) settled <<- "resolved",
      onRejected = function(error) settled <<- "rejected"
    )
    mirai::stop_mirai(running)
    for (i in 1:20) {
      later::run_now(0.1)
    }
    settled
  }
  expect_identical(outcome(ended = FALSE), "rejected")
  expect_identical(outcome(ended = TRUE), "pending")
})

test_that("the report card has labelled title and author fields", {
  html <- htmltools::renderTags(
    NACHO:::mod_export_ui("export", quarto = TRUE)
  )$html
  expect_match(
    html,
    '<label[^>]*for="export-title"[^>]*>Title \\(optional\\)'
  )
  expect_match(
    html,
    '<label[^>]*for="export-author"[^>]*>Author \\(optional\\)'
  )
  expect_match(
    html,
    'placeholder="NanoString quality-control report"',
    fixed = TRUE
  )
})

test_that("the render gets the title and author, blank ones as NULL", {
  seen <- list()
  local_mocked_bindings(
    has_package = function(package) package != "mirai",
    render = function(x, format, output_dir, title = NULL, author = NULL) {
      seen <<- list(title = title, author = author)
      stub_render(x, format, output_dir)
    }
  )
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(GSE74821), quarto = TRUE),
    {
      session$setInputs(
        format = "html",
        title = "Run A",
        author = "Jane Doe",
        render = 1
      )
      wait_for_task(task, session)
      expect_identical(seen, list(title = "Run A", author = "Jane Doe"))
      session$setInputs(title = "   ", author = "", render = 2)
      wait_for_task(task, session)
      expect_identical(seen, list(title = NULL, author = NULL))
    }
  )
})

test_that("the thresholds download reads back exactly", {
  x <- GSE74821
  x@thresholds <- nacho_thresholds("max", "legacy")
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(x), quarto = FALSE),
    {
      expect_identical(read_thresholds(output$thresholds_yaml), x@thresholds)
    }
  )
})

run_script <- function(lines, data_directory = NULL, sample_sheet = NULL) {
  lines <- lines[lines != "library(NACHO)"]
  if (!is.null(data_directory)) {
    lines <- sub(
      "\"path/to/rcc-files\"",
      deparse(data_directory),
      lines,
      fixed = TRUE
    )
    lines <- sub(
      "\"path/to/samplesheet.csv\"",
      deparse(sample_sheet),
      lines,
      fixed = TRUE
    )
  }
  env <- new.env(parent = parent.frame())
  suppressWarnings(suppressMessages(eval(parse(text = lines), envir = env)))
  env[["nacho"]]
}

tune_app <- function(
  x,
  method,
  background = "none",
  mode = "threshold",
  ruv_k = NULL,
  thresholds = x@thresholds
) {
  chosen <- list(
    normalisation_method = method,
    ruv_k = ruv_k,
    background = background,
    background_mode = mode
  )
  suppressWarnings(NACHO:::tune_object(x, chosen, thresholds))
}

expect_same_result <- function(script, app) {
  testthat::expect_identical(
    nacho_counts(script, normalised = TRUE),
    nacho_counts(app, normalised = TRUE)
  )
  testthat::expect_identical(nacho_qc(script), nacho_qc(app))
  testthat::expect_identical(script@thresholds, app@thresholds)
}

test_that("the R script reproduces the example data as tuned in the app", {
  thresholds <- utils::modifyList(
    GSE74821@thresholds,
    list(FoV = 95, BD = c(0.1, 2))
  )
  app <- tune_app(GSE74821, "GEO", "mean", "subtract", thresholds = thresholds)
  script <- NACHO:::reproduce_script(app)
  expect_true("nacho <- GSE74821" %in% script)
  expect_same_result(run_script(script), app)
})

test_that("the R script reproduces RUVg with its number of factors", {
  skip_if_not_installed("RUVSeq")
  app <- tune_app(GSE74821, "RUVg", ruv_k = 2L)
  expect_same_result(run_script(NACHO:::reproduce_script(app)), app)
})

test_that("the R script loads uploaded files from the paths the user sets", {
  sheet <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(
    salmon_tidy[c("IDFILE", "plexset_id")],
    sheet,
    row.names = FALSE
  )
  app <- tune_app(salmon_nacho, "GLM")
  script <- NACHO:::reproduce_script(app)
  expect_false("nacho <- GSE74821" %in% script)
  expect_true(any(grepl("load_rcc(", script, fixed = TRUE)))
  expect_same_result(run_script(script, test_path("salmon_data"), sheet), app)
})

test_that("the example data is still known after normalise() adds provenance", {
  x <- GSE74821
  x@provenance[["glm_fallback"]] <- TRUE
  expect_true("nacho <- GSE74821" %in% NACHO:::reproduce_script(x))
})

test_that("a subset of the example data is not reproduced as the example", {
  script <- NACHO:::reproduce_script(flagged_gse())
  expect_false("nacho <- GSE74821" %in% script)
  expect_true(any(grepl("load_rcc(", script, fixed = TRUE)))
})

test_that("the Export page previews and downloads the R script", {
  shiny::testServer(
    NACHO:::mod_export_server,
    args = list(object = shiny::reactiveVal(GSE74821), quarto = FALSE),
    {
      expect_identical(
        output$script,
        paste(NACHO:::reproduce_script(GSE74821), collapse = "\n")
      )
      expect_identical(
        readLines(output$script_r),
        NACHO:::reproduce_script(GSE74821)
      )
    }
  )
  html <- htmltools::renderTags(
    NACHO:::mod_export_ui("export", quarto = FALSE)
  )$html
  expect_match(html, "Reproduce in R", fixed = TRUE)
  expect_match(html, "id=\"export-script_r\"", fixed = TRUE)
})
