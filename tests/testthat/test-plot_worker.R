test_that("plot_cache_key() changes with any input", {
  size <- c(width = 5, height = 3.5)
  key <- function(type = "BD", options = list(size = 1), dark = FALSE) {
    NACHO:::plot_cache_key(GSE74821, type, options, dark, size)
  }
  base <- key()
  expect_type(base, "character")
  expect_identical(base, key())
  expect_false(identical(base, key(type = "FoV")))
  expect_false(identical(base, key(options = list(size = 2))))
  expect_false(identical(base, key(dark = TRUE)))
  expect_false(identical(
    base,
    NACHO:::plot_cache_key(
      GSE74821,
      "BD",
      list(size = 1),
      FALSE,
      c(width = 6, height = 3.5)
    )
  ))
})

test_that("plot_worker_count() keeps a core free and uses at most four", {
  expect_identical(NACHO:::plot_worker_count(10L), 4L)
  expect_identical(NACHO:::plot_worker_count(3L), 2L)
  expect_identical(NACHO:::plot_worker_count(1L), 1L)
  expect_identical(NACHO:::plot_worker_count(NA_integer_), 1L)
})

test_that("build_card() gives a ggplot for a static card", {
  plot <- NACHO:::build_card(
    GSE74821,
    "BD",
    list(colour = "CartridgeID"),
    FALSE,
    FALSE,
    c(width = 5, height = 3.5)
  )
  expect_s3_class(plot, "ggplot")
})

test_that("build_card() gives the same widget in a worker and in process", {
  skip_if_not_installed("ggiraph")
  skip_if_not_installed("mirai")
  args <- list(
    GSE74821,
    "BD",
    list(colour = "CartridgeID", show_legend = TRUE, size = 1),
    FALSE,
    TRUE,
    c(width = 5, height = 3.5)
  )
  local <- withr::with_seed(
    1,
    do.call(NACHO:::build_card, args),
    .rng_kind = "Mersenne-Twister",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )
  mirai::daemons(1, .compute = "nacho-test")
  on.exit(mirai::daemons(0, .compute = "nacho-test"), add = TRUE)
  remote <- mirai::mirai(
    {
      .libPaths(libs)
      set.seed(1, "Mersenne-Twister", "Inversion", "Rejection")
      do.call(NACHO:::build_card, args)
    },
    args = args,
    libs = .libPaths(),
    .compute = "nacho-test"
  )[]
  strip_ids <- function(html) gsub("svg_[0-9a-f]+", "svg", html)
  expect_identical(strip_ids(remote$x$html), strip_ids(local$x$html))
})

test_that("the worker pool starts once and stops with the session", {
  skip_if_not_installed("mirai")
  session <- shiny::MockShinySession$new()
  profile <- NACHO:::plot_workers_start(session)
  on.exit(mirai::daemons(0, .compute = profile), add = TRUE)
  expect_identical(NACHO:::plot_workers_start(session), profile)
  expect_true(mirai::mirai(TRUE, .compute = profile)[])
  expect_gt(mirai::status(.compute = profile)$connections, 0)
  session$close()
  expect_null(session$userData$plot_workers)
  expect_identical(mirai::status(.compute = profile)$connections, 0L)
})

test_that("without mirai the app starts no workers", {
  local_mocked_bindings(has_package = function(package) FALSE)
  session <- shiny::MockShinySession$new()
  expect_null(NACHO:::plot_workers_start(session))
  expect_null(session$userData$plot_workers)
})

test_that("a card builds in a worker only while its page shows", {
  skip_if_not_installed("ggiraph")
  skip_if_not_installed("mirai")
  skip_if_not_installed("later")
  root <- shiny::MockShinySession$new()
  root$clientData <- shiny::reactiveValues(
    `output_BD-girafe_width` = 600,
    `output_BD-girafe_height` = 350
  )
  workers <- NACHO:::plot_workers_start(root)
  on.exit(root$close(), add = TRUE)
  active <- shiny::reactiveVal(FALSE)
  settle <- function(done, seconds = 60) {
    deadline <- Sys.time() + seconds
    while (!done() && Sys.time() < deadline) {
      later::run_now(0.1)
      root$flushReact()
    }
    done()
  }
  shiny::testServer(
    NACHO:::mod_qc_plot_server,
    args = list(
      id = "BD",
      object = shiny::reactiveVal(GSE74821),
      qc = shiny::reactive(nacho_qc(GSE74821)),
      type = "BD",
      dark = shiny::reactive(FALSE),
      interactive = TRUE,
      active = active,
      workers = workers
    ),
    session = root,
    {
      session$setInputs(colour = "CartridgeID")
      session$elapse(300)
      settle(function() FALSE, seconds = 1)
      expect_length(session$cache$keys(), 0L)
      active(TRUE)
      expect_true(settle(function() length(session$cache$keys()) == 1L))
      widget <- jsonlite::fromJSON(output$girafe, simplifyVector = FALSE)
      expect_match(widget$x$html, "viewBox='0 0 450 262.5'", fixed = TRUE)
      session$setInputs(size = 2)
      root$flushReact()
      active(FALSE)
      settle(function() FALSE, seconds = 3)
      expect_length(session$cache$keys(), 1L)
      active(TRUE)
      expect_true(settle(function() length(session$cache$keys()) == 2L))
    }
  )
})
