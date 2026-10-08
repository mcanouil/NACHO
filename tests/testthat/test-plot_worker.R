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
  expect_identical(NACHO:::plot_worker_count(10L, NULL), 4L)
  expect_identical(NACHO:::plot_worker_count(3L, NULL), 2L)
  expect_identical(NACHO:::plot_worker_count(1L, NULL), 1L)
  expect_identical(NACHO:::plot_worker_count(NA_integer_, NULL), 1L)
})

test_that("the nacho.plot_workers option caps or turns off the workers", {
  expect_identical(NACHO:::plot_worker_count(10L, 2), 2L)
  expect_identical(NACHO:::plot_worker_count(3L, 8), 2L)
  expect_identical(NACHO:::plot_worker_count(10L, 0), 0L)
  withr::local_options(nacho.plot_workers = 0)
  expect_null(NACHO:::plot_workers_start())
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

test_that("one worker pool serves the app until it stops", {
  skip_if_not_installed("mirai")
  withr::defer(NACHO:::plot_workers_stop())
  withr::local_options(nacho.plot_workers = 2)
  started <- Sys.time()
  profile <- NACHO:::plot_workers_start()
  expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 2)
  expect_identical(NACHO:::plot_workers_start(), profile)
  expect_true(mirai::mirai(TRUE, .compute = profile)[])
  expect_gt(mirai::status(.compute = profile)$connections, 0)
  NACHO:::plot_workers_stop()
  expect_identical(mirai::status(.compute = profile)$connections, 0L)
})

test_that("without mirai the app starts no workers", {
  local_mocked_bindings(has_package = function(package) FALSE)
  expect_null(NACHO:::plot_workers_start())
})

card_args <- function(active, workers) {
  list(
    id = "BD",
    object = shiny::reactiveVal(NACHO::GSE74821),
    qc = shiny::reactive(nacho_qc(NACHO::GSE74821)),
    type = "BD",
    dark = shiny::reactive(FALSE),
    interactive = TRUE,
    active = active,
    workers = workers
  )
}

card_session <- function() {
  root <- shiny::MockShinySession$new()
  root$clientData <- shiny::reactiveValues(
    `output_BD-girafe_width` = 600,
    `output_BD-girafe_height` = 350
  )
  root
}

settle <- function(root, done, seconds = 60) {
  deadline <- Sys.time() + seconds
  while (!done() && Sys.time() < deadline) {
    later::run_now(0.1)
    root$flushReact()
  }
  done()
}

test_that("a card builds in a worker only while its page shows", {
  skip_if_not_installed("ggiraph")
  skip_if_not_installed("mirai")
  skip_if_not_installed("later")
  withr::defer(NACHO:::plot_workers_stop())
  withr::local_options(nacho.plot_workers = 2)
  root <- card_session()
  on.exit(root$close(), add = TRUE)
  active <- shiny::reactiveVal(FALSE)
  shiny::testServer(
    NACHO:::mod_qc_plot_server,
    args = card_args(active, NACHO:::plot_workers_start()),
    session = root,
    {
      cache <- shiny::getShinyOption("cache", default = session$cache)
      cache$reset()
      keys <- function() length(cache$keys())
      drawn <- function() {
        !inherits(try(output$girafe, silent = TRUE), "try-error")
      }
      session$setInputs(colour = "CartridgeID")
      session$elapse(300)
      settle(root, function() FALSE, seconds = 1)
      expect_identical(keys(), 0L)
      active(TRUE)
      expect_true(settle(root, function() keys() == 1L))
      expect_true(settle(root, drawn))
      widget <- jsonlite::fromJSON(output$girafe, simplifyVector = FALSE)
      expect_match(widget$x$html, "viewBox='0 0 450 262.5'", fixed = TRUE)
      session$setInputs(size = 2)
      active(FALSE)
      settle(root, function() FALSE, seconds = 3)
      expect_identical(keys(), 1L)
      session$setInputs(size = 3)
      active(TRUE)
      root$flushReact()
      active(FALSE)
      root$flushReact()
      active(TRUE)
      expect_true(settle(root, function() keys() == 2L))
      expect_true(settle(root, drawn))
      expect_false(isTRUE(NACHO:::plot_pool$failed))
    }
  )
})

test_that("a card builds in the app process when the workers do not start", {
  skip_if_not_installed("ggiraph")
  skip_if_not_installed("mirai")
  skip_if_not_installed("later")
  withr::defer(NACHO:::plot_workers_stop())
  timeout <- NACHO:::plot_pool$timeout
  withr::defer(assign("timeout", timeout, envir = NACHO:::plot_pool))
  assign("timeout", 1000, envir = NACHO:::plot_pool)
  local_mocked_bindings(
    launch_local = function(...) invisible(NULL),
    .package = "mirai"
  )
  withr::local_options(nacho.plot_workers = 2, nacho.quiet = TRUE)
  root <- card_session()
  on.exit(root$close(), add = TRUE)
  shiny::testServer(
    NACHO:::mod_qc_plot_server,
    args = card_args(shiny::reactive(TRUE), NACHO:::plot_workers_start()),
    session = root,
    {
      cache <- shiny::getShinyOption("cache", default = session$cache)
      cache$reset()
      drawn <- function() {
        !inherits(try(output$girafe, silent = TRUE), "try-error")
      }
      session$setInputs(colour = "CartridgeID")
      session$elapse(300)
      expect_true(settle(root, function() length(cache$keys()) == 1L))
      expect_true(settle(root, drawn))
      expect_true(NACHO:::plot_pool$failed)
      expect_null(NACHO:::plot_workers_start())
      widget <- jsonlite::fromJSON(output$girafe, simplifyVector = FALSE)
      expect_match(widget$x$html, "viewBox='0 0 450 262.5'", fixed = TRUE)
    }
  )
})
