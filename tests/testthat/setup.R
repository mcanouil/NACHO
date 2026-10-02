withr::local_options(
  nacho.quiet = TRUE,
  .local_envir = testthat::teardown_env()
)

withr::local_envvar(
  RGL_USE_NULL = "TRUE",
  .local_envir = testthat::teardown_env()
)
