withr::local_options(
  nacho.quiet = TRUE,
  .local_envir = testthat::teardown_env()
)

# Child processes (Chrome, mirai daemons, the app under test) write their temporary files
# in the temporary folder of this R session, which R deletes at the end.
# Otherwise R CMD check reports them as detritus.
withr::local_envvar(
  RGL_USE_NULL = "TRUE",
  TMPDIR = tempdir(),
  TMP = tempdir(),
  TEMP = tempdir(),
  .local_envir = testthat::teardown_env()
)
