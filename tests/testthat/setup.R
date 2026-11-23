# Chromote waits `chromote.timeout` seconds (10 by default) for Chrome to start.
# On busy CI runners, Chrome can take longer than that to open its debugging port.
withr::local_options(
  nacho.quiet = TRUE,
  chromote.timeout = 60,
  .local_envir = testthat::teardown_env()
)

# Child processes (Chrome, mirai daemons, the app under test) write their temporary files
# in the temporary folder of this R session, which R deletes at the end.
# Otherwise R CMD check reports them as detritus.
session_tempdir <- tempdir()
withr::local_envvar(
  RGL_USE_NULL = "TRUE",
  TMPDIR = session_tempdir,
  TMP = session_tempdir,
  TEMP = session_tempdir,
  .local_envir = testthat::teardown_env()
)
