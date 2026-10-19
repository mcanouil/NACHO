brave_path <- "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"

if (
  requireNamespace("chromote", quietly = TRUE) &&
    is.null(suppressMessages(chromote::find_chrome())) &&
    file.exists(brave_path)
) {
  Sys.setenv(CHROMOTE_CHROME = brave_path)
}

skip_if_no_browser <- function() {
  testthat::skip_on_cran()
  testthat::skip_if(
    requireNamespace("covr", quietly = TRUE) && covr::in_covr(),
    "Browser tests stop R processes that covr traces (r-lib/covr#509)."
  )
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  testthat::skip_if(
    is.null(suppressMessages(chromote::find_chrome())),
    "Chrome is not available."
  )
}
