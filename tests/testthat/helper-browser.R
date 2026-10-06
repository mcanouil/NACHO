brave_path <- "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"

if (
  requireNamespace("chromote", quietly = TRUE) &&
    is.null(chromote::find_chrome()) &&
    file.exists(brave_path)
) {
  Sys.setenv(CHROMOTE_CHROME = brave_path)
}
