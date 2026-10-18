# Generates the report screenshots of the home page of the package website.
# The pkgdown workflow runs it before each site build, so the images match the
# NACHO version that the workflow installs. The images are not in git: run it
# once before a local pkgdown::build_site().
# Run from the package root with the development version of NACHO installed.
# Without Chrome, set CHROMOTE_CHROME to the path of a Chromium based browser.

rlang::check_installed(c("chromote", "jsonlite", "pdftools"))

brave <- "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
if (
  is.null(suppressMessages(chromote::find_chrome())) &&
    file.exists(brave) &&
    !nzchar(Sys.getenv("CHROMOTE_CHROME"))
) {
  Sys.setenv(CHROMOTE_CHROME = brave)
}

shoot_html_top <- function(html_report, path) {
  chrome_args <- chromote::default_chrome_args()
  if (nzchar(Sys.getenv("CI"))) {
    chrome_args <- c(chrome_args, "--no-sandbox")
  }
  chrome <- chromote::Chromote$new(
    browser = chromote::Chrome$new(args = chrome_args)
  )
  on.exit(chrome$close(), add = TRUE)
  browser <- chromote::ChromoteSession$new(
    parent = chrome,
    width = 1280,
    height = 720
  )
  browser$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-color-scheme", value = "light"))
  )
  loaded <- browser$Page$loadEventFired(wait_ = FALSE)
  browser$Page$navigate(paste0("file://", html_report), wait_ = FALSE)
  browser$wait_for(loaded)
  browser$Runtime$evaluate(
    "document.fonts.ready.then(() => true)",
    awaitPromise = TRUE
  )
  shot <- browser$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(shot[["data"]]), path)
}

find_pdf_page <- function(pdf_report, text) {
  pages <- pdftools::pdf_text(pdf_report)
  hits <- grep(text, pages, fixed = TRUE)
  if (length(hits) == 0L) {
    stop(
      "The PDF report has no page with the text '",
      text,
      "'.",
      call. = FALSE
    )
  }
  hits[[length(hits)]]
}

# Each page shows at about 200 CSS pixels wide, so 50 dpi gives a sharp image on 2x screens.
save_pdf_page <- function(pdf_report, page, path) {
  pdftools::pdf_convert(
    pdf_report,
    format = "png",
    pages = page,
    filenames = path,
    dpi = 50,
    verbose = FALSE
  )
}

main <- function() {
  assets <- "pkgdown/assets/home"
  dir.create(assets, recursive = TRUE, showWarnings = FALSE)

  example <- NACHO::GSE74821
  limits <- NACHO::nacho_thresholds()
  limits[["FoV"]] <- 95
  example <- NACHO::normalise(example, outliers_thresholds = limits)

  report_dir <- tempfile("nacho-home-")
  on.exit(unlink(report_dir, recursive = TRUE), add = TRUE)
  title <- "PAM50 study, batch 3"
  html_report <- NACHO::render(
    example,
    format = "html",
    output_dir = report_dir,
    title = title
  )
  pdf_report <- NACHO::render(
    example,
    format = "typst",
    output_dir = report_dir,
    title = title
  )

  shoot_html_top(html_report, file.path(assets, "report-html.png"))
  save_pdf_page(pdf_report, 1L, file.path(assets, "report-pdf-cover.png"))
  save_pdf_page(
    pdf_report,
    find_pdf_page(pdf_report, "Decision summary"),
    file.path(assets, "report-pdf-page.png")
  )
}

main()
