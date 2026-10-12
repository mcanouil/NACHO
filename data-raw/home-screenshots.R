# Generates the report screenshots of the home page of the package website.
# Run from the package root with the development version of NACHO installed.
# Without Chrome, set CHROMOTE_CHROME to the path of a Chromium based browser.

rlang::check_installed(c("chromote", "pdftools", "png"))

brave <- "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
if (
  is.null(suppressMessages(chromote::find_chrome())) &&
    file.exists(brave) &&
    !nzchar(Sys.getenv("CHROMOTE_CHROME"))
) {
  Sys.setenv(CHROMOTE_CHROME = brave)
}

shoot_html_top <- function(html_report, path) {
  chrome <- chromote::Chromote$new()
  on.exit(chrome$close(), add = TRUE)
  browser <- chromote::ChromoteSession$new(
    parent = chrome,
    width = 1280,
    height = 720
  )
  browser$Emulation$setEmulatedMedia(
    features = list(list(name = "prefers-color-scheme", value = "light"))
  )
  browser$Page$navigate(paste0("file://", html_report))
  deadline <- Sys.time() + 60
  repeat {
    state <- browser$Runtime$evaluate(
      "document.readyState + ' ' + document.fonts.status"
    )[["result"]][["value"]]
    if (identical(state, "complete loaded")) {
      break
    }
    if (Sys.time() > deadline) {
      stop("The report did not finish loading within 60 seconds.", call. = FALSE)
    }
    Sys.sleep(0.2)
  }
  shot <- browser$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(shot[["data"]]), path)
}

find_pdf_page <- function(pdf_report, text) {
  pages <- pdftools::pdf_text(pdf_report)
  hits <- grep(text, pages, fixed = TRUE)
  if (length(hits) == 0L) {
    stop("The PDF report has no page with the text '", text, "'.", call. = FALSE)
  }
  hits[[length(hits)]]
}

save_pdf_page <- function(pdf_report, page, path) {
  bitmap <- pdftools::pdf_render_page(pdf_report, page = page, dpi = 110)
  png::writePNG(bitmap, path)
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
