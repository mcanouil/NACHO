# Generates the screenshots and the animation of the NACHO app.
# Run from the package root with the development version of NACHO installed.
# The animation needs the gifski package, which is not a dependency of NACHO.
# Without Chrome, set CHROMOTE_CHROME to the path of a Chromium based browser.

brave <- "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
if (
  is.null(suppressMessages(chromote::find_chrome())) &&
    file.exists(brave) &&
    !nzchar(Sys.getenv("CHROMOTE_CHROME"))
) {
  Sys.setenv(CHROMOTE_CHROME = brave)
}

figures <- "vignettes/articles/figures"
frames <- tempfile("nacho-frames-")
dir.create(figures, recursive = TRUE, showWarnings = FALSE)
dir.create(frames)

app <- shinytest2::AppDriver$new(
  NACHO::nacho_app(NACHO::GSE74821, done = TRUE),
  width = 1280,
  height = 800,
  load_timeout = 60000,
  timeout = 60000
)
on.exit(app$stop(), add = TRUE)

session <- app$get_chromote_session()
session$Emulation$setEmulatedMedia(
  features = list(list(name = "prefers-color-scheme", value = "light"))
)
app$run_js(
  "document.querySelector('bslib-input-dark-mode').setAttribute('mode', 'light')"
)

wait_for_plots <- function() {
  quiet <- 0
  waited <- 0
  while (quiet < 3 && waited < 60) {
    busy <- app$get_js(
      "Array.from(document.querySelectorAll('.recalculating')).filter(e => e.offsetParent !== null).length"
    )
    quiet <- if (busy == 0) quiet + 1 else 0
    Sys.sleep(1)
    waited <- waited + 1
  }
}

go_to <- function(page) {
  app$run_js(
    sprintf("document.querySelector('a[data-value=\"%s\"]').click()", page)
  )
  app$wait_for_idle(duration = 1000, timeout = 120000)
  wait_for_plots()
}

shoot <- function(name, directory = figures) {
  path <- file.path(directory, name)
  unlink(path)
  app$get_screenshot(path)
  invisible(path)
}

frame <- function() {
  shoot(sprintf("frame-%02d.png", length(list.files(frames)) + 1), frames)
}

click <- function(selector) {
  app$run_js(sprintf(
    "document.querySelector(%s).click()",
    jsonlite::toJSON(selector, auto_unbox = TRUE)
  ))
  Sys.sleep(1)
}

# Data page with the summary strip.
go_to("Data")
shoot("app-data.png")
frame()

# Threshold help popover.
help_button <- "button[aria-label='More about Binding density']"
for (attempt in 1:5) {
  click(help_button)
  Sys.sleep(1)
  if (app$get_js("document.querySelectorAll('.popover.show').length") > 0) {
    break
  }
}
app$run_js(
  "document.querySelectorAll('.popover').forEach(p => { p.style.transition = 'none'; p.style.opacity = 1; })"
)
Sys.sleep(1)
# AppDriver$get_screenshot() blurs the page and closes the popover first.
popover_path <- file.path(figures, "app-help-popover.png")
unlink(popover_path)
writeBin(
  jsonlite::base64_dec(session$Page$captureScreenshot(format = "png")$data),
  popover_path
)

# Move the field of view threshold, then the strip changes.
app$set_inputs(`thresholds-FoV` = 95)
app$wait_for_value(
  output = "overview-flagged_count",
  ignore = list(NULL, "", "0")
)
wait_for_plots()
frame()

go_to("QC metrics")
shoot("app-qc-metrics.png")
frame()

# Full-screen card.
click(".bslib-card .bslib-full-screen-enter")
Sys.sleep(2)
wait_for_plots()
shoot("app-full-screen.png")
click(".bslib-full-screen-exit")
Sys.sleep(1)

# A selected sample, outlined in the plots.
samples <- app$get_js(
  "Array.from(document.querySelectorAll('#outliers-highlight option')).map(o => o.value).filter(v => v)"
)
app$set_inputs(`outliers-highlight` = samples[[1]], wait_ = FALSE)
Sys.sleep(4)
wait_for_plots()
shoot("app-selected-qc.png")
frame()
frame()

go_to("Samples")
shoot("app-selected-samples.png")
frame()

go_to("Normalisation")
shoot("app-normalisation.png")
frame()

go_to("Batch")
shoot("app-batch.png")
frame()

# Help menu.
go_to("QC metrics")
app$run_js(
  "Array.from(document.querySelectorAll('a.dropdown-toggle')).find(a => a.textContent.trim() === 'Help').click()"
)
Sys.sleep(1)
shoot("app-help-menu.png")
frame()
app$run_js("document.body.click(); document.activeElement.blur()")
Sys.sleep(1)

# Export with a title and an author, and the report ready.
go_to("Export")
frame()
app$set_inputs(
  `export-title` = "Quality control of GSE74821",
  `export-author` = "A. Researcher"
)
app$click("export-render")
app$wait_for_js(
  "document.querySelector('#export-report') !== null",
  timeout = 180000
)
Sys.sleep(1)
shoot("app-export.png")
frame()

# Done button at the foot of the sidebar, with the open sections folded.
go_to("QC metrics")
app$run_js(
  "document.querySelectorAll('.accordion-button:not(.collapsed)').forEach(b => b.click())"
)
Sys.sleep(2)
shoot("app-done.png")
app$run_js(
  "document.querySelectorAll('.accordion-button.collapsed').forEach(b => b.click())"
)

# The picture for the README.
go_to("QC metrics")
shoot("README-app.png", "man/figures")
file.copy(
  "man/figures/README-app.png",
  "vignettes/README-app.png",
  overwrite = TRUE
)

gif <- "man/figures/README-nacho_app.gif"
unlink(gif)
gifski::gifski(
  list.files(frames, pattern = "^frame-.*[.]png$", full.names = TRUE),
  gif_file = gif,
  width = 900,
  delay = 1,
  loop = TRUE,
  progress = FALSE
)
unlink(frames, recursive = TRUE)
