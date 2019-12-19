library(NACHO)
library(webshot)

appshot_nacho <- function(
  app,
  file = "webshot.png",
  ...,
  nacho_object = NULL,
  port = getOption("shiny.port"),
  envvars = NULL,
  webshot_timeout = 60
) {
  shiny::shinyOptions(nacho_object = nacho_object)
  port <- webshot:::available_port(port)
  url <- webshot:::shiny_url(port)
  args <- list(url = url, file = file, ..., timeout = webshot:::webshot_app_timeout())
  p <- webshot:::r_background_process(function(url, file, ..., timeout) {
    wait <- utils::getFromNamespace("wait_until_server_exists", "webshot")
    wait(url, timeout = timeout)
    webshot::webshot(url = url, file = file, ...)
  }, args, envvars = envvars)
  on.exit({p$kill()})
  if (!is.null(args$delay)) {
    webshot_timeout <- webshot_timeout + args$delay
  }
  start_time <- as.numeric(Sys.time())
  shiny::observe({
    if (p$is_alive()) {
      if ((as.numeric(Sys.time()) - start_time) <= webshot_timeout) {
        shiny::invalidateLater(200)
      } else {
        message("webshot timed out")
        p$kill()
        shiny::stopApp()
      }
    } else {
      shiny::stopApp()
    }
    return()
  })
  shiny::runApp(app, port = port, display.mode = "normal", launch.browser = FALSE)
  invisible(p$get_result())
}

appshot_nacho(
  app = system.file("app", package = "NACHO"),
  file = "man/figures/README-visualise.png",
  nacho_object = GSE74821,
  delay = 3,
  cliprect = "viewport",
  vwidth = 1280 * 1.5,
  vheight = 640 * 1.5,
  zoom = 1
)
file.copy("man/figures/README-visualise.png", "vignettes/README-visualise.png", overwrite = TRUE)
appshot_nacho(
  app = system.file("app", package = "NACHO"),
  file = "man/figures/README-app.png",
  delay = 3,
  cliprect = "viewport",
  vwidth = 1280 * 1.5,
  vheight = 640 * 1.5,
  zoom = 1
)
file.copy("man/figures/README-app.png", "vignettes/README-app.png", overwrite = TRUE)
