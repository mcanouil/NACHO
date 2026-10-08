if (is.null(getOption("nacho.plot_workers"))) {
  options(nacho.plot_workers = 1)
}
NACHO::nacho_app()
