.onLoad <- function(libname, pkgname) {
  S7::S7_on_load()
}

.onUnload <- function(libpath) {
  S7::S7_on_unload()
}

S7::S7_on_build()
