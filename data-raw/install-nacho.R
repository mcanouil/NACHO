# Shared temp-library install used by the data-raw fixture scripts.
#
# Installs NACHO from `source_dir` (a package source directory, "." for this
# working tree, or a git worktree checked out at another ref) into a fresh
# temporary library, then loads it: attached with library() when `attach` is
# TRUE, or namespace-only with loadNamespace() otherwise.
# Returns a list with the library path and a `cleanup()` function that
# detaches or unloads only what this call itself loaded, removes the
# temporary library and warns if that fails, and restores
# `R_KEEP_PKG_SOURCE`. Callers must run `cleanup()` from their own
# `on.exit()`, right after calling this function, so every exit path cleans
# up.

install_nacho <- function(source_dir = ".", attach = TRUE) {
  if (attach && "package:NACHO" %in% search()) {
    stop(
      "package:NACHO is already attached; the temporary-library guarantee ",
      "cannot hold. Detach it before running this script.",
      call. = FALSE
    )
  }

  keep_source <- Sys.getenv("R_KEEP_PKG_SOURCE", unset = NA)
  restore_env <- function() {
    if (is.na(keep_source)) {
      Sys.unsetenv("R_KEEP_PKG_SOURCE")
    } else {
      Sys.setenv(R_KEEP_PKG_SOURCE = keep_source)
    }
  }
  Sys.setenv(R_KEEP_PKG_SOURCE = "no")

  lib <- tempfile("nacho-install-")
  dir.create(lib)
  install_log <- system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD",
      "INSTALL",
      "--no-docs",
      "--no-help",
      paste0("--library=", lib),
      source_dir
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  if (
    !is.null(attr(install_log, "status")) && attr(install_log, "status") != 0
  ) {
    cat(install_log, sep = "\n")
    unlink(lib, recursive = TRUE)
    restore_env()
    stop(
      "R CMD INSTALL failed while installing NACHO from ",
      source_dir,
      ".",
      call. = FALSE
    )
  }

  if (attach) {
    library(NACHO, lib.loc = lib)
  } else {
    loadNamespace("NACHO", lib.loc = lib)
  }

  cleanup <- function() {
    if (attach) {
      if ("package:NACHO" %in% search()) {
        detach("package:NACHO", unload = TRUE, character.only = TRUE)
      }
    } else if (isNamespaceLoaded("NACHO")) {
      unloadNamespace("NACHO")
    }
    if (unlink(lib, recursive = TRUE) != 0) {
      warning(
        "Could not remove the temporary install library: ",
        lib,
        call. = FALSE
      )
    }
    restore_env()
  }

  list(lib = lib, cleanup = cleanup)
}
