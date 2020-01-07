devtools::check()

devtools::check_win_devel()

cran_prep <- rhub::check_for_cran(
  check_args = "'--as-cran --no-manual --no-build-vignettes'",
  env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false",  `_R_CHECK_CRAN_INCOMING_USE_ASPELL_` = "false")
)

cran_prep_big <- rhub::check(
  platform = c(
    "debian-clang-devel",
    "debian-gcc-devel",
    "fedora-clang-devel",
    "fedora-gcc-devel",
    "windows-x86_64-devel",
    "debian-gcc-patched",
    "solaris-x86-patched",
    "debian-gcc-release",
    "windows-x86_64-release",
    "macos-elcapitan-release"
  ),
  env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false",  `_R_CHECK_CRAN_INCOMING_USE_ASPELL_` = "false"),
  check_args = "'--as-cran --no-manual --no-build-vignettes'",
  show_status = FALSE
)

# rhub::list_package_checks()

cran_prep$cran_summary()
cran_prep_big$cran_summary()

# rm CRAN-RELEASE

# git tag v1.0.0

# usethis::use_dev_version()
