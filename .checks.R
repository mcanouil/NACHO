setwd("/disks/PROJECT/_Mickael/PACKAGES/NACHO")

rhub::validate_email(email = "mickael.canouil@cnrs.fr", token = "1c1d492385b44415b673cf8acc567178")

devtools::check(pkg = "/disks/PROJECT/_Mickael/PACKAGES/NACHO")

devtools::check_win_devel(pkg = "/disks/PROJECT/_Mickael/PACKAGES/NACHO")

# rhub::check_for_cran(
#   path = "/disks/PROJECT/_Mickael/PACKAGES/NACHO",
#   email = "mickael.canouil@cnrs.fr",
#   check_args = "--as-cran",
#   env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false",  `_R_CHECK_CRAN_INCOMING_USE_ASPELL_` = "false")
# )
# rhub::check_on_solaris(
#   path = "/disks/PROJECT/_Mickael/PACKAGES/NACHO",
#   email = "mickael.canouil@cnrs.fr",
#   show_status = FALSE
# )
# rhub::check_on_macos(
#   path = "/disks/PROJECT/_Mickael/PACKAGES/NACHO",
#   email = "mickael.canouil@cnrs.fr",
#   show_status = FALSE
# )
# rhub::check_on_windows(
#   path = "/disks/PROJECT/_Mickael/PACKAGES/NACHO",
#   email = "mickael.canouil@cnrs.fr",
#   show_status = FALSE
# )

rhub::check(
  path = "/disks/PROJECT/_Mickael/PACKAGES/NACHO",
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
  email = "mickael.canouil@cnrs.fr",
  env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false",  `_R_CHECK_CRAN_INCOMING_USE_ASPELL_` = "false"),
  show_status = FALSE
)

# rhub::list_package_checks()

# rm CRAN-RELEASE

# git tag v1.0.0

# usethis::use_dev_version()
