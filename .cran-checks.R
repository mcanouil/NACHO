devtools::check()
devtools::check_win_devel()
cran <- rhub::check_for_cran()
mac <- rhub::check_on_macos()
sol <- rhub::check_on_solaris()

# cran_prep <- rhub::check(
#   platform = c(
#     "debian-clang-devel",
#     "debian-gcc-devel",
#     "debian-gcc-patched",
#     "debian-gcc-release",
#     "fedora-clang-devel",
#     "fedora-gcc-devel",
#     "windows-x86_64-devel",
#     "solaris-x86-patched",
#     "windows-x86_64-release",
#     "macos-elcapitan-release"
#   ),
#   env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false",  `_R_CHECK_CRAN_INCOMING_USE_ASPELL_` = "false"),
#   check_args = "'--as-cran --no-manual --no-build-vignettes'",
#   show_status = FALSE
# )

# rm CRAN-RELEASE
# use_github_release(host = NULL, auth_token = github_token())
# git tag v1.0.0
# usethis::use_dev_version()
