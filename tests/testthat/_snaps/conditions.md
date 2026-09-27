# check_package() names the install command

    Code
      NACHO:::check_package("notapackage", reason = "to test", install = "install.packages(\"notapackage\")")
    Condition
      Error:
      ! The notapackage package is needed to test.
      i Install it with `install.packages("notapackage")`.

