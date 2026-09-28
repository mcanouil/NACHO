# check_choice() reports a bad choice once, with a suggestion

    Code
      k("geo")
    Condition
      Error in `k()`:
      ! `value` must be one of "GEO" or "GLM", not "geo".
      i Did you mean "GEO"?

---

    Code
      k("nope")
    Condition
      Error in `k()`:
      ! `value` must be one of "GEO" or "GLM", not "nope".

# check_nacho() explains what it expected

    Code
      check_outliers(list(a = 1))
    Condition
      Error in `check_outliers()`:
      ! `nacho_object` must be a <nacho> object, not a list.
      i Create one with `load_rcc()`.

# check_package() names the install command

    Code
      NACHO:::check_package("notapackage", reason = "to test", install = "install.packages(\"notapackage\")")
    Condition
      Error:
      ! The notapackage package is needed to test.
      i Install it with `install.packages("notapackage")`.

