# print() shows a short summary and returns the object invisibly

    Code
      print(x)
    Output
      <nacho> 4 samples, 11 probes, from single-sample RCC files
      Normalisation: GEO, with 1 housekeeping gene
      Flagged samples: 0 of 4
      Created with NACHO 0.0.0

# NACHO 2 method errors name the argument

    Code
      dim(old)
    Condition
      Error in `dim()`:
      ! `old` is a NACHO 2 object, which NACHO 3 cannot use.
      i Convert it with `upgrade_nacho(old)`, or read the saved file with `read_nacho()`.

---

    Code
      summary(old)
    Condition
      Error in `summary()`:
      ! `old` is a NACHO 2 object, which NACHO 3 cannot use.
      i Convert it with `upgrade_nacho(old)`, or read the saved file with `read_nacho()`.

