# autoplot() needs a known plot type

    Code
      autoplot(GSE74821, type = "bd")
    Condition
      Error in `autoplot()`:
      ! `type` must be one of "BD", "FoV", "PCL", "LoD", "Positive", "Negative", "Housekeeping", "PN", "ACBD", "ACMC", "PCA12", "PCAi", "PCA", "PFNF", "HF", or "NORM", not "bd".
      i Did you mean "BD"?

# autoplot() points NACHO 2 callers to type

    Code
      autoplot(GSE74821, x = "BD")
    Condition
      Error in `autoplot()`:
      ! `x` was renamed `type` in NACHO 3.0.0.
      i Use `autoplot(object, type = "BD")`.

