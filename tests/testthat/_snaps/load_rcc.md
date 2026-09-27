# load_rcc() names the RCC files it cannot find

    Code
      load_rcc(test_path("plexset_data"), sheet, "IDFILE")
    Condition
      Error in `load_rcc()`:
      ! 1 value of IDFILE does not match an RCC file in '/Users/mcanouil/Projects/pro/academia/NACHO/tests/testthat/plexset_data'.
      x Missing: 'missing.RCC'.
      i Check that `id_colname` holds file names, including the ".RCC" or ".RCC.gz" extension.

