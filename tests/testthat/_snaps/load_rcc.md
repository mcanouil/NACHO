# load_rcc() names the RCC files it cannot find

    Code
      load_rcc(test_path("plexset_data"), sheet, "IDFILE")
    Condition
      Error in `load_rcc()`:
      ! 1 value of IDFILE does not match an RCC file in '<data_directory>'.
      x Missing: 'missing.RCC'.
      i Check that `id_colname` holds file names, including the ".RCC" or ".RCC.gz" extension.

# heterogenous

    Code
      suppressMessages(load_rcc(data_directory = test_path(), ssheet_csv = sheet,
      id_colname = "IDFILE", housekeeping_predict = TRUE, housekeeping_norm = TRUE))
    Condition
      Error in `load_rcc()`:
      ! RCC files come from more than one NanoString file or software version.
      * File versions: "1.7".
      * Software versions: "3.1.0.1" and "4.0.0.3".
      i Load each version separately.

