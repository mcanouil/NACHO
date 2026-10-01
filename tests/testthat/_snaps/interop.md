# as_nacho() blames itself for a bad SummarizedExperiment

    Code
      as_nacho(se)
    Condition
      Error in `as_nacho()`:
      ! The probe data must have a CodeClass column.

# as_nacho() needs positive and negative control probes

    Code
      as_nacho(se[endogenous, ])
    Condition
      Error in `as_nacho()`:
      ! The probe data has no "Positive" or "Negative" control probes.
      i Keep the control probes when you subset the rows.

# as_nacho() points NACHO 2 objects to upgrade_nacho()

    Code
      as_nacho(nacho_2)
    Condition
      Error in `as_nacho()`:
      ! `x` is a NACHO 2 object, which NACHO 3 cannot use.
      i Convert it with `upgrade_nacho(x)`, or read the saved file with `read_nacho()`.

# as_nacho() checks the settings saved in the metadata

    Code
      as_nacho(se_with_setting("normalisation_method", "foo"))
    Condition
      Error in `as_nacho()`:
      ! `metadata(x)$nacho$settings$normalisation_method` must be one of "GEO", "GLM", or "RUVg", not "foo".

# as_nacho() checks the thresholds and RCC type saved in the metadata

    Code
      as_nacho(se)
    Condition
      Error in `as_nacho()`:
      ! `metadata(x)$nacho$rcc_type` must be one of "n1" or "n8", not "n2".
      i Did you mean "n1"?

# a missing Bioconductor package gives the install command

    Code
      as_summarized_experiment(GSE74821)
    Condition
      Error in `as_summarized_experiment()`:
      ! The SummarizedExperiment package is needed to build a SummarizedExperiment.
      i Install it with `BiocManager::install("SummarizedExperiment")`.

# as_nacho() checks the provenance saved in the metadata

    Code
      as_nacho(se)
    Condition
      Error in `as_nacho()`:
      ! `metadata(x)$nacho$provenance` must be a named list, not a string.

# as_nacho() does not overwrite a different sample id column

    Code
      as_nacho(se)
    Condition
      Error in `as_nacho()`:
      ! The sample data already has a column IDFILE that differs from the sample names.
      i Pass another `id_colname`, or rename that column.

# as_nacho() checks the NACHO metadata block

    Code
      as_nacho(se)
    Condition
      Error in `as_nacho()`:
      ! `metadata(x)$nacho` must be a named list, not a string.

# as_nacho() refuses unknown saved settings

    Code
      as_nacho(se)
    Condition
      Error in `as_nacho()`:
      ! `metadata(x)$nacho$settings` has unknown setting: n_comps.
      i Known settings: id_colname, housekeeping_genes, housekeeping_predict, housekeeping_norm, normalisation_method, ruv_k, background, background_mode, n_comp, and panel.

# as_nacho() points a saved id column clash to the metadata

    Code
      as_nacho(se)
    Condition
      Error in `as_nacho()`:
      ! The sample data already has a column IDFILE that differs from the sample names.
      i Rename that column, or remove id_colname from `metadata(x)$nacho$settings`.

