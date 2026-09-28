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

# a missing Bioconductor package gives the install command

    Code
      as_summarized_experiment(GSE74821)
    Condition
      Error in `as_summarized_experiment()`:
      ! The SummarizedExperiment package is needed to build a SummarizedExperiment.
      i Install it with `BiocManager::install("SummarizedExperiment")`.

