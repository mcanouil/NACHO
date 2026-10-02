#' @include mirna.R
NULL

#' Cramér's V between two categorical variables
#'
#' @noRd
cramers_v <- function(a, b) {
  observed <- table(droplevels(factor(a)), droplevels(factor(b)))
  if (min(dim(observed)) < 2) {
    return(NA_real_)
  }
  n <- sum(observed)
  expected <- outer(rowSums(observed), colSums(observed)) / n
  chi_squared <- sum((observed - expected)^2 / expected)
  sqrt(chi_squared / (n * (min(dim(observed)) - 1)))
}

#' Share of variance explained by a batch variable
#'
#' @noRd
group_r2 <- function(values, batch) {
  keep <- !is.na(values) & !is.na(batch)
  values <- values[keep]
  batch <- batch[keep]
  if (length(unique(batch)) < 2) {
    return(NA_real_)
  }
  total <- sum((values - mean(values))^2)
  if (total == 0) {
    return(NA_real_)
  }
  1 - sum((values - stats::ave(values, batch))^2) / total
}

metrics_template <- data.frame(
  metric = character(),
  batch = character(),
  statistic = numeric(),
  p_value = numeric()
)

pc_batch_template <- data.frame(
  PC = character(),
  batch = character(),
  r_squared = numeric()
)

#' Row-bind a list of data frames, or give the empty template
#'
#' @noRd
bind_or_empty <- function(template, pieces) {
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) == 0) {
    return(template)
  }
  do.call(rbind, pieces)
}

#' Rows that carry one observation of a metric
#'
#' PlexSet lane metrics repeat across the eight samples of a lane, so only
#' the first sample of each lane file counts.
#'
#' @noRd
metric_rows <- function(samples, metric, x) {
  if (x@rcc_type != "n8" || !metric %in% lane_metrics) {
    return(rep(TRUE, nrow(samples)))
  }
  lanes <- strip_plexset_suffix(samples, x@settings[["id_colname"]])
  !duplicated(lanes[[x@settings[["id_colname"]]]])
}

#' Batch and confounding diagnostics
#'
#' Checks whether the study design, the quality-control metrics and the main
#' axes of variation follow technical batches such as cartridges and run
#' dates.
#'
#' Start with `design`: when a batch level holds a single group, batch and
#' biology cannot be told apart, and no normalisation can fix it.
#'
#' @param x A `nacho` object from [load_rcc()] or [normalise()].
#' @param group A column of `nacho_samples(x)` with the biological groups, or
#'   `NULL` to skip the design check.
#' @param batch Columns of `nacho_samples(x)` that describe technical
#'   batches.
#'
#' @return A list:
#' * `design`: `NULL` without `group`; otherwise, for each batch variable, its number of levels, Cramér's V with
#'   `group`, the number of levels that hold a single group, and
#'   `confounded`, `TRUE` when at least one does.
#'   A level counts as holding a single group only when it has two or more
#'   samples with a group, so a cartridge with one sample never makes the
#'   design confounded.
#' * `crosstabs`: `NULL` without `group`; otherwise the table of `group` against each batch variable.
#' * `metrics`: Kruskal-Wallis tests of each quality-control metric against
#'   each batch variable.
#' * `pc_batch`: the share of each principal component's variance explained
#'   by each batch variable.
#'
#' `metrics` and `pc_batch` are data frames with these columns, and have no
#' rows when there is nothing to report.
#' A batch variable with one level gives `NA` in `statistic`, `p_value` and
#' `r_squared`, and a constant metric gives `NA` in `statistic` and `p_value`.
#' For PlexSet data, `BD` and `FoV` are tested once per lane.
#' @export
#' @examples
#' data(GSE74821)
#' batch_diagnostics(GSE74821)$pc_batch
batch_diagnostics <- function(
  x,
  group = NULL,
  batch = c("CartridgeID", "Date")
) {
  check_nacho(x)
  samples <- nacho_samples(x)
  check_string(group, allow_null = TRUE)
  if (!is.null(group)) {
    check_column(group, samples, data_arg = "nacho_samples(x)")
  }
  check_character(batch)
  for (column in batch) {
    check_column(column, samples, data_arg = "nacho_samples(x)", arg = "batch")
  }
  design <- crosstabs <- NULL
  if (!is.null(group)) {
    groups <- samples[[group]]
    crosstabs <- stats::setNames(
      lapply(batch, function(b) table(groups, samples[[b]], dnn = c(group, b))),
      batch
    )
    design <- do.call(
      rbind,
      lapply(batch, function(b) {
        single_group <- tapply(groups, samples[[b]], function(g) {
          g <- g[!is.na(g)]
          length(g) >= 2 && length(unique(g)) == 1
        })
        n_levels <- length(unique(stats::na.omit(samples[[b]])))
        single <- if (n_levels < 2) {
          0L
        } else {
          as.integer(sum(single_group, na.rm = TRUE))
        }
        data.frame(
          batch = b,
          n_levels = n_levels,
          cramers_v = cramers_v(groups, samples[[b]]),
          single_group_levels = single,
          confounded = single > 0
        )
      })
    )
  }
  metric_names <- intersect(
    c(qc_metrics, "MC", "MedC", "Negative_factor"),
    names(samples)
  )
  metrics <- bind_or_empty(
    metrics_template,
    lapply(batch, function(b) {
      do.call(
        rbind,
        lapply(metric_names, function(m) {
          rows <- metric_rows(samples, m, x)
          values <- samples[[m]][rows]
          batch_values <- samples[[b]][rows]
          usable <- !is.na(values) & !is.na(batch_values)
          if (
            length(unique(batch_values[usable])) < 2 ||
              length(unique(values[usable])) < 2
          ) {
            return(data.frame(
              metric = m,
              batch = b,
              statistic = NA_real_,
              p_value = NA_real_
            ))
          }
          test <- stats::kruskal.test(
            values[usable],
            factor(batch_values[usable])
          )
          data.frame(
            metric = m,
            batch = b,
            statistic = unname(test[["statistic"]]),
            p_value = test[["p.value"]]
          )
        })
      )
    })
  )
  scores <- x@pca[["scores"]]
  pc_batch <- bind_or_empty(
    pc_batch_template,
    lapply(batch, function(b) {
      data.frame(
        PC = as.character(colnames(scores)),
        batch = rep(b, ncol(scores)),
        r_squared = vapply(
          seq_len(ncol(scores)),
          function(k) group_r2(scores[, k], samples[[b]]),
          numeric(1)
        )
      )
    })
  )
  list(
    design = design,
    crosstabs = crosstabs,
    metrics = metrics,
    pc_batch = pc_batch
  )
}
