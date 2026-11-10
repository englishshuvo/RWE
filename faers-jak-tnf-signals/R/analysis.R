# Turn query totals into exclusive 2x2 contrasts and the quarterly VTE series.

total_of <- function(totals, id) {
  hit <- totals$total[match(id, totals$id)]
  if (length(hit) != 1L || is.na(hit)) stop("Missing query total: ", id)
  hit
}

build_contrasts <- function(totals) {
  rows <- list()
  for (cmp in COMPARISONS) {
    for (outcome in names(OUTCOME_LABELS)) {
      for (period in names(PERIODS)) {
        fit <- exclusive_cells(
          n_exposed = total_of(totals, query_id(period, cmp$exposed, "cohort")),
          n_exposed_outcome = total_of(totals, query_id(period, cmp$exposed, outcome)),
          n_comparator = total_of(totals, query_id(period, "tnf", "cohort")),
          n_comparator_outcome = total_of(totals, query_id(period, "tnf", outcome)),
          n_both = total_of(totals, query_id(period, cmp$both, "cohort")),
          n_both_outcome = total_of(totals, query_id(period, cmp$both, outcome))
        )
        rows[[length(rows) + 1L]] <- data.frame(
          comparison = cmp$id,
          comparison_label = cmp$label,
          outcome = outcome,
          outcome_label = unname(OUTCOME_LABELS[[outcome]]),
          period = period,
          period_label = unname(PERIOD_LABELS[[period]]),
          a = fit$a,
          b = fit$b,
          c = fit$c,
          d = fit$d,
          n_exposed = fit$n_exposed,
          n_comparator = fit$n_comparator,
          n_both = fit$n_both,
          ror = fit$ror,
          ror_low = fit$ror_low,
          ror_high = fit$ror_high,
          prr = fit$prr,
          signal = fit$signal,
          continuity_correction = fit$continuity_correction,
          estimable = fit$estimable,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$ror_fmt <- fmt_ratio(out$ror)
  out$ror_low_fmt <- fmt_ratio(out$ror_low)
  out$ror_high_fmt <- fmt_ratio(out$ror_high)
  out$prr_fmt <- fmt_ratio(out$prr)
  out
}

build_quarterly <- function(totals) {
  rows <- lapply(seq_len(nrow(QUARTERS)), function(i) {
    q <- QUARTERS[i, ]
    jak <- total_of(totals, query_id(q$id, "jak", "vte"))
    tnf <- total_of(totals, query_id(q$id, "tnf", "vte"))
    both <- total_of(totals, query_id(q$id, "jak_and_tnf", "vte"))
    if (both > jak || both > tnf) {
      stop("Quarter ", q$id, " has an intersection larger than one group.")
    }
    data.frame(
      quarter = q$id,
      year = q$year,
      q = q$quarter,
      mid = q$mid,
      jak_vte = jak,
      tnf_vte = tnf,
      both_vte = both,
      jak_only_vte = jak - both,
      tnf_only_vte = tnf - both,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

fmt_int <- function(x) {
  prettyNum(as.integer(round(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_ratio <- function(x) {
  ifelse(is.finite(x), sprintf("%.2f", x), NA_character_)
}

fmt_date <- function(x) {
  d <- as.Date(x)
  sprintf(
    "%d %s %d",
    as.integer(format(d, "%d")),
    format(d, "%B"),
    as.integer(format(d, "%Y"))
  )
}

cohort_flow <- function(totals) {
  jak <- total_of(totals, query_id("full", "jak", "cohort"))
  tnf <- total_of(totals, query_id("full", "tnf", "cohort"))
  both <- total_of(totals, query_id("full", "jak_and_tnf", "cohort"))
  data.frame(
    step = c(
      "Reports received in the window",
      "JAK ingredient, indication not required",
      "TNF ingredient, indication not required",
      "JAK ingredient and an immunology indication",
      "TNF ingredient and an immunology indication",
      "Both a JAK and a TNF ingredient, with an immunology indication",
      "JAK only, with an immunology indication",
      "TNF only, with an immunology indication"
    ),
    reports = c(
      total_of(totals, "window__all"),
      total_of(totals, query_id("full", "jak", "cohort", FALSE)),
      total_of(totals, query_id("full", "tnf", "cohort", FALSE)),
      jak,
      tnf,
      both,
      jak - both,
      tnf - both
    ),
    stringsAsFactors = FALSE
  )
}
