# Reporting odds ratio, proportional reporting ratio, and the 2x2
# that remains after reports naming both sides of a contrast are removed.

signal_flag <- function(ror_low, cases) {
  isTRUE(is.finite(ror_low) && ror_low > 1 && is.finite(cases) && cases >= 3)
}

two_by_two <- function(a, b, c, d) {
  # Doubles, not integers. A product of two FAERS counts overflows a 32-bit integer.
  cells <- c(a = as.numeric(a), b = as.numeric(b), c = as.numeric(c), d = as.numeric(d))
  if (any(!is.finite(cells)) || any(cells < 0)) {
    stop("A 2x2 cell must be finite and at least zero.")
  }
  empty <- list(
    a = unname(cells[["a"]]),
    b = unname(cells[["b"]]),
    c = unname(cells[["c"]]),
    d = unname(cells[["d"]]),
    a_used = NA_real_,
    b_used = NA_real_,
    c_used = NA_real_,
    d_used = NA_real_,
    ror = NA_real_,
    ror_low = NA_real_,
    ror_high = NA_real_,
    log_ror_se = NA_real_,
    prr = NA_real_,
    signal = FALSE,
    continuity_correction = FALSE,
    estimable = FALSE
  )
  if ((cells[["a"]] + cells[["b"]]) == 0 || (cells[["c"]] + cells[["d"]]) == 0) {
    return(empty)
  }
  corrected <- any(cells == 0)
  used <- if (corrected) cells + 0.5 else cells
  ror <- (used[["a"]] * used[["d"]]) / (used[["b"]] * used[["c"]])
  se <- sqrt(1 / used[["a"]] + 1 / used[["b"]] + 1 / used[["c"]] + 1 / used[["d"]])
  log_ror <- log(ror)
  prr <- (used[["a"]] / (used[["a"]] + used[["b"]])) / (used[["c"]] / (used[["c"]] + used[["d"]]))
  list(
    a = unname(cells[["a"]]),
    b = unname(cells[["b"]]),
    c = unname(cells[["c"]]),
    d = unname(cells[["d"]]),
    a_used = unname(used[["a"]]),
    b_used = unname(used[["b"]]),
    c_used = unname(used[["c"]]),
    d_used = unname(used[["d"]]),
    ror = unname(ror),
    ror_low = unname(exp(log_ror - Z_95 * se)),
    ror_high = unname(exp(log_ror + Z_95 * se)),
    log_ror_se = unname(se),
    prr = unname(prr),
    signal = signal_flag(exp(log_ror - Z_95 * se), cells[["a"]]),
    continuity_correction = corrected,
    estimable = TRUE
  )
}

# n_* are report totals. n_*_outcome are the reports inside that total
# that also have the outcome. n_both is the intersection and is removed
# from both arms so a report is not its own comparator.
exclusive_cells <- function(n_exposed, n_exposed_outcome, n_comparator, n_comparator_outcome, n_both, n_both_outcome) {
  margins <- c(
    n_exposed = n_exposed,
    n_exposed_outcome = n_exposed_outcome,
    n_comparator = n_comparator,
    n_comparator_outcome = n_comparator_outcome,
    n_both = n_both,
    n_both_outcome = n_both_outcome
  )
  if (any(!is.finite(margins)) || any(margins < 0)) {
    stop("Report counts must be finite and at least zero.")
  }
  if (n_both > n_exposed || n_both > n_comparator || n_both_outcome > n_exposed_outcome || n_both_outcome > n_comparator_outcome) {
    stop("The intersection is larger than one of the groups being compared.")
  }
  if (n_both_outcome > n_both) {
    stop("The intersection has more outcome reports than reports.")
  }
  n_e <- n_exposed - n_both
  n_c <- n_comparator - n_both
  a <- n_exposed_outcome - n_both_outcome
  c <- n_comparator_outcome - n_both_outcome
  if (a > n_e || c > n_c) {
    stop("Outcome reports exceed the exclusive group.")
  }
  fit <- two_by_two(a, n_e - a, c, n_c - c)
  fit$n_exposed <- n_e
  fit$n_comparator <- n_c
  fit$n_both <- n_both
  fit
}
