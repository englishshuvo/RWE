# Aggregate outputs. No report-level rows.

signal_clause <- function(flag) {
  if (isTRUE(flag)) "The contrast meets the signal rule." else "The contrast does not meet the signal rule."
}

one_row <- function(contrasts, comparison, outcome, period) {
  hit <- contrasts[
    contrasts$comparison == comparison &
      contrasts$outcome == outcome &
      contrasts$period == period,
    ,
    drop = FALSE
  ]
  if (nrow(hit) != 1L) stop("Expected one contrast row for ", comparison, " ", outcome, " ", period)
  hit
}

headline_for <- function(contrasts, outcome) {
  row <- one_row(contrasts, "jak_vs_tnf", outcome, "full")
  corr <- if (isTRUE(row$continuity_correction)) {
    " A continuity correction of 0.5 was added to each cell."
  } else {
    ""
  }
  sprintf(
    "JAK versus TNF, %s: ROR %s (95%% CI %s to %s), PRR %s, from %s JAK reports with the outcome among %s JAK reports and %s TNF reports with the outcome among %s TNF reports. %s%s",
    OUTCOME_SENTENCE[[outcome]],
    row$ror_fmt, row$ror_low_fmt, row$ror_high_fmt, row$prr_fmt,
    fmt_int(row$a), fmt_int(row$n_exposed), fmt_int(row$c), fmt_int(row$n_comparator),
    signal_clause(row$signal),
    corr
  )
}

prepost_for <- function(contrasts, outcome) {
  pre <- one_row(contrasts, "jak_vs_tnf", outcome, "pre")
  post <- one_row(contrasts, "jak_vs_tnf", outcome, "post")
  sprintf(
    "For %s, the JAK versus TNF ROR was %s (95%% CI %s to %s) before 1 September 2021 and %s (95%% CI %s to %s) on or after that date.",
    OUTCOME_SENTENCE[[outcome]],
    pre$ror_fmt, pre$ror_low_fmt, pre$ror_high_fmt,
    post$ror_fmt, post$ror_low_fmt, post$ror_high_fmt
  )
}

data_sentence <- function(context, totals) {
  sprintf(
    "openFDA drug event API counts of FDA FAERS reports received from 1 January 2019 through 30 September 2026, covering 2019 Q1 through 2026 Q3. The API last_updated field was %s. Counts were retrieved %s. September 2026 had %s reports and October 2026 had %s reports.",
    fmt_date(context$stamp$last_updated),
    fmt_date(as.Date(substr(context$finished_at, 1, 10))),
    fmt_int(total_of(totals, "month__202609__all")),
    fmt_int(total_of(totals, "month__202610__all"))
  )
}

cohort_sentence <- function(flow) {
  val <- function(step) flow$reports[match(step, flow$step)]
  sprintf(
    "With an immunology indication, %s reports named a JAK inhibitor and %s named a TNF inhibitor. %s named both and are left out of the contrast, leaving %s JAK reports and %s TNF reports.",
    fmt_int(val("JAK ingredient and an immunology indication")),
    fmt_int(val("TNF ingredient and an immunology indication")),
    fmt_int(val("Both a JAK and a TNF ingredient, with an immunology indication")),
    fmt_int(val("JAK only, with an immunology indication")),
    fmt_int(val("TNF only, with an immunology indication"))
  )
}

unrestricted_sentence <- function(flow) {
  val <- function(step) flow$reports[match(step, flow$step)]
  sprintf(
    "Without the indication restriction, the same window has %s JAK reports and %s TNF reports.",
    fmt_int(val("JAK ingredient, indication not required")),
    fmt_int(val("TNF ingredient, indication not required"))
  )
}

window_sentence <- function(flow) {
  sprintf(
    "The openFDA index holds %s reports with a received date in that window.",
    fmt_int(flow$reports[match("Reports received in the window", flow$step)])
  )
}

build_key_strings <- function(contrasts, flow, context, totals) {
  c(
    data_sentence(context, totals),
    window_sentence(flow),
    unrestricted_sentence(flow),
    cohort_sentence(flow),
    headline_for(contrasts, "vte"),
    headline_for(contrasts, "infection"),
    prepost_for(contrasts, "vte"),
    prepost_for(contrasts, "infection")
  )
}

md_table <- function(contrasts, periods) {
  sub <- contrasts[contrasts$period %in% periods, , drop = FALSE]
  lines <- c(
    "| Contrast | Period | Outcome | JAK with outcome | JAK reports | TNF with outcome | TNF reports | ROR | 95% CI | PRR | Signal |",
    "| --- | --- | --- | ---: | ---: | ---: | ---: | ---: | --- | ---: | --- |"
  )
  for (i in seq_len(nrow(sub))) {
    row <- sub[i, ]
    ror_cell <- row$ror_fmt
    if (isTRUE(row$continuity_correction)) ror_cell <- paste0(ror_cell, " (corrected)")
    lines <- c(lines, sprintf(
      "| %s | %s | %s | %s | %s | %s | %s | %s | %s to %s | %s | %s |",
      row$comparison_label,
      row$period_label,
      row$outcome_label,
      fmt_int(row$a),
      fmt_int(row$n_exposed),
      fmt_int(row$c),
      fmt_int(row$n_comparator),
      ror_cell,
      row$ror_low_fmt,
      row$ror_high_fmt,
      row$prr_fmt,
      if (isTRUE(row$signal)) "yes" else "no"
    ))
  }
  lines
}

write_results_markdown <- function(contrasts, flow, path) {
  flow_lines <- c(
    "| Step | Reports |",
    "| --- | ---: |",
    sprintf("| %s | %s |", flow$step, fmt_int(flow$reports))
  )
  lines <- c(
    "### Report counts",
    "",
    flow_lines,
    "",
    "### Full window",
    "",
    md_table(contrasts, "full"),
    "",
    "### Before and after 1 September 2021",
    "",
    md_table(contrasts, c("pre", "post")),
    ""
  )
  writeLines(lines, path)
}

pkg_version <- function(pkg) as.character(utils::packageVersion(pkg))

write_outputs <- function(out_dir, provenance, totals, contrasts, quarterly, flow, key_strings, readme_strings) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(totals, file.path(out_dir, "query-totals.csv"), row.names = FALSE)
  utils::write.csv(contrasts, file.path(out_dir, "contrasts.csv"), row.names = FALSE)
  utils::write.csv(quarterly, file.path(out_dir, "quarterly-vte.csv"), row.names = FALSE)
  utils::write.csv(flow, file.path(out_dir, "cohort-flow.csv"), row.names = FALSE)
  provenance$query_totals_sha256 <- sha256_file(file.path(out_dir, "query-totals.csv"))
  # query_totals_sha256 is the SHA-256 of the committed totals file. Each query
  # row also carries the SHA-256 of the openFDA response body.
  jsonlite::write_json(
    provenance,
    file.path(out_dir, "provenance.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA,
    na = "null"
  )
  payload <- list(contrasts = contrasts, quarterly = quarterly, flow = flow)
  jsonlite::write_json(
    payload,
    file.path(out_dir, "estimates.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA,
    na = "null"
  )
  write_results_markdown(contrasts, flow, file.path(out_dir, "results-fragment.md"))
  writeLines(key_strings, file.path(out_dir, "key-strings.txt"))
  writeLines(readme_strings, file.path(out_dir, "readme-strings.txt"))
  writeLines(utils::capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))
}

check_case_study <- function(root) {
  keys <- readLines(file.path(root, "output", "key-strings.txt"), warn = FALSE)
  keys <- keys[nzchar(keys)]
  case_path <- file.path(root, "docs", "case-study.qmd")
  case_txt <- paste(readLines(case_path, warn = FALSE), collapse = "\n")
  missing <- keys[!vapply(keys, function(k) grepl(k, case_txt, fixed = TRUE), logical(1))]
  if (length(missing)) {
    stop("Case study is missing ", length(missing), " key string(s). First: ", missing[[1]])
  }
  readme_keys <- readLines(file.path(root, "output", "readme-strings.txt"), warn = FALSE)
  readme_keys <- readme_keys[nzchar(readme_keys)]
  readme_path <- file.path(dirname(root), "README.md")
  readme <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  missing_r <- readme_keys[!vapply(readme_keys, function(k) grepl(k, readme, fixed = TRUE), logical(1))]
  if (length(missing_r)) stop("Root README is missing: ", missing_r[[1]])
  study_readme <- paste(readLines(file.path(root, "README.md"), warn = FALSE), collapse = "\n")
  missing_s <- readme_keys[!vapply(readme_keys, function(k) grepl(k, study_readme, fixed = TRUE), logical(1))]
  if (length(missing_s)) stop("Study README is missing: ", missing_s[[1]])
  figs <- c(
    "figure-forest-ror.png",
    "figure-vte-quarterly.png",
    "figure-pre-post-ror.png",
    "figure-heatmap-ror.png"
  )
  for (fig in figs) {
    fig_path <- file.path(root, "output", fig)
    if (!file.exists(fig_path)) stop("Missing figure ", fig)
    info <- file.info(fig_path)
    if (is.na(info$size) || info$size < 10000) stop("Figure looks empty: ", fig)
    if (!grepl(fig, case_txt, fixed = TRUE)) stop("Case study does not embed ", fig)
  }
  if (!grepl("figure-forest-ror.png", readme, fixed = TRUE)) {
    stop("Root README does not embed the forest plot.")
  }
  if (!grepl("figure-forest-ror.png", study_readme, fixed = TRUE)) {
    stop("Study README does not embed the forest plot.")
  }
  message("Case study and README contain the key strings and the figures.")
}
