# Sentences, tables, and provenance. The case study has to contain the sentences.

strip_html <- function(x) {
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

pkg_version <- function(pkg) as.character(utils::packageVersion(pkg))

read_json_file <- function(path) {
  jsonlite::fromJSON(paste(readLines(path, warn = FALSE), collapse = "\n"), simplifyVector = FALSE, simplifyDataFrame = FALSE)
}

expect_code <- function(got, expected, label) {
  if (!identical(got, expected)) {
    stop(label, " is '", got, "' in the codebook. The locked label is '", expected, "'.")
  }
}

check_release <- function(cache_dir) {
  codebook <- read_json_file(file.path(cache_dir, "codebook.json"))
  sites <- codebook$VariableFormats$site
  sexes <- codebook$VariableFormats$sex
  races <- codebook$VariableFormats$race
  ages <- codebook$VariableFormats$age_range
  for (cancer in SITE_ORDER) {
    expect_code(sites[[as.character(SITES[[cancer]]$id)]], SITES[[cancer]]$seer_label, SITES[[cancer]]$label)
  }
  expect_code(sexes[["1"]], "Both Sexes", "sex 1")
  expect_code(sexes[["2"]], "Male", "sex 2")
  expect_code(sexes[["3"]], "Female", "sex 3")
  expect_code(races[["2"]], "White (includes Hispanic)", "race 2")
  expect_code(races[["3"]], "Black (includes Hispanic)", "race 3")
  expect_code(races[["8"]], "Non-Hispanic White", "race 8")
  expect_code(races[["9"]], "Non-Hispanic Black", "race 9")
  expect_code(races[["6"]], "Hispanic (any race)", "race 6")
  expect_code(races[["4"]], "Non-Hispanic Asian / Pacific Islander", "race 4")
  expect_code(races[["5"]], "Non-Hispanic American Indian / Alaska Native", "race 5")
  expect_code(ages[["1"]], "All Ages", "age 1")
  expect_code(ages[["9"]], "Ages < 50", "age 9")
  expect_code(ages[["141"]], "Ages 50-64", "age 141")
  expect_code(ages[["157"]], "Ages 65+", "age 157")

  footnotes <- read_json_file(file.path(cache_dir, "footnotes.json"))
  texts <- vapply(footnotes, function(x) strip_html(x$fn_html %||% ""), character(1))
  notes <- read_json_file(file.path(cache_dir, "notes.json"))
  covid <- strip_html(notes$COVID_2020_NOTE %||% "")
  require_phrase <- function(hay, needle) {
    if (!any(grepl(needle, hay, fixed = TRUE))) stop("Release text is missing this locked phrase: ", needle)
  }
  require_phrase(texts, LOCK_RELEASE)
  require_phrase(texts, LOCK_INCIDENCE_REGISTRY)
  require_phrase(texts, LOCK_SURVIVAL_REGISTRY)
  require_phrase(texts, LOCK_STD_POP)
  require_phrase(texts, LOCK_JOINPOINT)
  require_phrase(texts, LOCK_JOINPOINT_VERSION)
  require_phrase(texts, LOCK_AACS)
  require_phrase(covid, LOCK_COVID)
  list(footnotes = texts, covid = covid)
}

`%||%` <- function(x, y) if (is.null(x) || !nzchar(x)) y else x

last_survival_year <- function(survival, cancer) {
  years <- survival$year[survival$cancer == cancer & survival$sex == 1L & survival$race == 1L & survival$age_range == 1L]
  max(years)
}

data_sentence <- function(retrieved_on, last_year) {
  sprintf(
    "SEER*Explorer aggregate rates from the SEER November 2025 submission, retrieved %s. Incidence is the SEER 21 registries, age-adjusted to the 2000 US standard population, for 2000 through 2023. Five-year relative survival is the SEER 21 registries excluding Illinois. The last diagnosis year with an observed 5-year relative survival in this download is %d.",
    fmt_date(retrieved_on),
    last_year
  )
}

dlbcl_sentence <- function() {
  "Diffuse large B-cell lymphoma is not a separate cancer site in SEER*Explorer. The lymphoma series in this study is non-Hodgkin lymphoma."
}

causal_sentence <- function() {
  "The approval lines are context. SEER does not record the drug a person received, so a change in survival after an approval is not evidence that the drug caused it."
}

survival_sentence <- function(survival, cancer) {
  y1 <- last_survival_year(survival, cancer)
  a <- require_cell(survival, cancer, INCIDENCE_START)
  b <- require_cell(survival, cancer, y1)
  sprintf(
    "%s 5-year relative survival was %s%% for people diagnosed in %d (%s cases) and %s%% for people diagnosed in %d (%s cases).",
    SITES[[cancer]]$label,
    fmt1(a$estimate), a$year, fmt_int(a$count),
    fmt1(b$estimate), b$year, fmt_int(b$count)
  )
}

incidence_sentence <- function(incidence) {
  cell <- function(cancer, year) require_cell(incidence, cancer, year, rate_type = 1L)
  sprintf(
    "Observed age-adjusted incidence in 2023 was %s per 100,000 for myeloma (%s cases), %s for CLL (%s cases), and %s for non-Hodgkin lymphoma (%s cases). In 2000 those rates were %s, %s, and %s.",
    fmt1(cell("myeloma", 2023)$estimate), fmt_int(cell("myeloma", 2023)$count),
    fmt1(cell("cll", 2023)$estimate), fmt_int(cell("cll", 2023)$count),
    fmt1(cell("nhl", 2023)$estimate), fmt_int(cell("nhl", 2023)$count),
    fmt1(cell("myeloma", 2000)$estimate),
    fmt1(cell("cll", 2000)$estimate),
    fmt1(cell("nhl", 2000)$estimate)
  )
}

delay_sentence <- function(incidence) {
  cell <- function(cancer) require_cell(incidence, cancer, 2023, rate_type = 2L)
  sprintf(
    "Delay-adjusted incidence in 2023 was %s per 100,000 for myeloma, %s for CLL, and %s for non-Hodgkin lymphoma. The delay-adjusted rate sits above the observed rate in the most recent year.",
    fmt1(cell("myeloma")$estimate),
    fmt1(cell("cll")$estimate),
    fmt1(cell("nhl")$estimate)
  )
}

gap_ends <- function(gap, cancer) {
  sub <- gap[gap$cancer == cancer, , drop = FALSE]
  sub <- sub[order(sub$year), , drop = FALSE]
  list(first = sub[1, , drop = FALSE], last = sub[nrow(sub), , drop = FALSE])
}

gap_sentence <- function(gap) {
  bits <- vapply(SITE_ORDER, function(cancer) {
    ends <- gap_ends(gap, cancer)
    extra <- ""
    if (ends$first$gap_pp < 0 && ends$last$gap_pp < 0) {
      extra <- ", so the Black group had the higher survival in both years"
    }
    prose <- c(
      myeloma = "multiple myeloma",
      cll = "CLL",
      nhl = "non-Hodgkin lymphoma"
    )
    sprintf(
      "For %s the gap was %s percentage points in %d and %s percentage points in %d%s.",
      prose[[cancer]],
      fmt1(ends$first$gap_pp), ends$first$year,
      fmt1(ends$last$gap_pp), ends$last$year,
      extra
    )
  }, character(1))
  paste(
    "The 5-year relative survival gap is White (includes Hispanic) minus Black (includes Hispanic).",
    paste(bits, collapse = " ")
  )
}

apc_sentence <- function(trends, cancer, rate_type, kind_label) {
  sub <- trends[trends$cancer == cancer & trends$measure == "incidence" & trends$rate_type == rate_type, , drop = FALSE]
  sub <- sub[order(sub$year_start, sub$seg_number), , drop = FALSE]
  if (!nrow(sub)) stop("No incidence segments for ", cancer, " rate type ", rate_type)
  parts <- vapply(seq_len(nrow(sub)), function(i) {
    s <- sub[i, , drop = FALSE]
    sprintf(
      "%d-%d APC %s%% per year (%s)",
      s$year_start, s$year_end, fmt1(s$estimate), direction_word(s$estimate, s$significant)
    )
  }, character(1))
  sprintf("%s %s, SEER joinpoint: %s.", SITES[[cancer]]$short, kind_label, paste(parts, collapse = "; "))
}

aacs_sentence <- function(trends, cancer) {
  sub <- trends[trends$cancer == cancer & trends$measure == "survival", , drop = FALSE]
  sub <- sub[order(sub$year_start, sub$seg_number), , drop = FALSE]
  if (!nrow(sub)) stop("No survival segments for ", cancer)
  parts <- vapply(seq_len(nrow(sub)), function(i) {
    s <- sub[i, , drop = FALSE]
    sprintf(
      "%d-%d, %s percentage points per year (%s)",
      s$year_start, s$year_end, fmt1(s$estimate), direction_word(s$estimate, s$significant)
    )
  }, character(1))
  sprintf("%s 5-year relative survival, SEER average absolute change: %s.", SITES[[cancer]]$short, paste(parts, collapse = "; "))
}

method_sentence <- function() {
  "SEER fit the incidence trends in Joinpoint version 6.0 and left the 2020 rate out of the fit. The published APC is an annual percent change. The survival summary is an average absolute change in percentage points per year, not an annual percent change. A survival segment can end after the last observed 5-year rate because the model uses shorter follow-up."
}

build_key_strings <- function(incidence, survival, gap, trends, retrieved_on) {
  last_year <- last_survival_year(survival, "myeloma")
  if (!all(vapply(SITE_ORDER, function(id) last_survival_year(survival, id) == last_year, logical(1)))) {
    stop("The three cancers do not share a last observed survival year.")
  }
  c(
    data_sentence(retrieved_on, last_year),
    dlbcl_sentence(),
    causal_sentence(),
    survival_sentence(survival, "myeloma"),
    survival_sentence(survival, "cll"),
    survival_sentence(survival, "nhl"),
    incidence_sentence(incidence),
    delay_sentence(incidence),
    gap_sentence(gap),
    method_sentence(),
    apc_sentence(trends, "myeloma", 1L, "observed incidence"),
    apc_sentence(trends, "cll", 1L, "observed incidence"),
    apc_sentence(trends, "nhl", 1L, "observed incidence"),
    apc_sentence(trends, "myeloma", 2L, "delay-adjusted incidence"),
    aacs_sentence(trends, "myeloma"),
    aacs_sentence(trends, "cll"),
    aacs_sentence(trends, "nhl")
  )
}

build_readme_strings <- function(key_strings) {
  key_strings[c(1, 2, 4, 5, 6, 7, 9)]
}

md_table <- function(headers, rows) {
  align <- rep("---", length(headers))
  c(
    paste0("| ", paste(headers, collapse = " | "), " |"),
    paste0("| ", paste(align, collapse = " | "), " |"),
    vapply(rows, function(cells) paste0("| ", paste(cells, collapse = " | "), " |"), character(1))
  )
}

rate_cells <- function(row) {
  c(fmt2(row$estimate), sprintf("%s to %s", fmt2(row$ci_low), fmt2(row$ci_high)), fmt_int(row$count))
}

incidence_table <- function(incidence) {
  headers <- c("Cancer", "Group", "2000 rate", "2000 95% CI", "2000 cases", "2023 rate", "2023 95% CI", "2023 cases")
  specs <- list(
    list("Both sexes, observed", sex = 1L, race = 1L, age = 1L, rate_type = 1L),
    list("Both sexes, delay-adjusted", sex = 1L, race = 1L, age = 1L, rate_type = 2L),
    list("Male, observed", sex = 2L, race = 1L, age = 1L, rate_type = 1L),
    list("Female, observed", sex = 3L, race = 1L, age = 1L, rate_type = 1L),
    list("Ages <50, observed", sex = 1L, race = 1L, age = 9L, rate_type = 1L),
    list("Ages 50-64, observed", sex = 1L, race = 1L, age = 141L, rate_type = 1L),
    list("Ages 65+, observed", sex = 1L, race = 1L, age = 157L, rate_type = 1L),
    list("Non-Hispanic White", sex = 1L, race = 8L, age = 1L, rate_type = 1L),
    list("Non-Hispanic Black", sex = 1L, race = 9L, age = 1L, rate_type = 1L),
    list("Hispanic, any race", sex = 1L, race = 6L, age = 1L, rate_type = 1L),
    list("Non-Hispanic Asian / Pacific Islander", sex = 1L, race = 4L, age = 1L, rate_type = 1L)
  )
  rows <- list()
  for (cancer in SITE_ORDER) {
    for (spec in specs) {
      a <- one_cell(incidence, cancer, 2000, sex = spec$sex, race = spec$race, age = spec$age, rate_type = spec$rate_type)
      b <- one_cell(incidence, cancer, 2023, sex = spec$sex, race = spec$race, age = spec$age, rate_type = spec$rate_type)
      if (nrow(a) != 1L || nrow(b) != 1L) next
      rows[[length(rows) + 1L]] <- c(
        SITES[[cancer]]$short, spec[[1]],
        rate_cells(a), rate_cells(b)
      )
    }
  }
  md_table(headers, rows)
}

survival_table <- function(survival) {
  headers <- c("Cancer", "Group", "Start year", "Start percent", "Start 95% CI", "Start cases", "End year", "End percent", "End 95% CI", "End cases")
  specs <- list(
    list("Both sexes", sex = 1L, race = 1L, age = 1L),
    list("Male", sex = 2L, race = 1L, age = 1L),
    list("Female", sex = 3L, race = 1L, age = 1L),
    list("Ages <50", sex = 1L, race = 1L, age = 9L),
    list("Ages 50-64", sex = 1L, race = 1L, age = 141L),
    list("Ages 65+", sex = 1L, race = 1L, age = 157L),
    list("White (includes Hispanic)", sex = 1L, race = 2L, age = 1L),
    list("Black (includes Hispanic)", sex = 1L, race = 3L, age = 1L)
  )
  rows <- list()
  for (cancer in SITE_ORDER) {
    for (spec in specs) {
      sub <- one_cell(survival, cancer, year = INCIDENCE_START, sex = spec$sex, race = spec$race, age = spec$age)
      # one_cell filters year, so pull the series without a year first
      series <- survival[
        survival$cancer == cancer & survival$sex == spec$sex &
          survival$race == spec$race & survival$age_range == spec$age,
        ,
        drop = FALSE
      ]
      if (!nrow(series)) next
      series <- series[order(series$year), , drop = FALSE]
      a <- series[1, , drop = FALSE]
      b <- series[nrow(series), , drop = FALSE]
      rows[[length(rows) + 1L]] <- c(
        SITES[[cancer]]$short, spec[[1]],
        as.character(a$year), fmt1(a$estimate), sprintf("%s to %s", fmt1(a$ci_low), fmt1(a$ci_high)), fmt_int(a$count),
        as.character(b$year), fmt1(b$estimate), sprintf("%s to %s", fmt1(b$ci_low), fmt1(b$ci_high)), fmt_int(b$count)
      )
    }
  }
  md_table(headers, rows)
}

trend_table <- function(trends) {
  headers <- c("Cancer", "Series", "Start", "End", "Estimate", "95% CI", "SEER direction")
  sub <- trends[order(match(trends$cancer, SITE_ORDER), trends$measure, trends$rate_type, trends$year_start), , drop = FALSE]
  rows <- lapply(seq_len(nrow(sub)), function(i) {
    s <- sub[i, , drop = FALSE]
    series <- if (s$measure == "survival") {
      "5-year survival, percentage points per year"
    } else if (s$rate_type == 2L) {
      "Delay-adjusted incidence, APC"
    } else {
      "Observed incidence, APC"
    }
    c(
      SITES[[s$cancer]]$short,
      series,
      as.character(s$year_start),
      as.character(s$year_end),
      fmt2(s$estimate),
      sprintf("%s to %s", fmt2(s$ci_low), fmt2(s$ci_high)),
      direction_word(s$estimate, s$significant)
    )
  })
  md_table(headers, rows)
}

results_fragment <- function(incidence, survival, trends) {
  c(
    "### Incidence, per 100,000",
    "",
    "Rates are age-adjusted to the 2000 US standard population. A row is omitted when 2000 or 2023 was suppressed.",
    "",
    incidence_table(incidence),
    "",
    "### 5-year relative survival, percent",
    "",
    "The end year is the last diagnosis year with an observed 5-year relative survival and a case count of at least 16.",
    "",
    survival_table(survival),
    "",
    "### SEER trend summaries",
    "",
    "Incidence estimates are annual percent changes. Survival estimates are average absolute changes in percentage points per year. SEER left 2020 out of the incidence fit.",
    "",
    trend_table(trends),
    ""
  )
}

write_rate_csv <- function(df, path) {
  out <- add_labels(df)
  out <- out[order(match(out$cancer, SITE_ORDER), out$rate_type, out$sex, out$race, out$age_range, out$year), , drop = FALSE]
  keep <- c(
    "cancer", "cancer_label", "measure", "slice", "year",
    "sex", "sex_label", "race", "race_label", "age_range", "age_label",
    "rate_type", "rate_type_label", "estimate", "ci_low", "ci_high",
    "std_error", "count", "covid_year"
  )
  utils::write.csv(out[, keep], path, row.names = FALSE, na = "")
}

write_trend_csv <- function(trends, path) {
  out <- trends
  out$cancer_label <- vapply(out$cancer, function(id) SITES[[id]]$label, character(1))
  out$kind <- ifelse(out$measure == "survival", "AAC_S", "APC")
  out$direction <- mapply(direction_word, out$estimate, out$significant, USE.NAMES = FALSE)
  out <- out[order(match(out$cancer, SITE_ORDER), out$measure, out$rate_type, out$year_start, out$seg_number), , drop = FALSE]
  utils::write.csv(out, path, row.names = FALSE, na = "")
}

write_outputs <- function(out_dir, incidence, survival, gap, trends, key_strings, readme_strings, provenance) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write_rate_csv(incidence, file.path(out_dir, "incidence.csv"))
  write_rate_csv(survival, file.path(out_dir, "survival.csv"))
  gap_out <- gap
  gap_out$cancer_label <- vapply(gap_out$cancer, function(id) SITES[[id]]$label, character(1))
  utils::write.csv(gap_out, file.path(out_dir, "survival-gap.csv"), row.names = FALSE, na = "")
  write_trend_csv(trends, file.path(out_dir, "trends.csv"))
  writeLines(key_strings, file.path(out_dir, "key-strings.txt"))
  writeLines(readme_strings, file.path(out_dir, "readme-strings.txt"))
  writeLines(results_fragment(incidence, survival, trends), file.path(out_dir, "results-fragment.md"))
  jsonlite::write_json(provenance, file.path(out_dir, "provenance.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(utils::capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))
}

check_case_study <- function(root) {
  keys <- readLines(file.path(root, "output", "key-strings.txt"), warn = FALSE)
  keys <- keys[nzchar(keys)]
  case_path <- file.path(root, "docs", "case-study.qmd")
  case_txt <- paste(readLines(case_path, warn = FALSE), collapse = "\n")
  missing <- keys[!vapply(keys, function(k) grepl(k, case_txt, fixed = TRUE), logical(1))]
  if (length(missing)) stop("Case study is missing ", length(missing), " key string(s). First: ", missing[[1]])
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
    "figure-survival-trends.png",
    "figure-incidence-trends.png",
    "figure-survival-gap.png"
  )
  for (fig in figs) {
    fig_path <- file.path(root, "output", fig)
    if (!file.exists(fig_path)) stop("Missing figure ", fig)
    info <- file.info(fig_path)
    if (is.na(info$size) || info$size < 10000) stop("Figure looks empty: ", fig)
    if (!grepl(fig, case_txt, fixed = TRUE)) stop("Case study does not embed ", fig)
    if (!grepl(fig, readme, fixed = TRUE)) stop("Root README does not link ", fig)
    if (!grepl(fig, study_readme, fixed = TRUE)) stop("Study README does not link ", fig)
  }
  message("Case study and README contain the key strings and the figures.")
}
