# Filters, the survival gap, and the sentences that have to match the files.

fmt1 <- function(x) {
  y <- formatC(x, format = "f", digits = 1)
  y[y == "-0.0"] <- "0.0"
  y
}

fmt2 <- function(x) {
  y <- formatC(x, format = "f", digits = 2)
  y[y == "-0.00"] <- "0.00"
  y
}

fmt_int <- function(x) {
  prettyNum(as.integer(round(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_date <- function(x) {
  d <- as.Date(x)
  sprintf("%d %s %d", as.integer(format(d, "%d")), format(d, "%B"), as.integer(format(d, "%Y")))
}

label_of <- function(map, code) {
  key <- as.character(code)
  if (!key %in% names(map)) stop("No label for code ", key, ".")
  unname(map[[key]])
}

publishable <- function(estimate, count) {
  is.finite(estimate) & is.finite(count) & count >= MIN_COUNT
}

# Observed 5-year relative survival only. A modeled rate with no observed
# rate is a projection and is not copied into the estimate.
keep_observed <- function(df) {
  df[publishable(df$estimate, df$count), , drop = FALSE]
}

direction_word <- function(estimate, significant) {
  if (!isTRUE(significant)) return("not significant")
  if (is.finite(estimate) && estimate > 0) return("rising")
  if (is.finite(estimate) && estimate < 0) return("falling")
  "not significant"
}

subset_dims <- function(df, cancers = SITE_ORDER, measure = NULL, slice = NULL,
                        sex = NULL, race = NULL, age = NULL, rate_type = NULL) {
  keep <- df$cancer %in% cancers
  if (!is.null(measure)) keep <- keep & df$measure == measure
  if (!is.null(slice)) keep <- keep & df$slice %in% slice
  if (!is.null(sex)) keep <- keep & df$sex %in% sex
  if (!is.null(race)) keep <- keep & df$race %in% race
  if (!is.null(age)) keep <- keep & df$age_range %in% age
  if (!is.null(rate_type)) keep <- keep & df$rate_type %in% rate_type
  df[keep, , drop = FALSE]
}

prepare_rates <- function(rates) {
  inc <- subset_dims(rates, measure = "incidence")
  inc <- inc[inc$stage == 101L, , drop = FALSE]
  # All-ages, all-races rates come from the sex slice. The race and age
  # slices keep the other groups only, so the overall row is stored once.
  inc_sex <- subset_dims(inc, slice = "sex", sex = SEX_KEEP, race = 1L, age = 1L, rate_type = 1L)
  inc_race <- subset_dims(inc, slice = "race", sex = 1L, race = setdiff(RACE_KEEP, 1L), age = 1L, rate_type = 1L)
  inc_age <- subset_dims(inc, slice = "age", sex = 1L, race = 1L, age = c(9L, 141L, 157L), rate_type = 1L)
  inc_delay <- subset_dims(inc, slice = "delay", sex = 1L, race = 1L, age = 1L, rate_type = 2L)
  incidence <- keep_observed(rbind(inc_sex, inc_race, inc_age, inc_delay))

  surv <- subset_dims(rates, measure = "survival")
  surv <- surv[surv$stage == 101L, , drop = FALSE]
  surv_sex <- subset_dims(surv, slice = "sex", sex = SEX_KEEP, race = 1L, age = 1L)
  surv_race <- subset_dims(surv, slice = "race", sex = 1L, race = c(2L, 3L), age = 1L)
  surv_age <- subset_dims(surv, slice = "age", sex = 1L, race = 1L, age = c(9L, 141L, 157L))
  survival <- keep_observed(rbind(surv_sex, surv_race, surv_age))

  list(incidence = incidence, survival = survival)
}

prepare_trends <- function(trends) {
  inc <- subset_dims(
    trends, measure = "incidence", slice = c("sex", "delay"),
    sex = 1L, race = 1L, age = 1L, rate_type = c(1L, 2L)
  )
  inc <- inc[inc$trend_type == 2L, , drop = FALSE]
  surv <- subset_dims(
    trends, measure = "survival", slice = "sex",
    sex = 1L, race = 1L, age = 1L
  )
  rbind(inc, surv)
}

one_cell <- function(df, cancer, year, sex = 1L, race = 1L, age = 1L, rate_type = NULL) {
  hit <- df[df$cancer == cancer & df$year == year & df$sex == sex & df$race == race & df$age_range == age, , drop = FALSE]
  if (!is.null(rate_type)) hit <- hit[hit$rate_type == rate_type, , drop = FALSE]
  hit
}

require_cell <- function(df, cancer, year, ...) {
  hit <- one_cell(df, cancer, year, ...)
  if (nrow(hit) != 1L) {
    stop("Expected one published cell for ", cancer, " in ", year, ". Found ", nrow(hit), ".")
  }
  hit
}

assert_required <- function(incidence, survival) {
  for (cancer in SITE_ORDER) {
    require_cell(incidence, cancer, INCIDENCE_START, rate_type = 1L)
    require_cell(incidence, cancer, INCIDENCE_END, rate_type = 1L)
    require_cell(incidence, cancer, INCIDENCE_END, rate_type = 2L)
    require_cell(survival, cancer, INCIDENCE_START)
    last_year <- max(survival$year[survival$cancer == cancer & survival$sex == 1L & survival$race == 1L & survival$age_range == 1L])
    require_cell(survival, cancer, last_year)
    require_cell(survival, cancer, INCIDENCE_START, race = 2L)
    require_cell(survival, cancer, INCIDENCE_START, race = 3L)
    require_cell(survival, cancer, last_year, race = 2L)
    require_cell(survival, cancer, last_year, race = 3L)
  }
  invisible(TRUE)
}

survival_gap <- function(survival) {
  rows <- list()
  for (cancer in SITE_ORDER) {
    white <- subset_dims(survival, cancers = cancer, sex = 1L, race = 2L, age = 1L)
    black <- subset_dims(survival, cancers = cancer, sex = 1L, race = 3L, age = 1L)
    years <- sort(intersect(white$year, black$year))
    for (year in years) {
      w <- white[white$year == year, , drop = FALSE]
      b <- black[black$year == year, , drop = FALSE]
      if (nrow(w) != 1L || nrow(b) != 1L) stop("Gap year ", year, " for ", cancer, " is not one row per group.")
      rows[[length(rows) + 1L]] <- data.frame(
        cancer = cancer,
        year = year,
        white_survival = w$estimate,
        black_survival = b$estimate,
        gap_pp = w$estimate - b$estimate,
        white_count = w$count,
        black_count = b$count,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

add_labels <- function(df) {
  df$cancer_label <- vapply(df$cancer, function(id) SITES[[id]]$label, character(1))
  df$sex_label <- vapply(df$sex, function(code) label_of(SEX_LABELS, code), character(1))
  df$race_label <- vapply(df$race, function(code) label_of(RACE_LABELS, code), character(1))
  df$age_label <- vapply(df$age_range, function(code) label_of(AGE_LABELS, code), character(1))
  if ("rate_type" %in% names(df)) {
    df$rate_type_label <- NA_character_
    ok <- !is.na(df$rate_type)
    if (any(ok)) {
      df$rate_type_label[ok] <- vapply(
        df$rate_type[ok],
        function(code) label_of(RATE_TYPE_LABELS, code),
        character(1)
      )
    }
  }
  if ("year" %in% names(df)) df$covid_year <- df$year == COVID_YEAR
  df
}

order_cancers <- function(df) {
  df$cancer <- factor(df$cancer, levels = SITE_ORDER)
  df[order(df$cancer, df$year), , drop = FALSE]
}
