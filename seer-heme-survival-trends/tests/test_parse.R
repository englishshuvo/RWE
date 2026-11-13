# Parser tests on a fabricated body. These rates are not SEER rates.
extra_lib <- path.expand("~/R/library")
if (dir.exists(extra_lib)) .libPaths(unique(c(extra_lib, .libPaths())))
r_libs <- Sys.getenv("R_LIBS", unset = "")
if (nzchar(r_libs)) .libPaths(unique(c(strsplit(r_libs, .Platform$path.sep, fixed = TRUE)[[1]], .libPaths())))
args_root <- Sys.getenv("SEER_STUDY_ROOT", unset = "")
if (!nzchar(args_root)) {
  cand <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
  if (file.exists(file.path(cand, "R", "parse.R"))) {
    Sys.setenv(SEER_STUDY_ROOT = cand)
  } else {
    Sys.setenv(SEER_STUDY_ROOT = normalizePath(getwd()))
  }
}
root <- Sys.getenv("SEER_STUDY_ROOT")
source(file.path(root, "R", "constants.R"))
source(file.path(root, "R", "parse.R"))
source(file.path(root, "tests", "helper.R"))

message("Parse tests on a fabricated explorer body. Not SEER estimates.")

# Field order is year, count, rate, so a parser that assumes column 2 is the rate would fail.
inner <- '{
  "info": {
    "key-order": ["rate_type", "sex", "site"],
    "data-fields": ["year", "count", "rate", "rate_lower_ci", "rate_upper_ci", "modeled_rate"],
    "trend-fields": ["year_start", "trend_type", "num_segments", "seg_number", "year_end", "apc", "apc_lower_ci", "apc_upper_ci", "apc_significance", "apc_pvalue"]
  },
  "data": {
    "1_2_89": {
      "data_series": [[2000, 40, "3.5", "3.0", "4.0", "3.6"]],
      "trend_data": [[2000, 2, 1, 0, 2001, "1.25", "0.5", "2.0", true, "0.04"]],
      "year_range": 3
    }
  }
}'
text <- jsonlite::toJSON(inner, auto_unbox = TRUE)
query <- list(cancer = "myeloma", measure = "incidence", slice = "sex")
got <- parse_query_text(text, query)
expect_equal(nrow(got$rates), 1L, "one fabricated rate row")
expect_equal(got$rates$year, 2000L, "year comes from the year field")
expect_equal(got$rates$estimate, 3.5, "rate is read by field name, not by position 2")
expect_equal(got$rates$count, 40, "count is read by field name")
expect_equal(got$rates$sex, 2L, "sex code comes from the key")
expect_equal(got$rates$rate_type, 1L, "rate type comes from the key")
expect_equal(got$rates$site, 89L, "site comes from the key")
expect_equal(got$trends$estimate, 1.25, "trend estimate is the apc field")
expect_true(isTRUE(got$trends$significant), "significance flag is kept")
expect_equal(got$trends$p_value, 0.04, "p value is kept when the field is present")

surv_inner <- '{
  "info": {
    "key-order": ["relative_survival_interval", "sex", "race", "age_range", "stage", "site"],
    "data-fields": ["year", "rel_rate", "rel_rate_se", "rel_rate_lower_ci", "rel_rate_upper_ci", "modeled_rel_rate", "count"],
    "trend-fields": ["year_start", "trend_type", "num_segments", "seg_number", "year_end", "apc", "apc_lower_ci", "apc_upper_ci", "apc_significance"]
  },
  "data": {
    "5_1_1_1_101_89": {
      "data_series": [
        [2000, "40", "1", "38", "42", "41", 100],
        [2001, null, null, null, null, "99", null]
      ],
      "trend_data": [[2000, 2, 1, 1, 2001, "1.2", "0.4", "2.0", true]],
      "year_range": 33
    }
  }
}'
surv_text <- jsonlite::toJSON(surv_inner, auto_unbox = TRUE)
surv_query <- list(cancer = "myeloma", measure = "survival", slice = "sex")
surv <- parse_query_text(surv_text, surv_query)
expect_equal(nrow(surv$rates), 2L, "observed and empty year are both parsed")
expect_equal(surv$rates$estimate[1], 40, "observed relative survival is the estimate")
expect_true(is.na(surv$rates$estimate[2]), "a null observed rate stays missing")
expect_equal(surv$rates$modeled[2], 99, "the modeled value stays in its own column")
expect_true(is.na(surv$rates$count[2]), "a null count stays missing")
expect_true(is.na(surv$trends$p_value), "a survival trend without a p value is missing, not invented")

finish_tests()
