# Gap, suppression, and labels. The rows are fabricated.
args_root <- Sys.getenv("SEER_STUDY_ROOT", unset = "")
if (!nzchar(args_root)) {
  cand <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
  if (file.exists(file.path(cand, "R", "measures.R"))) {
    Sys.setenv(SEER_STUDY_ROOT = cand)
  } else {
    Sys.setenv(SEER_STUDY_ROOT = normalizePath(getwd()))
  }
}
root <- Sys.getenv("SEER_STUDY_ROOT")
source(file.path(root, "R", "constants.R"))
source(file.path(root, "R", "measures.R"))
source(file.path(root, "tests", "helper.R"))

message("Measure tests on fabricated rows. Not SEER estimates.")

expect_true(publishable(10, 16), "a count of 16 is kept")
expect_true(!publishable(10, 15), "a count of 15 is dropped")
expect_true(!publishable(NA_real_, 100), "a missing estimate is dropped")
expect_true(!publishable(99, NA_real_), "a modeled stand-in with no count is dropped")

raw <- data.frame(
  cancer = c("myeloma", "myeloma", "myeloma", "myeloma"),
  measure = "survival",
  slice = "sex",
  rate_type = NA_integer_,
  sex = 1L,
  race = c(2L, 2L, 3L, 3L),
  age_range = 1L,
  stage = 101L,
  site = 89L,
  year = c(2000L, 2001L, 2000L, 2002L),
  estimate = c(80, 82, 70, 71),
  std_error = NA_real_,
  ci_low = NA_real_,
  ci_high = NA_real_,
  modeled = c(81, 83, 71, 72),
  count = c(100, 100, 100, 100),
  stringsAsFactors = FALSE
)
kept <- keep_observed(raw)
expect_equal(nrow(kept), 4L, "complete fabricated rows are kept")
gap <- survival_gap(kept)
expect_equal(nrow(gap), 1L, "the gap keeps only a year present in both groups")
expect_equal(gap$year, 2000L, "shared year is 2000")
expect_equal(gap$gap_pp, 10, "gap is white minus black")
expect_true(gap$gap_pp == (gap$white_survival - gap$black_survival), "gap column matches the two rates")

tiny <- raw[1, , drop = FALSE]
tiny$count <- 15
expect_equal(nrow(keep_observed(tiny)), 0L, "the small cell does not survive the filter")

labeled <- add_labels(raw[1, , drop = FALSE])
expect_equal(labeled$race_label, "White (includes Hispanic)", "race 2 label")
expect_equal(labeled$cancer_label, "Multiple myeloma", "cancer label")
expect_true(isFALSE(labeled$covid_year), "2000 is not the COVID year")

words <- c(
  direction_word(1.2, TRUE),
  direction_word(-0.4, TRUE),
  direction_word(1.2, FALSE)
)
expect_equal(words, c("rising", "falling", "not significant"), "direction follows the SEER flag and the sign")

finish_tests()
