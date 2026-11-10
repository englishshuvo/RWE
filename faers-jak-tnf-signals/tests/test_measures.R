# ROR, PRR, and the exclusive 2x2. None of these numbers are FAERS results.
args_root <- Sys.getenv("FAERS_STUDY_ROOT", unset = "")
if (!nzchar(args_root)) {
  cand <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
  if (file.exists(file.path(cand, "R", "measures.R"))) {
    Sys.setenv(FAERS_STUDY_ROOT = cand)
  } else {
    Sys.setenv(FAERS_STUDY_ROOT = normalizePath(getwd()))
  }
}
root <- Sys.getenv("FAERS_STUDY_ROOT")
source(file.path(root, "R", "constants.R"))
source(file.path(root, "R", "measures.R"))
source(file.path(root, "tests", "helper.R"))

message("Measure tests on fabricated 2x2 tables. Not FAERS estimates.")

# a=30, b=70, c=20, d=180
# ROR = 3.8571428571, lower = 2.0551708106, upper = 7.2390824859, PRR = 3
got <- two_by_two(30, 70, 20, 180)
expect_equal(got$ror, (30 * 180) / (70 * 20), "ROR is (a*d)/(b*c)")
expect_equal(got$prr, (30 / 100) / (20 / 200), "PRR is the ratio of outcome proportions")
se <- sqrt(1 / 30 + 1 / 70 + 1 / 20 + 1 / 180)
expect_equal(got$log_ror_se, se, "log ROR standard error")
expect_equal(got$ror_low, exp(log(got$ror) - 1.96 * se), "lower limit uses 1.96")
expect_equal(got$ror_high, exp(log(got$ror) + 1.96 * se), "upper limit uses 1.96")
expect_equal(round(got$ror, 6), 3.857143, "ROR matches the hand value 3.857143", tol = 1e-6)
expect_equal(round(got$ror_low, 6), 2.055171, "lower limit matches the hand value 2.055171", tol = 1e-6)
expect_equal(round(got$ror_high, 6), 7.239082, "upper limit matches the hand value 7.239082", tol = 1e-6)
expect_equal(got$prr, 3, "PRR is 3")
expect_true(isTRUE(got$signal), "lower limit above 1 and 30 cases is a signal")
expect_true(!got$continuity_correction, "a complete 2x2 is not corrected")
expect_true(identical(Z_95, 1.96), "the interval uses the locked 1.96")

# A zero cell adds 0.5 to every cell. Observed a is still 5, so the rule can fire.
# Corrected cells 5.5, 10.5, 0.5, 20.5. ROR 21.4761904762, lower 1.0809159893.
zero <- two_by_two(5, 10, 0, 20)
expect_true(zero$continuity_correction, "a zero cell turns the correction on")
expect_equal(zero$a, 5, "the observed case count is kept")
expect_equal(zero$c_used, 0.5, "the zero cell becomes 0.5")
expect_equal(zero$a_used, 5.5, "the other cells move by the same 0.5")
expect_equal(round(zero$ror, 6), 21.476190, "corrected ROR matches the hand value", tol = 1e-6)
expect_equal(round(zero$ror_low, 6), 1.080916, "corrected lower limit matches the hand value", tol = 1e-6)
expect_equal(round(zero$prr, 4), 14.4375, "corrected PRR", tol = 1e-6)
expect_true(isTRUE(zero$signal), "five observed cases and a lower limit above 1 is a signal")

# Two cases. The interval is above 1. The rule still says no.
small <- two_by_two(2, 10, 1, 400)
expect_equal(small$ror, 80, "ROR with two cases")
expect_true(small$ror_low > 1, "the interval can sit above 1 with two cases")
expect_true(!small$signal, "fewer than 3 cases is not a signal")

# Balanced table. ROR is 1 and the interval covers values below 1.
balanced <- two_by_two(10, 10, 10, 10)
expect_equal(balanced$ror, 1, "balanced ROR")
expect_equal(balanced$prr, 1, "balanced PRR")
expect_true(balanced$ror_low < 1 && balanced$ror_high > 1, "balanced interval covers 1")
expect_true(!balanced$signal, "an interval that covers 1 is not a signal")

# No reports on one side. The ratio is not estimated and it is not a signal.
empty <- two_by_two(0, 0, 5, 5)
expect_true(!empty$estimable, "a missing arm is not estimable")
expect_true(is.na(empty$ror), "a missing arm has no ROR")
expect_true(!empty$signal, "a missing arm is not a signal")

# Both outcome cells are zero. Correction runs. The observed case count is 0.
both_zero <- two_by_two(0, 10, 0, 10)
expect_true(both_zero$continuity_correction, "two zero cells are corrected")
expect_true(!both_zero$signal, "zero observed cases is not a signal")

expect_error(two_by_two(-1, 4, 1, 4), "a negative cell is rejected")

# Products of these counts exceed 2^31 - 1. They must stay finite.
big <- two_by_two(100000, 500000, 200000, 800000)
expect_true(is.finite(big$ror) && is.finite(big$ror_low) && is.finite(big$prr), "large counts do not overflow")
expect_equal(big$ror, (100000 * 800000) / (500000 * 200000), "large-count ROR")

# Intersection removed from both arms.
# Exposed 100 with 30 outcomes, comparator 200 with 40, both 10 with 5.
# Exclusive: a=25, b=65, c=35, d=155.
ex <- exclusive_cells(100, 30, 200, 40, 10, 5)
expect_equal(ex$a, 25, "exclusive exposed outcomes")
expect_equal(ex$b, 65, "exclusive exposed non-outcomes")
expect_equal(ex$c, 35, "exclusive comparator outcomes")
expect_equal(ex$d, 155, "exclusive comparator non-outcomes")
expect_equal(ex$n_exposed, 90, "exclusive exposed total")
expect_equal(ex$n_comparator, 190, "exclusive comparator total")
expect_equal(ex$ror, (25 * 155) / (65 * 35), "exclusive ROR uses the reduced cells")
expect_error(
  exclusive_cells(10, 4, 20, 5, 12, 1),
  "an intersection larger than a group is rejected"
)

finish_tests()
