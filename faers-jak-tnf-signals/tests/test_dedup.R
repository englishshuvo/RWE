# Latest case version per case id, on a fabricated table.
args_root <- Sys.getenv("FAERS_STUDY_ROOT", unset = "")
if (!nzchar(args_root)) {
  cand <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
  if (file.exists(file.path(cand, "R", "dedup.R"))) {
    Sys.setenv(FAERS_STUDY_ROOT = cand)
  } else {
    Sys.setenv(FAERS_STUDY_ROOT = normalizePath(getwd()))
  }
}
root <- Sys.getenv("FAERS_STUDY_ROOT")
source(file.path(root, "R", "dedup.R"))
source(file.path(root, "tests", "helper.R"))

message("Dedup tests on a fabricated case table. Not FAERS reports.")

# A has versions 1, 3, 2. Keep version 3.
# B has one row. C has two rows at version 4. Keep the later input row.
df <- data.frame(
  caseid = c("A", "A", "A", "B", "C", "C"),
  caseversion = c(1, 3, 2, 1, 4, 4),
  drug = c("d1", "d3", "d2", "b", "c_first", "c_second"),
  stringsAsFactors = FALSE
)
out <- dedup_latest_caseversion(df)
expect_equal(nrow(out), 3, "one row per case id")
expect_equal(length(unique(out$caseid)), 3, "case ids are unique after dedup")
expect_equal(out$drug[out$caseid == "A"], "d3", "the highest version is kept")
expect_equal(out$caseversion[out$caseid == "A"], 3, "kept version is 3")
expect_equal(out$drug[out$caseid == "B"], "b", "a single version is kept")
expect_equal(out$drug[out$caseid == "C"], "c_second", "a tied version keeps the later input row")

# A later row with an older version must not win.
rev_df <- data.frame(
  caseid = c("Z", "Z"),
  caseversion = c(2, 9),
  drug = c("old", "new"),
  stringsAsFactors = FALSE
)
rev_out <- dedup_latest_caseversion(rev_df[c(2, 1), ])
expect_equal(rev_out$drug, "new", "version outranks input order")

empty <- df[0, , drop = FALSE]
empty_out <- dedup_latest_caseversion(empty)
expect_equal(nrow(empty_out), 0, "an empty table stays empty")
expect_equal(names(empty_out), names(df), "columns are preserved on an empty table")

bad_version <- df
bad_version$caseversion[2] <- NA
expect_error(
  dedup_latest_caseversion(bad_version),
  "a missing case version is rejected"
)

bad_id <- df
bad_id$caseid[4] <- NA
expect_error(
  dedup_latest_caseversion(bad_id),
  "a missing case id is rejected"
)

finish_tests()
