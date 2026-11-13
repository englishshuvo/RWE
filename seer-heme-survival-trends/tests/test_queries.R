# Query plan and the FDA date table. None of these numbers are SEER rates.
args_root <- Sys.getenv("SEER_STUDY_ROOT", unset = "")
if (!nzchar(args_root)) {
  cand <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
  if (file.exists(file.path(cand, "R", "constants.R"))) {
    Sys.setenv(SEER_STUDY_ROOT = cand)
  } else {
    Sys.setenv(SEER_STUDY_ROOT = normalizePath(getwd()))
  }
}
root <- Sys.getenv("SEER_STUDY_ROOT")
source(file.path(root, "R", "constants.R"))
source(file.path(root, "R", "queries.R"))
source(file.path(root, "tests", "helper.R"))

message("Query and approval tests. Not SEER estimates.")

plan <- planned_queries()
ids <- vapply(plan, function(q) q$id, character(1))
expect_equal(length(plan), 21L, "three cancers, seven requests each")
expect_true(length(unique(ids)) == length(ids), "query ids are unique")

sites <- vapply(plan, function(q) as.integer(q$params$site), integer(1))
expect_true(all(sites %in% c(89L, 93L, 86L)), "sites are myeloma, CLL, and non-Hodgkin lymphoma")
expect_true(!any(sites == 68L), "the expanded-site code for large B-cell lymphoma is not requested")

for (q in plan) {
  expect_true(startsWith(q$url, REGION5_URL), paste("URL stays on the explorer endpoint:", q$id))
  expect_true(grepl("stage=101", q$url, fixed = TRUE), paste("all stages:", q$id))
  expect_true(!grepl("seerstat", q$url, ignore.case = TRUE), paste("not a SEER*Stat URL:", q$id))
  if (q$measure == "incidence") {
    expect_true(grepl("data_type=1", q$url, fixed = TRUE), paste("incidence data type:", q$id))
    expect_true(grepl("rate_type=", q$url, fixed = TRUE), paste("incidence names a rate type:", q$id))
  }
  if (q$measure == "survival") {
    expect_true(grepl("data_type=4", q$url, fixed = TRUE), paste("survival data type:", q$id))
    expect_true(grepl("relative_survival_interval=5", q$url, fixed = TRUE), paste("5-year interval:", q$id))
    expect_true(!grepl("rate_type=", q$url, fixed = TRUE), paste("survival request has no rate type:", q$id))
  }
}

delay <- plan[vapply(plan, function(q) q$slice == "delay", logical(1))]
expect_true(all(vapply(delay, function(q) q$params$rate_type == "2", logical(1))), "delay requests use rate type 2")

on <- APPROVALS[APPROVALS$on_figure, , drop = FALSE]
expect_true(all(on$date >= as.Date("2000-01-01") & on$date <= as.Date("2023-12-31")), "drawn dates fall inside 2000-2023")
expect_true(all(startsWith(APPROVALS$source_url, "https://www.accessdata.fda.gov/") | startsWith(APPROVALS$source_url, "https://www.fda.gov/")), "every approval cites an FDA host")
expect_true(!any(grepl("\u2014", APPROVALS$indication)), "approval text has no em dash")

ritux <- APPROVALS[APPROVALS$drug == "Rituximab", , drop = FALSE]
expect_equal(nrow(ritux), 1L, "one rituximab row")
expect_equal(ritux$date, as.Date("1997-11-26"), "rituximab date is 26 November 1997")
expect_true(!ritux$on_figure, "rituximab is not drawn")

pola <- APPROVALS[APPROVALS$drug == "Polatuzumab vedotin" & APPROVALS$on_figure, , drop = FALSE]
expect_equal(pola$date, as.Date("2019-06-10"), "drawn polatuzumab date is 10 June 2019")
later <- APPROVALS[APPROVALS$drug == "Polatuzumab vedotin" & !APPROVALS$on_figure, , drop = FALSE]
expect_equal(later$date, as.Date("2023-04-19"), "later polatuzumab date is recorded and not drawn")

expect_equal(MIN_COUNT, 16L, "suppression cutoff is 16")
expect_equal(SITES$nhl$id, 86L, "lymphoma series is non-Hodgkin lymphoma")

finish_tests()
