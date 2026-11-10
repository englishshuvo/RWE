# The search text is part of the method. These checks do not call the API.
args_root <- Sys.getenv("FAERS_STUDY_ROOT", unset = "")
if (!nzchar(args_root)) {
  cand <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
  if (file.exists(file.path(cand, "R", "queries.R"))) {
    Sys.setenv(FAERS_STUDY_ROOT = cand)
  } else {
    Sys.setenv(FAERS_STUDY_ROOT = normalizePath(getwd()))
  }
}
root <- Sys.getenv("FAERS_STUDY_ROOT")
source(file.path(root, "R", "constants.R"))
source(file.path(root, "R", "queries.R"))
source(file.path(root, "tests", "helper.R"))

message("Query-text tests. Not live counts.")

plan <- planned_queries()
expect_equal(nrow(plan), 179, "179 planned queries")
expect_equal(nrow(QUARTERS), 31, "31 quarters, 2019 Q1 through 2026 Q3")
expect_equal(QUARTERS$id[[1]], "2019Q1", "first quarter")
expect_equal(QUARTERS$id[[nrow(QUARTERS)]], "2026Q3", "last quarter")
expect_true(!any(grepl("^", plan$search, fixed = TRUE)), "no caret in any search")
expect_true(all(grepl("limit=1", plan$url, fixed = TRUE)), "every URL asks for a count, not a report")
expect_true(!anyDuplicated(plan$id), "query ids are unique")

vte <- build_search("20190101", "20260930", "jak", "vte", TRUE)
expect_true(grepl("receivedate:[20190101+TO+20260930]", vte, fixed = TRUE), "full window dates")
expect_true(grepl("+AND+(", vte, fixed = TRUE), "OR groups are parenthesized")
for (term in VTE_PTS) {
  piece <- quoted_field("patient.reaction.reactionmeddrapt.exact", term)
  expect_true(grepl(piece, vte, fixed = TRUE), paste("VTE term locked:", term))
}
expect_true(grepl("medicinalproduct:xeljanz", vte, fixed = TRUE), "JAK search includes Xeljanz")
expect_true(grepl("medicinalproduct:rinvoq", vte, fixed = TRUE), "JAK search includes Rinvoq")
expect_true(grepl("medicinalproduct:olumiant", vte, fixed = TRUE), "JAK search includes Olumiant")
expect_true(!grepl("humira", vte), "JAK search does not include Humira")
expect_true(grepl("drugindication:crohn", vte, fixed = TRUE), "Crohn indication uses the token")
expect_true(grepl("%22rheumatoid+arthritis%22", vte, fixed = TRUE), "rheumatoid arthritis is a quoted phrase")

cohort <- build_search("20190101", "20260930", "tnf", "cohort", TRUE)
expect_true(!grepl("reactionmeddrapt", cohort), "a cohort query has no reaction term")
expect_true(grepl("medicinalproduct:humira", cohort, fixed = TRUE), "TNF search includes Humira")
expect_true(grepl("medicinalproduct:remicade", cohort, fixed = TRUE), "TNF search includes Remicade")
expect_true(grepl("medicinalproduct:enbrel", cohort, fixed = TRUE), "TNF search includes Enbrel")
expect_true(grepl("medicinalproduct:cimzia", cohort, fixed = TRUE), "TNF search includes Cimzia")
expect_true(grepl("medicinalproduct:simponi", cohort, fixed = TRUE), "TNF search includes Simponi")
expect_true(!grepl("xeljanz", cohort), "TNF search does not include Xeljanz")

upa <- build_search("20190101", "20210831", "upa", "infection", TRUE)
expect_true(grepl("receivedate:[20190101+TO+20210831]", upa, fixed = TRUE), "pre-warning dates")
expect_true(grepl("serious:1", upa, fixed = TRUE), "serious infection requires the serious flag")
expect_true(grepl("%22covid-19%22", upa, fixed = TRUE), "COVID-19 is an exact term")
expect_true(grepl("medicinalproduct:rinvoq", upa, fixed = TRUE), "upadacitinib includes Rinvoq")
expect_true(!grepl("xeljanz", upa), "upadacitinib does not include Xeljanz")
expect_true(!grepl("olumiant", upa), "upadacitinib does not include Olumiant")

plain <- build_search("20190101", "20260930", "jak", "cohort", FALSE)
expect_true(!grepl("drugindication", plain), "the indication filter can be turned off")

both <- build_search("20190101", "20260930", "jak_and_tnf", "vte", TRUE)
expect_true(grepl("xeljanz", both) && grepl("humira", both), "the intersection names both classes")

expect_equal(length(INFECTION_PTS), 47, "infection list length is locked")
expect_equal(length(VTE_PTS), 5, "VTE list length is locked")
expect_true(!any(duplicated(INFECTION_PTS)), "infection terms are unique")

finish_tests()
