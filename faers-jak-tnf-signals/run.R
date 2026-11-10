# One command, from the repository root:
#   Rscript faers-jak-tnf-signals/run.R
# Stages: test, fetch, estimate, all (default), check.
# API responses stay under data/ and are not committed.

args <- commandArgs(trailingOnly = TRUE)
stage <- if (length(args)) args[[1]] else "all"
if (!stage %in% c("test", "fetch", "estimate", "all", "check")) {
  stop("Stage must be test, fetch, estimate, all, or check.")
}

cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd[grep("^--file=", cmd)])
root <- normalizePath(dirname(file_arg))
Sys.setenv(FAERS_STUDY_ROOT = root)
extra_lib <- path.expand("~/R/library")
if (dir.exists(extra_lib)) .libPaths(unique(c(extra_lib, .libPaths())))
Sys.setlocale("LC_TIME", "C")

source(file.path(root, "R", "constants.R"))
source(file.path(root, "R", "measures.R"))
source(file.path(root, "R", "dedup.R"))
source(file.path(root, "R", "queries.R"))
source(file.path(root, "R", "fetch.R"))
source(file.path(root, "R", "analysis.R"))
source(file.path(root, "R", "figures.R"))
source(file.path(root, "R", "report.R"))

run_unit_tests <- function() {
  files <- c(
    "tests/test_measures.R",
    "tests/test_dedup.R",
    "tests/test_queries.R"
  )
  for (f in files) {
    message("Running ", f)
    status <- system2("Rscript", file.path(root, f))
    if (!identical(status, 0L)) stop("Test file failed: ", f)
  }
}

if (stage == "check") {
  check_case_study(root)
  quit(status = 0)
}
if (stage %in% c("test", "all")) {
  run_unit_tests()
}
if (stage == "test") quit(status = 0)

need <- "jsonlite"
if (stage %in% c("estimate", "all")) need <- c(need, "ggplot2")
missing_pkg <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkg)) {
  stop(
    "Missing R packages: ", paste(missing_pkg, collapse = ", "),
    ". Install them, or restore the lockfile with renv::restore() from the study directory."
  )
}

cache_dir <- file.path(root, "data", "cache")
out_dir <- file.path(root, "output")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
totals_path <- file.path(cache_dir, "query-totals.csv")
context_path <- file.path(cache_dir, "context.json")

if (stage %in% c("fetch", "all")) {
  started <- utc_now()
  message("Fetching openFDA counts.")
  plan <- planned_queries()
  totals <- fetch_queries(plan, cache_dir)
  stamp <- fetch_api_stamp()
  ascii <- list(
    head_ascii_zip("2026Q3"),
    head_ascii_zip("2026Q4")
  )
  context <- list(
    started_at = started,
    finished_at = utc_now(),
    stamp = stamp,
    ascii = ascii
  )
  utils::write.csv(totals, totals_path, row.names = FALSE)
  jsonlite::write_json(context, context_path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  message("Fetched ", nrow(totals), " queries.")
}
if (stage == "fetch") quit(status = 0)

if (!file.exists(totals_path) || !file.exists(context_path)) {
  stop("No cached counts. Run the fetch stage first.")
}
totals <- utils::read.csv(totals_path, stringsAsFactors = FALSE)
context <- jsonlite::read_json(context_path, simplifyVector = FALSE)
# read_json leaves the stamp as a list. The ascii statuses come back as a list of lists.
message("Building contrasts.")
contrasts <- build_contrasts(totals)
quarterly <- build_quarterly(totals)
flow <- cohort_flow(totals)
message("Drawing figures.")
write_figures(contrasts, quarterly, out_dir)

key_strings <- build_key_strings(contrasts, flow, context, totals)
readme_strings <- c(
  data_sentence(context, totals),
  headline_for(contrasts, "vte"),
  headline_for(contrasts, "infection")
)

ascii_rows <- lapply(context$ascii, function(x) {
  data.frame(
    quarter = x$quarter,
    url = x$url,
    http_status = x$http_status,
    stringsAsFactors = FALSE
  )
})

provenance <- list(
  study = "FAERS JAK inhibitors versus TNF inhibitors, VTE and serious infection",
  analyst = "Dr. Rishad Ahmed, PhD in Public Health, MBA",
  independent_sample_work = TRUE,
  employer_analysis = FALSE,
  endpoint = API_ENDPOINT,
  docs_url = OPENFDA_DOCS_URL,
  faers_landing_url = FAERS_LANDING_URL,
  retrieval_started_at = context$started_at,
  retrieval_finished_at = context$finished_at,
  openfda_last_updated = context$stamp$last_updated,
  openfda_index_total = context$stamp$index_total,
  openfda_terms = context$stamp$terms,
  openfda_license = context$stamp$license,
  receivedate_start = WINDOW_START,
  receivedate_end = WINDOW_END,
  quarters = QUARTERS$id,
  september_2026_reports = total_of(totals, "month__202609__all"),
  october_2026_reports = total_of(totals, "month__202610__all"),
  window_reports = total_of(totals, "window__all"),
  ascii_zip_head = do.call(rbind, ascii_rows),
  ascii_files_downloaded = FALSE,
  dedup_applied = FALSE,
  method = c(
    "Totals are openFDA drug/event counts from meta.results.total with limit=1. The quarterly ASCII zips were not downloaded.",
    "The receivedate window is 1 January 2019 through 30 September 2026, mapped to 2019 Q1 through 2026 Q3. October 2026 was queried and is recorded above.",
    "A drug is named when the ingredient token is on the generic name, the substance name, or the medicinal product, or when the originator brand token is on the medicinal product.",
    "drugcharacterization was not used as a primary-suspect filter. openFDA does not tie that field to the same drug object as the name.",
    "Reports that name both the JAK side of a contrast and a TNF inhibitor are removed from both arms.",
    "The latest-caseversion rule is implemented in R/dedup.R and covered by tests. It was not applied to these counts because the count endpoint does not return caseid or caseversion.",
    "Indications are phrase matches for rheumatoid arthritis, psoriatic arthropathy, psoriatic arthritis, ankylosing spondylitis, colitis ulcerative, psoriasis, and dermatitis atopic, plus the token crohn. FAERS stores that indication as Crohn^s disease, and a caret is not legal in an openFDA search.",
    "VTE terms are exact preferred terms: deep vein thrombosis, pulmonary embolism, venous thrombosis, thrombosis, and embolism venous. The term thrombosis is not specific to veins.",
    "Serious infection is serious:1 plus an exact match to the custom preferred-term list in R/constants.R. That list is not a MedDRA SMQ.",
    "The ROR interval is exp(log(ROR) plus or minus 1.96 times the usual log standard error). A zero cell adds 0.5 to every cell. The signal rule is a lower limit above 1 and at least 3 observed JAK reports with the outcome.",
    "The sensitivity split is reports received before 1 September 2021 versus reports received on or after that date."
  ),
  queries = totals,
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  packages = list(
    ggplot2 = pkg_version("ggplot2"),
    jsonlite = pkg_version("jsonlite")
  ),
  quarto_note = "Case study renders with Quarto from faers-jak-tnf-signals/docs."
)

write_outputs(out_dir, provenance, totals, contrasts, quarterly, flow, key_strings, readme_strings)
message("Estimation complete. Outputs are in ", out_dir)
for (line in key_strings) message(line)
