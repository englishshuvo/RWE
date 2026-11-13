# One command, from the repository root:
#   Rscript seer-heme-survival-trends/run.R
# Stages: test, fetch, estimate, all (default), check.
# Downloaded JSON stays under data/ and is not committed.

args <- commandArgs(trailingOnly = TRUE)
stage <- if (length(args)) args[[1]] else "all"
if (!stage %in% c("test", "fetch", "estimate", "all", "check")) {
  stop("Stage must be test, fetch, estimate, all, or check.")
}

cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd[grep("^--file=", cmd)])
root <- normalizePath(dirname(file_arg))
Sys.setenv(SEER_STUDY_ROOT = root)
extra_lib <- path.expand("~/R/library")
renv_libs <- Sys.glob(file.path(root, "renv", "library", "*", "*"))
.libPaths(unique(c(renv_libs, extra_lib, .libPaths())))
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
Sys.setlocale("LC_TIME", "C")

source(file.path(root, "R", "constants.R"))
source(file.path(root, "R", "queries.R"))
source(file.path(root, "R", "parse.R"))
source(file.path(root, "R", "measures.R"))
source(file.path(root, "R", "fetch.R"))
source(file.path(root, "R", "figures.R"))
source(file.path(root, "R", "report.R"))

run_unit_tests <- function() {
  files <- c(
    "tests/test_queries.R",
    "tests/test_parse.R",
    "tests/test_measures.R"
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
if (stage %in% c("test", "all")) run_unit_tests()
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
log_path <- file.path(cache_dir, "query-log.csv")
support_path <- file.path(cache_dir, "support.json")

if (stage %in% c("fetch", "all")) {
  started <- utc_now()
  plan <- planned_queries()
  log_df <- fetch_queries(plan, cache_dir)
  support <- fetch_support(cache_dir)
  utils::write.csv(log_df, log_path, row.names = FALSE)
  jsonlite::write_json(
    list(started_at = started, finished_at = utc_now(), support = support),
    support_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  message("Fetched ", nrow(log_df), " queries.")
}
if (stage == "fetch") quit(status = 0)

if (!file.exists(log_path) || !file.exists(support_path)) {
  stop("No cached download. Run the fetch stage first.")
}

message("Checking the codebook and the footnotes.")
release <- check_release(cache_dir)
log_df <- utils::read.csv(log_path, stringsAsFactors = FALSE)
support <- jsonlite::read_json(support_path, simplifyVector = FALSE)
plan <- planned_queries()
message("Parsing cached responses.")
parsed <- read_cached_queries(plan, cache_dir)
prepared <- prepare_rates(parsed$rates)
assert_required(prepared$incidence, prepared$survival)
trends <- prepare_trends(parsed$trends)
gap <- survival_gap(prepared$survival)
message("Drawing figures.")
write_figures(prepared$incidence, prepared$survival, gap, out_dir)

retrieved_on <- as.Date(substr(support$finished_at, 1, 10))
key_strings <- build_key_strings(prepared$incidence, prepared$survival, gap, trends, retrieved_on)
readme_strings <- build_readme_strings(key_strings)

dropped_note <- sprintf(
  "Parsed %s rate rows from the API. The committed incidence file has %s rows and the survival file has %s rows. A row was dropped when the estimate was missing or the case count was under %d. Modeled survival for years without an observed 5-year rate was not copied into the survival file.",
  fmt_int(nrow(parsed$rates)),
  fmt_int(nrow(prepared$incidence)),
  fmt_int(nrow(prepared$survival)),
  MIN_COUNT
)

provenance <- list(
  study = "SEER blood cancer incidence and 5-year relative survival",
  analyst = "Dr. Rishad Ahmed, PhD in Public Health, MBA",
  independent_sample_work = TRUE,
  employer_analysis = FALSE,
  speaks_for_company = FALSE,
  explorer_page = EXPLORER_PAGE,
  archive_page = ARCHIVE_PAGE,
  site_definitions = SITE_DEFS_URL,
  region5_url = REGION5_URL,
  codebook_url = CODEBOOK_URL,
  footnote_url = FOOTNOTE_URL,
  notes_url = NOTES_URL,
  retrieval_started_at = support$started_at,
  retrieval_finished_at = support$finished_at,
  seer_submission = "November 2025 submission (1975-2023), released in SEER*Explorer on 22 April 2026 and updated 22 June 2026",
  incidence_registry = "SEER 21 registries",
  survival_registry = "SEER 21 registries excluding Illinois",
  standard_population = "2000 US standard population (20 age groups, Census P25-1130)",
  dlbcl_in_source = FALSE,
  lymphoma_series = "Non-Hodgkin lymphoma, SEER*Explorer site 86",
  small_cell_rule = "Rates with a case count under 16 are not written. This is the SEER suppression threshold.",
  covid_note = release$covid,
  approval_lines_are_effects = FALSE,
  queries = log_df,
  support_files = support$support,
  approvals = APPROVALS,
  dropped_rows_note = dropped_note,
  method = c(
    "Each series is a SEER*Explorer render_region_5.php response. The body is a JSON string that itself contains JSON.",
    "Incidence recent trends use data_type 1 and graph_type 2. Survival recent trends use data_type 4 and graph_type 2, with relative_survival_interval 5.",
    "The comparison dimension is sex, race, or age. The other dimensions are fixed at all races, all ages, both sexes, and all stages, except where that dimension is the comparison.",
    "Observed incidence is rate_type 1. Delay-adjusted incidence is rate_type 2, kept for both sexes, all races, and all ages.",
    "Age bands in the committed files are all ages, under 50, 50 to 64, and 65 and older. The incidence control offers other bands. Those bands were not retained.",
    "Survival race groups in this endpoint are all races, White (includes Hispanic), and Black (includes Hispanic). Hispanic ethnicity and non-Hispanic race are not on the survival trend.",
    "A 5-year relative survival value is plotted and tabulated only when the observed rate and a case count of at least 16 are both present. The modeled rate is not used as a substitute.",
    "The gap is White (includes Hispanic) minus Black (includes Hispanic), in percentage points, for both sexes and all ages.",
    "Joinpoint segments are the ones SEER returned. This study does not refit them. Incidence trend_type 2 is the annual percent change. Average annual percent change rows were not kept.",
    "For survival, the JSON field named apc is the average absolute change in survival, in percentage points per year, as the SEER footnote defines it.",
    "FDA dates are context markers. Rituximab (26 November 1997) is before the axis. Polatuzumab vedotin's 19 April 2023 untreated-DLBCL approval is after the last observed 5-year survival year. Neither is drawn."
  ),
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  packages = list(
    ggplot2 = pkg_version("ggplot2"),
    jsonlite = pkg_version("jsonlite")
  ),
  quarto_note = "Case study renders with Quarto from seer-heme-survival-trends/docs."
)

write_outputs(out_dir, prepared$incidence, prepared$survival, gap, trends, key_strings, readme_strings, provenance)
message("Estimation complete. Outputs are in ", out_dir)
for (line in key_strings) message(line)
