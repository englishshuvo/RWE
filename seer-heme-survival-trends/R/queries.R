# The SEER*Explorer requests this study makes. One row is one download.

query_url <- function(params) {
  parts <- vapply(names(params), function(nm) {
    paste0(utils::URLencode(nm, reserved = TRUE), "=", utils::URLencode(as.character(params[[nm]]), reserved = TRUE))
  }, character(1))
  paste0(REGION5_URL, "?", paste(parts, collapse = "&"))
}

one_query <- function(cancer, measure, slice, params) {
  params <- lapply(params, as.character)
  list(
    id = paste(cancer, measure, slice, sep = "__"),
    cancer = cancer,
    measure = measure,
    slice = slice,
    params = params,
    url = query_url(params)
  )
}

site_queries <- function(cancer) {
  site <- SITES[[cancer]]$id
  common_inc <- list(site = site, data_type = 1, graph_type = 2, stage = 101)
  common_surv <- list(
    site = site, data_type = 4, graph_type = 2, stage = 101,
    relative_survival_interval = 5
  )
  list(
    one_query(cancer, "incidence", "sex", c(
      common_inc, list(compareBy = "sex", race = 1, age_range = 1, rate_type = 1)
    )),
    one_query(cancer, "incidence", "race", c(
      common_inc, list(compareBy = "race", sex = 1, age_range = 1, rate_type = 1)
    )),
    one_query(cancer, "incidence", "age", c(
      common_inc, list(compareBy = "age_range", sex = 1, race = 1, rate_type = 1)
    )),
    one_query(cancer, "incidence", "delay", c(
      common_inc, list(compareBy = "sex", race = 1, age_range = 1, rate_type = 2)
    )),
    one_query(cancer, "survival", "sex", c(
      common_surv, list(compareBy = "sex", race = 1, age_range = 1)
    )),
    one_query(cancer, "survival", "race", c(
      common_surv, list(compareBy = "race", sex = 1, age_range = 1)
    )),
    one_query(cancer, "survival", "age", c(
      common_surv, list(compareBy = "age_range", sex = 1, race = 1)
    ))
  )
}

planned_queries <- function() {
  out <- list()
  for (cancer in SITE_ORDER) out <- c(out, site_queries(cancer))
  out
}
