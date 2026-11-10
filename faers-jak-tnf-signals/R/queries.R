# openFDA search text. OR groups stay inside parentheses because AND
# binds more tightly than OR on this endpoint. Quotes are %22 so a
# space inside a preferred term cannot be read as AND.

quoted_field <- function(field, term) {
  escaped <- gsub(" ", "+", term, fixed = TRUE)
  paste0(field, ":%22", escaped, "%22")
}

or_group <- function(parts) {
  paste0("(", paste(parts, collapse = "+OR+"), ")")
}

drug_clause <- function(ingredient, brand) {
  c(
    paste0("patient.drug.openfda.generic_name:", ingredient),
    paste0("patient.drug.openfda.substance_name:", ingredient),
    paste0("patient.drug.medicinalproduct:", ingredient),
    paste0("patient.drug.medicinalproduct:", brand)
  )
}

exposure_parts <- function(ids) {
  unlist(lapply(ids, function(id) {
    drug <- DRUGS[[id]]
    drug_clause(drug$ingredient, drug$brand)
  }), use.names = FALSE)
}

exposure_clause <- function(exposure) {
  jak <- or_group(exposure_parts(JAK_IDS))
  tnf <- or_group(exposure_parts(TNF_IDS))
  if (identical(exposure, "jak")) return(jak)
  if (identical(exposure, "tnf")) return(tnf)
  if (identical(exposure, "jak_and_tnf")) return(paste0(jak, "+AND+", tnf))
  if (grepl("_and_tnf$", exposure)) {
    drug_id <- sub("_and_tnf$", "", exposure)
    if (!drug_id %in% JAK_IDS) stop("Unknown JAK intersection: ", exposure)
    return(paste0(or_group(exposure_parts(drug_id)), "+AND+", tnf))
  }
  if (exposure %in% names(DRUGS)) return(or_group(exposure_parts(exposure)))
  stop("Unknown exposure: ", exposure)
}

indication_clause <- function() {
  phrases <- vapply(INDICATION_PHRASES, function(term) {
    quoted_field("patient.drug.drugindication", term)
  }, character(1), USE.NAMES = FALSE)
  or_group(c(phrases, paste0("patient.drug.drugindication:", CROHN_INDICATION_TOKEN)))
}

vte_clause <- function() {
  parts <- vapply(VTE_PTS, function(term) {
    quoted_field("patient.reaction.reactionmeddrapt.exact", term)
  }, character(1), USE.NAMES = FALSE)
  or_group(parts)
}

infection_clause <- function() {
  parts <- vapply(INFECTION_PTS, function(term) {
    quoted_field("patient.reaction.reactionmeddrapt.exact", term)
  }, character(1), USE.NAMES = FALSE)
  paste0("serious:1+AND+", or_group(parts))
}

build_search <- function(start, end, exposure, outcome, indication = TRUE) {
  if (!grepl("^[0-9]{8}$", start) || !grepl("^[0-9]{8}$", end)) {
    stop("Dates must be YYYYMMDD.")
  }
  parts <- c(
    sprintf("receivedate:[%s+TO+%s]", start, end),
    exposure_clause(exposure)
  )
  if (isTRUE(indication)) parts <- c(parts, indication_clause())
  if (identical(outcome, "vte")) {
    parts <- c(parts, vte_clause())
  } else if (identical(outcome, "infection")) {
    parts <- c(parts, infection_clause())
  } else if (!identical(outcome, "cohort")) {
    stop("Unknown outcome: ", outcome)
  }
  paste(parts, collapse = "+AND+")
}

query_url <- function(search) {
  paste0(API_ENDPOINT, "?limit=1&search=", search)
}

query_id <- function(period, exposure, outcome, indication = TRUE) {
  paste(period, exposure, outcome, if (isTRUE(indication)) "ind" else "any", sep = "__")
}

planned_queries <- function() {
  rows <- list()
  add <- function(id, search) {
    rows[[length(rows) + 1L]] <<- data.frame(
      id = id,
      search = search,
      url = query_url(search),
      stringsAsFactors = FALSE
    )
  }
  exposures <- c(
    "jak", "tnf", "upa", "tofa", "bari",
    "jak_and_tnf", "upa_and_tnf", "tofa_and_tnf", "bari_and_tnf"
  )
  for (period in names(PERIODS)) {
    bounds <- PERIODS[[period]]
    for (exposure in exposures) {
      for (outcome in c("cohort", "vte", "infection")) {
        add(
          query_id(period, exposure, outcome, TRUE),
          build_search(bounds[[1]], bounds[[2]], exposure, outcome, TRUE)
        )
      }
    }
  }
  add(
    query_id("full", "jak", "cohort", FALSE),
    build_search(WINDOW_START, WINDOW_END, "jak", "cohort", FALSE)
  )
  add(
    query_id("full", "tnf", "cohort", FALSE),
    build_search(WINDOW_START, WINDOW_END, "tnf", "cohort", FALSE)
  )
  for (i in seq_len(nrow(QUARTERS))) {
    q <- QUARTERS[i, ]
    for (exposure in c("jak", "tnf", "jak_and_tnf")) {
      add(
        query_id(q$id, exposure, "vte", TRUE),
        build_search(q$start, q$end, exposure, "vte", TRUE)
      )
    }
  }
  add("month__202609__all", "receivedate:[20260901+TO+20260930]")
  add("month__202610__all", "receivedate:[20261001+TO+20261031]")
  add("window__all", sprintf("receivedate:[%s+TO+%s]", WINDOW_START, WINDOW_END))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if (anyDuplicated(out$id)) stop("Query ids are not unique.")
  out
}
