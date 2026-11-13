# Turn a SEER*Explorer region-5 body into rate rows and trend rows.
# The body is a JSON string whose value is itself JSON.

as_num <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  suppressWarnings(as.numeric(x[[1]]))
}

as_logi <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA)
  if (is.logical(x)) return(isTRUE(x))
  tolower(as.character(x[[1]])) %in% c("true", "1")
}

parse_explorer_text <- function(text) {
  outer <- jsonlite::fromJSON(text, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  if (is.character(outer) && length(outer) == 1L) {
    inner <- jsonlite::fromJSON(outer, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  } else if (is.list(outer)) {
    inner <- outer
  } else {
    stop("SEER*Explorer body was neither a JSON string nor a JSON object.")
  }
  if (is.null(inner$info) || is.null(inner$data)) {
    stop("SEER*Explorer body had no info or data object.")
  }
  inner
}

point_to_row <- function(point, fields) {
  if (length(point) < length(fields)) {
    stop("A data point had ", length(point), " values and the field list has ", length(fields), ".")
  }
  vals <- stats::setNames(lapply(seq_along(fields), function(i) point[[i]]), fields)
  data.frame(
    year = as.integer(as_num(vals$year)),
    estimate = as_num(if (!is.null(vals$rate)) vals$rate else vals$rel_rate),
    std_error = as_num(vals$rel_rate_se),
    ci_low = as_num(if (!is.null(vals$rate_lower_ci)) vals$rate_lower_ci else vals$rel_rate_lower_ci),
    ci_high = as_num(if (!is.null(vals$rate_upper_ci)) vals$rate_upper_ci else vals$rel_rate_upper_ci),
    modeled = as_num(if (!is.null(vals$modeled_rate)) vals$modeled_rate else vals$modeled_rel_rate),
    count = as_num(vals$count),
    stringsAsFactors = FALSE
  )
}

trend_to_row <- function(point, fields) {
  if (length(point) < length(fields)) {
    stop("A trend row had ", length(point), " values and the field list has ", length(fields), ".")
  }
  vals <- stats::setNames(lapply(seq_along(fields), function(i) point[[i]]), fields)
  data.frame(
    year_start = as.integer(as_num(vals$year_start)),
    year_end = as.integer(as_num(vals$year_end)),
    trend_type = as.integer(as_num(vals$trend_type)),
    seg_number = as.integer(as_num(vals$seg_number)),
    estimate = as_num(vals$apc),
    ci_low = as_num(vals$apc_lower_ci),
    ci_high = as_num(vals$apc_upper_ci),
    significant = as_logi(vals$apc_significance),
    p_value = as_num(vals$apc_pvalue),
    stringsAsFactors = FALSE
  )
}

key_values <- function(key, key_order) {
  parts <- strsplit(key, "_", fixed = TRUE)[[1]]
  if (length(parts) != length(key_order)) {
    stop("Key ", key, " has ", length(parts), " parts and the key order has ", length(key_order), ".")
  }
  stats::setNames(parts, key_order)
}

empty_rates <- function() {
  data.frame(
    cancer = character(), measure = character(), slice = character(),
    rate_type = integer(), sex = integer(), race = integer(),
    age_range = integer(), stage = integer(), site = integer(),
    year = integer(), estimate = numeric(), std_error = numeric(),
    ci_low = numeric(), ci_high = numeric(), modeled = numeric(),
    count = numeric(), stringsAsFactors = FALSE
  )
}

empty_trends <- function() {
  data.frame(
    cancer = character(), measure = character(), slice = character(),
    rate_type = integer(), sex = integer(), race = integer(),
    age_range = integer(), stage = integer(), site = integer(),
    year_start = integer(), year_end = integer(), trend_type = integer(),
    seg_number = integer(), estimate = numeric(), ci_low = numeric(),
    ci_high = numeric(), significant = logical(), p_value = numeric(),
    stringsAsFactors = FALSE
  )
}

code_of <- function(vals, name) {
  if (!name %in% names(vals)) return(NA_integer_)
  as.integer(vals[[name]])
}

parse_series <- function(key, series, info, query) {
  key_order <- unlist(info[["key-order"]], use.names = FALSE)
  data_fields <- unlist(info[["data-fields"]], use.names = FALSE)
  trend_fields <- unlist(info[["trend-fields"]], use.names = FALSE)
  vals <- key_values(key, key_order)
  rates <- do.call(rbind, lapply(series$data_series, point_to_row, fields = data_fields))
  trends <- do.call(rbind, lapply(series$trend_data, trend_to_row, fields = trend_fields))
  stamp <- data.frame(
    cancer = query$cancer,
    measure = query$measure,
    slice = query$slice,
    rate_type = code_of(vals, "rate_type"),
    sex = code_of(vals, "sex"),
    race = code_of(vals, "race"),
    age_range = code_of(vals, "age_range"),
    stage = code_of(vals, "stage"),
    site = code_of(vals, "site"),
    stringsAsFactors = FALSE
  )
  list(
    rates = if (is.null(rates) || !nrow(rates)) empty_rates() else cbind(stamp, rates, row.names = NULL),
    trends = if (is.null(trends) || !nrow(trends)) empty_trends() else cbind(stamp, trends, row.names = NULL)
  )
}

parse_query_text <- function(text, query) {
  inner <- parse_explorer_text(text)
  keys <- names(inner$data)
  if (!length(keys)) stop("Query ", query$id, " returned no series.")
  parsed <- lapply(keys, function(key) parse_series(key, inner$data[[key]], inner$info, query))
  list(
    rates = do.call(rbind, lapply(parsed, `[[`, "rates")),
    trends = do.call(rbind, lapply(parsed, `[[`, "trends"))
  )
}
