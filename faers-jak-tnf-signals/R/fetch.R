# Pull openFDA count totals. Each response is cached under data/cache
# so a second run does not repeat a query whose URL has not changed.
# Nothing in that cache is committed.

utc_now <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
}

sha256_file <- function(path) {
  out <- system2("sha256sum", path, stdout = TRUE)
  if (!is.null(attr(out, "status"))) stop("sha256sum failed for ", path)
  strsplit(out, " ", fixed = TRUE)[[1]][[1]]
}

parse_total <- function(path, http_status) {
  body <- paste(readLines(path, warn = FALSE), collapse = "\n")
  parsed <- tryCatch(jsonlite::fromJSON(body), error = function(e) NULL)
  if (is.null(parsed)) stop("openFDA returned a body that is not JSON.")
  if (identical(http_status, 200L)) {
    total <- parsed$meta$results$total
    if (length(total) != 1L || !is.finite(total)) stop("openFDA 200 response had no total.")
    return(as.numeric(total))
  }
  if (identical(http_status, 404L)) {
    code <- parsed$error$code
    if (!identical(code, "NOT_FOUND")) stop("openFDA 404 was not an empty search: ", code)
    return(0)
  }
  message <- if (!is.null(parsed$error$message)) parsed$error$message else body
  stop("openFDA HTTP ", http_status, ": ", message)
}

# system2 routes a stderr file through a shell, and the search text
# contains parentheses. A curl config file keeps those characters out of the shell.
curl_config <- function(lines) {
  cfg <- tempfile(fileext = ".curl")
  writeLines(lines, cfg)
  cfg
}

run_curl <- function(lines) {
  cfg <- curl_config(lines)
  on.exit(unlink(cfg), add = TRUE)
  system2("curl", c("-K", cfg), stdout = TRUE)
}

fetch_search <- function(search) {
  url <- if (grepl("^https://", search)) search else query_url(search)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  delay <- c(2, 4, 8, 16, 32)
  last_note <- "no response"
  for (attempt in seq_along(delay)) {
    status <- run_curl(c(
      sprintf("url = \"%s\"", url),
      sprintf("output = \"%s\"", tmp),
      "silent",
      "show-error",
      "globoff",
      "max-time = 120",
      "user-agent = \"rwe-faers-jak-tnf/1.0\"",
      "write-out = \"%{http_code}\""
    ))
    curl_status <- attr(status, "status")
    http_txt <- if (length(status)) status[[1]] else ""
    http <- suppressWarnings(as.integer(http_txt))
    if (!is.null(curl_status) || !length(http) || is.na(http) || http == 0L) {
      last_note <- if (length(status)) paste(status, collapse = " ") else "curl failed"
      message("  curl retry ", attempt, ": ", last_note)
      Sys.sleep(delay[[attempt]])
      next
    }
    if (http %in% c(200L, 404L)) {
      return(list(
        url = url,
        total = parse_total(tmp, http),
        http_status = http,
        sha256 = sha256_file(tmp),
        retrieved_at = utc_now()
      ))
    }
    body_note <- if (file.exists(tmp)) paste(readLines(tmp, warn = FALSE), collapse = " ") else ""
    last_note <- paste(http, body_note)
    message("  HTTP ", http, " retry ", attempt)
    Sys.sleep(delay[[attempt]])
  }
  stop("Gave up on ", url, " (", last_note, ")")
}

fetch_queries <- function(plan, cache_dir) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- vector("list", nrow(plan))
  for (i in seq_len(nrow(plan))) {
    id <- plan$id[[i]]
    cache_path <- file.path(cache_dir, paste0(id, ".json"))
    used_cache <- FALSE
    if (file.exists(cache_path)) {
      cached <- jsonlite::read_json(cache_path, simplifyVector = TRUE)
      if (identical(cached$url, plan$url[[i]]) && length(cached$total) == 1L) {
        hit <- cached
        used_cache <- TRUE
      }
    }
    if (!used_cache) {
      message(sprintf("[%d/%d] %s", i, nrow(plan), id))
      hit <- fetch_search(plan$url[[i]])
      jsonlite::write_json(hit, cache_path, auto_unbox = TRUE, pretty = TRUE)
      Sys.sleep(0.15)
    }
    rows[[i]] <- data.frame(
      id = id,
      url = hit$url,
      total = hit$total,
      http_status = hit$http_status,
      sha256 = hit$sha256,
      retrieved_at = hit$retrieved_at,
      from_cache = used_cache,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

fetch_api_stamp <- function() {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  status <- run_curl(c(
    sprintf("url = \"%s\"", API_ENDPOINT),
    sprintf("output = \"%s\"", tmp),
    "silent",
    "show-error",
    "max-time = 60",
    "user-agent = \"rwe-faers-jak-tnf/1.0\"",
    "write-out = \"%{http_code}\""
  ))
  if (!is.null(attr(status, "status"))) stop("Could not read the openFDA stamp.")
  http <- suppressWarnings(as.integer(if (length(status)) status[[1]] else NA))
  if (!identical(http, 200L)) stop("openFDA stamp HTTP ", http)
  parsed <- jsonlite::fromJSON(tmp)
  list(
    last_updated = parsed$meta$last_updated,
    index_total = parsed$meta$results$total,
    terms = parsed$meta$terms,
    license = parsed$meta$license
  )
}

head_ascii_zip <- function(quarter) {
  url <- sprintf(ASCII_ZIP_URL, quarter)
  status <- run_curl(c(
    sprintf("url = \"%s\"", url),
    "output = \"/dev/null\"",
    "head",
    "location",
    "silent",
    "show-error",
    "max-time = 40",
    "user-agent = \"rwe-faers-jak-tnf/1.0\"",
    "write-out = \"%{http_code}\""
  ))
  code <- suppressWarnings(as.integer(if (length(status)) status[[1]] else NA))
  list(quarter = quarter, url = url, http_status = code)
}
