# Download the public SEER*Explorer JSON. Cached bodies stay in data/.

utc_now <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
}

sha256_file <- function(path) {
  out <- system2("sha256sum", path, stdout = TRUE)
  if (!is.null(attr(out, "status"))) stop("sha256sum failed for ", path)
  strsplit(out, " ", fixed = TRUE)[[1]][[1]]
}

curl_config <- function(lines) {
  cfg <- tempfile(fileext = ".curl")
  writeLines(lines, cfg)
  cfg
}

download_url <- function(url, dest) {
  delay <- c(2, 4, 8, 16)
  last_note <- "no response"
  for (attempt in seq_along(delay)) {
    status <- system2(
      "curl",
      c("-K", curl_config(c(
        sprintf("url = \"%s\"", url),
        sprintf("output = \"%s\"", dest),
        "silent",
        "show-error",
        "location",
        "max-time = 180",
        "user-agent = \"rwe-seer-heme/1.0\"",
        "write-out = \"%{http_code}\""
      ))),
      stdout = TRUE
    )
    curl_status <- attr(status, "status")
    http <- suppressWarnings(as.integer(if (length(status)) status[[1]] else NA))
    if (!is.null(curl_status) || length(http) != 1L || is.na(http) || http == 0L) {
      last_note <- if (length(status)) paste(status, collapse = " ") else "curl failed"
      message("  retry ", attempt, ": ", last_note)
      Sys.sleep(delay[[attempt]])
      next
    }
    if (identical(http, 200L) && file.exists(dest) && file.info(dest)$size > 0) {
      return(list(http_status = http, sha256 = sha256_file(dest), retrieved_at = utc_now()))
    }
    last_note <- paste("HTTP", http)
    message("  retry ", attempt, ": ", last_note)
    Sys.sleep(delay[[attempt]])
  }
  stop("Download failed for ", url, ": ", last_note)
}

fetch_queries <- function(plan, cache_dir) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- list()
  for (query in plan) {
    dest <- file.path(cache_dir, paste0(query$id, ".json"))
    message("Fetching ", query$id)
    got <- download_url(query$url, dest)
    rows[[query$id]] <- data.frame(
      id = query$id,
      cancer = query$cancer,
      measure = query$measure,
      slice = query$slice,
      url = query$url,
      http_status = got$http_status,
      sha256 = got$sha256,
      bytes = file.info(dest)$size,
      retrieved_at = got$retrieved_at,
      stringsAsFactors = FALSE
    )
    Sys.sleep(0.2)
  }
  do.call(rbind, rows)
}

fetch_support <- function(cache_dir) {
  files <- c(
    codebook = CODEBOOK_URL,
    footnotes = FOOTNOTE_URL,
    notes = NOTES_URL
  )
  out <- list()
  for (nm in names(files)) {
    dest <- file.path(cache_dir, paste0(nm, ".json"))
    message("Fetching ", nm)
    got <- download_url(files[[nm]], dest)
    out[[nm]] <- c(got, list(url = files[[nm]], path = dest, bytes = file.info(dest)$size))
  }
  out
}

read_cached_queries <- function(plan, cache_dir) {
  rates <- list()
  trends <- list()
  for (query in plan) {
    path <- file.path(cache_dir, paste0(query$id, ".json"))
    if (!file.exists(path)) stop("Missing cache file for ", query$id)
    parsed <- parse_query_text(paste(readLines(path, warn = FALSE), collapse = "\n"), query)
    rates[[query$id]] <- parsed$rates
    trends[[query$id]] <- parsed$trends
  }
  list(
    rates = do.call(rbind, rates),
    trends = do.call(rbind, trends)
  )
}
