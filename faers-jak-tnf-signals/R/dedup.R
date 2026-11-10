# Latest case version per case id.
# This is the rule for an ASCII DEMO-style table. The openFDA count
# endpoint does not return caseid or caseversion, so the live counts
# do not pass through this function. The tests still lock the rule.

dedup_latest_caseversion <- function(df, caseid_col = "caseid", caseversion_col = "caseversion") {
  if (!is.data.frame(df)) stop("dedup expects a data frame.")
  if (!caseid_col %in% names(df) || !caseversion_col %in% names(df)) {
    stop("The case id and case version columns are required.")
  }
  if (!nrow(df)) {
    out <- df
    rownames(out) <- NULL
    return(out)
  }
  if (anyNA(df[[caseid_col]]) || any(!nzchar(as.character(df[[caseid_col]])))) {
    stop("Every case id must be present. Missing ids are not dropped silently.")
  }
  version <- suppressWarnings(as.numeric(df[[caseversion_col]]))
  if (any(!is.finite(version))) {
    stop("Every case version must be a finite number. Missing versions are not dropped silently.")
  }
  # Ascending version, then ascending input order. duplicated(fromLast)
  # then keeps the highest version, and the later input row when versions tie.
  ord <- order(df[[caseid_col]], version, seq_len(nrow(df)), method = "radix")
  sorted <- df[ord, , drop = FALSE]
  keep <- !duplicated(sorted[[caseid_col]], fromLast = TRUE)
  out <- sorted[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}
