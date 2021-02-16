#' Filtered package downloads from the RStudio CRAN mirror (prototype).
#'
#' ip, triplet, small, sequence and size filters.
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param triplet.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param check.package Logical. Validate and "spell check" package.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

filteredDownloads <- function(packages = "HistData", date = NULL,
  all.filters = TRUE, ip.filter = FALSE, triplet.filter = FALSE,
  small.filter = FALSE, sequence.filter = FALSE, size.filter = FALSE,
  check.package = TRUE, memoization = TRUE) {

  if (check.package) packages <- checkPackage(packages)

  ymd <- logDate(date)
  cran_log <- packageLog(packages = packages, date = ymd,
    memoization = memoization)

  if (is.data.frame(cran_log)) {
    ct <- nrow(cran_log)
  } else {
    ct <- vapply(cran_log, nrow, integer(1L))
  }

  f.cran_log <- packageLog(packages = packages, date = ymd,
    all.filters = all.filters, ip.filter = ip.filter,
    triplet.filter = triplet.filter, small.filter = small.filter,
    sequence.filter = sequence.filter, size.filter = size.filter,
    memoization = memoization)

  if (is.data.frame(f.cran_log)) {
    f.ct <- nrow(f.cran_log)
  } else {
    f.ct <- vapply(f.cran_log, nrow, integer(1L))
  }

  inflation <- round(100 * (ct - f.ct) / f.ct, 2)

  data.frame(date = ymd, package = packages, downloads = ct,
    filtered.downloads = f.ct, inflation = inflation, row.names = NULL)
}
