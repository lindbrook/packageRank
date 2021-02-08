#' Filtered package downloads from the RStudio CRAN mirror (protoype).
#'
#' triplet, ip and small filters.
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param check.package Logical. Validate and "spell check" package.
#' @param ip.filter Logical.
#' @param triplet.filter Logical.
#' @param small.filter Logical.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

filteredDownloads <- function(packages = "HistData", date = NULL,
  check.package = TRUE, ip.filter = TRUE, triplet.filter = TRUE,
  small.filter = TRUE, sequence.filter = TRUE, size.filter = TRUE,
  memoization = TRUE) {

  if (check.package) packages <- checkPackage(packages)

  ymd <- logDate(date)
  cran_log <- packageLog0(packages = packages, date = ymd,
    memoization = memoization)

  if (is.data.frame(cran_log)) {
    ct <- nrow(cran_log)
  } else {
    ct <- vapply(cran_log, nrow, integer(1L))
  }

  f.cran_log <- packageLog(packages = packages, date = ymd,
    ip.filter = ip.filter, triplet.filter = triplet.filter,
    small.filter = small.filter, sequence.filter = sequence.filter,
    size.filter = size.filter, memoization = memoization)

  if (is.data.frame(f.cran_log)) {
    f.ct <- nrow(f.cran_log)
  } else {
    f.ct <- vapply(f.cran_log, nrow, integer(1L))
  }

  inflation <- round(100 * (ct - f.ct) / f.ct, 2)

  data.frame(date = ymd, package = packages, downloads = ct,
    filtered.downloads = f.ct, inflation = inflation, row.names = NULL)
}
