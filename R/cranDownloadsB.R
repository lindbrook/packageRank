#' Filtered package downloads from the RStudio CRAN mirror (protoype).
#'
#' triplet, ip and small filters.
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param check.package Logical. Validate and "spell check" package.
#' @param triplet.filter Logical.
#' @param ip.filter Logical.
#' @param small.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @export

cranDownloadsB <- function(packages = "HistData", date = Sys.Date() - 1,
  check.package = TRUE, triplet.filter = TRUE, ip.filter = TRUE,
  small.filter = TRUE, memoization = TRUE, dev.mode = FALSE) {

  if (check.package) packages <- checkPackage(packages, dev.mode)
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)

  cran_log <- packageLog2(packages = packages, date = ymd,
    triplet.filter = FALSE, small.filter = FALSE, ip.filter = FALSE)

  if (is.data.frame(cran_log)) {
    ct <- nrow(cran_log)
  } else {
    ct <- vapply(cran_log, nrow, integer(1L))
  }

  f.cran_log <- packageLog2(packages = packages, date = ymd,
    triplet.filter = triplet.filter, ip.filter = ip.filter,
    small.filter = small.filter)

  if (is.data.frame(f.cran_log)) {
    f.ct <- nrow(f.cran_log)
  } else {
    f.ct <- vapply(f.cran_log, nrow, integer(1L))
  }

  data.frame(date = date, package = packages, downloads = ct,
    filtered.downloads = f.ct, row.names = NULL)
}
