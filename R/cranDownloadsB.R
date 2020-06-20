#' Filtered package downloads from the RStudio CRAN mirror. (protoype)
#'
#' Small and triplet filter.
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param check.package Logical. Validate and "spell check" package.
#' @param small.filter Logical.
#' @param triplet.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @export

cranDownloadsB <- function(packages = "HistData", date = Sys.Date() - 1,
  check.package = TRUE, small.filter = TRUE, triplet.filter = TRUE,
  memoization = TRUE, dev.mode = FALSE) {

  if (check.package) packages <- checkPackage(packages, dev.mode)
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)

  cran_log <- packageLog(packages = packages, date = ymd, small.filter = FALSE,
    triplet.filter = FALSE)
  ct <- vapply(cran_log, nrow, integer(1L))

  f.cran_log <- packageLog(packages = packages, date = ymd,
    small.filter = small.filter, triplet.filter = triplet.filter)
  f.ct <- vapply(f.cran_log, nrow, integer(1L))

  data.frame(date = date, package = packages, downloads = ct,
    filtered.downloads = f.ct, row.names = NULL)
}
