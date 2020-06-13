#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param small.filter Logical.
#' @param triplet.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @return An R data frame.
#' @export

packageLog <- function(packages = NULL, date = Sys.Date() - 1,
  small.filter = TRUE, triplet.filter = TRUE, memoization = TRUE,
  check.package = TRUE, dev.mode = FALSE) {

  if (!is.null(packages)) {
    if (!"R" %in% packages) {
      if (check.package) {
        packages <- checkPackage(packages, dev.mode)
      }
    } # stop()
  }

  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- as.data.frame(cran_log[!is.na(cran_log$package), ])

  if (!is.null(packages)) {
    cran_log <- lapply(packages, function(p) cran_log[cran_log$package == p, ])
  }

  if (triplet.filter) {
    cran_log <- lapply(cran_log, function(x) {
      filtered.data <- tripletFilter(x, small.filter = FALSE)
      do.call(rbind, filtered.data)
    })
  }

  if (small.filter) {
    cran_log <- lapply(cran_log, function(x) {
      smallFilter(x, filter = small.filter)
    })
  }

  cran_log <- lapply(cran_log, function(x) x[order(x$time), ])
  names(cran_log) <- packages
  cran_log
}
