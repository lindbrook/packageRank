#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param small.filter Logical.
#' @param triplet.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @return An R data frame.
#' @export

packageLog <- function(packages = NULL, date = Sys.Date() - 1,
  small.filter = TRUE, triplet.filter = TRUE, memoization = TRUE) {

  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- as.data.frame(cran_log[!is.na(cran_log$package), ])

  if (!is.null(packages)) {
    cran_log <- cran_log[cran_log$package %in% packages, ]
  }

  if (triplet.filter) {
    cran_log <- tripletFilter(cran_log, small.filter = FALSE)
  }

  if (small.filter) {
    cran_log <- smallFilter(cran_log, filter = FALSE)
  }

  if (!is.null(packages)) {
    cran_log <- cran_log[order(cran_log$package, cran_log$time), ]
  } else {
    cran_log <- cran_log[order(cran_log$time), ]
  }

  # row.names(cran_log) <- NULL
  cran_log
}
