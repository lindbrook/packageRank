#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param size.filter Logical or Numeric. If Logical, TRUE filters out downloads less than 1000 bytes. If Numeric, a positive value (bytes) sets the minimum download size to consider; a negative value sets the maximum download size to consider.
#' @param memoization Logical. Use memoization when downloading logs.
#' @return An R data frame.
#' @export

packageLog <- function(packages = NULL, date = Sys.Date() - 1,
  size.filter = FALSE, memoization = TRUE) {

  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- as.data.frame(cran_log[!is.na(cran_log$package), ])

  if (size.filter) {
    if (is.numeric(size.filter)) {
      if (size.filter >= 0) {
          cran_log <- cran_log[cran_log$size >= size.filter, ]
        } else if (size.filter < 0) {
          cran_log <- cran_log[cran_log$size < -size.filter, ]
        }
    } else if (is.logical(size.filter)) {
      cran_log <- cran_log[cran_log$size >= 1000, ]
    } else stop("'size.filter' must be Logical or Numeric.")
  }

  if (!is.null(packages)) {
    cran_log <- cran_log[cran_log$package %in% packages, ]
    cran_log <- cran_log[order(cran_log$package, cran_log$time), ]
  } else {
    cran_log <- cran_log[order(cran_log$time), ]
  }

  row.names(cran_log) <- NULL
  cran_log
}
