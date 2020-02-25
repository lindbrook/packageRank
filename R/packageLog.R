#' Package download counts and rank percentiles (cross-sectional).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param filter Logical or Numeric. If Logical, TRUE filters out downloads less than 1000 bytes. If Numeric, a postive value (bytes) sets the minimum download size to consider; a negative value sets the maximum download size to consider.
#' @param memoization Logical. Use memoization when downloading logs.
#' @return An R data frame.
#' @export

packageLog <- function(packages = NULL, date = Sys.Date() - 1,
  filter = FALSE, memoization = TRUE) {

  cran_log <- fetchLog2(date = date, memoization = memoization)
  cran_log <- as.data.frame(cran_log[!is.na(cran_log$package), ])

  if (filter) {
    if (is.numeric(filter)) {
      if (filter >= 0) {
          cran_log <- cran_log[cran_log$size >= filter, ]
        } else if (filter < 0) {
          cran_log <- cran_log[cran_log$size < -filter, ]
        }
    } else if (is.logical(filter)) {
      cran_log <- cran_log[cran_log$size >= 1000, ]
    } else stop("'filter' must be Logical or Numeric.")
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
