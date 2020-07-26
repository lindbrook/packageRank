#' Package download count  by IP distribution summary.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

ipDownloads <- function(date = Sys.Date() - 1, memoization = TRUE) {
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  sel <- !is.na(cran_log$package) & !is.na(cran_log$size)
  cran_log <- cran_log[sel, ]

  crosstab <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  out <- data.frame(ip = names(crosstab), count = c(crosstab), row.names = NULL)
  out[order(out$count, decreasing = TRUE), ]
}
