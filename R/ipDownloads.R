#' Unique package download counts by IP address.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

ipDownloads <- function(date = Sys.Date() - 1, memoization = TRUE) {
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)

  freqtab <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  out <- data.frame(ip = names(freqtab), count = c(freqtab))
  out <- out[order(out$count, decreasing = TRUE), ]
  row.names(out) <- NULL
  out
}
