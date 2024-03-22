#' Unique package download counts by IP address.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

ipDownloads <- function(date = NULL, memoization = TRUE) {
  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)

  freqtab <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  out <- data.frame(ip = names(freqtab), count = c(freqtab))
  out <- out[order(out$count, decreasing = TRUE), ]
  row.names(out) <- NULL
  out
}
