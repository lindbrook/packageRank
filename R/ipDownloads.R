#' Unique package download counts by IP address.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param dev.mode Logical. Use fetchLogBase().
#' @export

ipDownloads <- function(date = NULL, memoization = TRUE, dev.mode = FALSE) {
  ymd <- logDate(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization,
    dev.mode = dev.mode)
  cran_log <- cleanLog(cran_log)

  freqtab <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  out <- data.frame(ip = names(freqtab), count = c(freqtab))
  out <- out[order(out$count, decreasing = TRUE), ]
  row.names(out) <- NULL
  out
}
