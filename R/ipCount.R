#' Count number of IP addresses.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort.count Logical. Sort by download count.
#' @export

ipCount <- function(date = NULL, memoization = TRUE, sort.count = TRUE) {
  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  freqtab <- table(cran_log$ip_id)
  if (sort.count) sort(freqtab, decreasing = TRUE)
  else freqtab
}
