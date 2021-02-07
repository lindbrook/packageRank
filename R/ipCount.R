#' Count number of IP addresses.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @export

ipCount <- function(date = NULL, memoization = TRUE, sort = TRUE) {
  ymd <- logDate(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  freqtab <- table(cran_log$ip_id)
  if (sort) sort(freqtab, decreasing = TRUE)
  else freqtab
}
