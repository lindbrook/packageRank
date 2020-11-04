#' Count number of IP addresses.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @export

ipCount <- function(date = Sys.Date() - 1, memoization = TRUE, sort = TRUE) {
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  freqtab <- table(cran_log$ip_id)
  if (sort) sort(freqtab, decreasing = TRUE)
  else freqtab
}
