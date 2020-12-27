#' Count number of IP addresses.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @param dev.mode Logical. Use fetchLogBase().
#' @export

ipCount <- function(date = NULL, memoization = TRUE, sort = TRUE,
  dev.mode = FALSE) {

  if (is.null(date)) ymd <- logDate()
  else ymd <- checkDate(date)
  ymd <- fixDate_2012(ymd)

  cran_log <- fetchCranLog(date = ymd, memoization = memoization,
    dev.mode = dev.mode)
  cran_log <- cleanLog(cran_log)
  freqtab <- table(cran_log$ip_id)
  if (sort) sort(freqtab, decreasing = TRUE)
  else freqtab
}
