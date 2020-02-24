#' Tabulate a IP address's package downloads (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param ip Numeric. ip_id (anonymized IP address).
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @export

ipPackage <- function(ip = 1, date = Sys.Date() - 1,
  memoization = TRUE, sort = TRUE) {

  cran_log <- fetchLog2(date = date, memoization = memoization)
  cran_log <- cran_log[!is.na(cran_log$package) , ]
  crosstab <- table(cran_log[cran_log$ip_id == ip, "package"])

  if (sort) {
    sort(crosstab, decreasing = TRUE)
  } else {
    crosstab
  }
}
