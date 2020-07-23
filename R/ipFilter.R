#' Identify IP's that are mirroring CRAN (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. yyyy-mm-dd.
#' @param cutpoint Numeric. Threshold number of downloads.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

ipFilter <- function(date = Sys.Date() - 1, cutpoint = 10000L,
  memoization = TRUE) {

  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)

  sel <- !is.na(cran_log$package) &
         !is.na(cran_log$size)
  cran_log <- cran_log[sel, ]

  crosstab <- tapply(cran_log$package, cran_log$ip_id, length)
  df <- data.frame(ip = names(crosstab), count = c(crosstab),
    row.names = NULL)
  as.numeric(unique(df[df$count >= cutpoint, "ip"]))
}
