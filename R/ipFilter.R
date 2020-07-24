#' Identify IP's that are mirroring CRAN (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. yyyy-mm-dd.
#' @param cutpoint Numeric. Threshold number of unique packages downloaded.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

ipFilter <- function(date = Sys.Date() - 1, cutpoint = 15000L,
  memoization = TRUE) {

  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)

  sel <- !is.na(cran_log$package) &
         !is.na(cran_log$size)
  cran_log <- cran_log[sel, ]

  # number of unique packages downloaded by ip address.
  crosstab <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  df <- data.frame(ip = names(crosstab), count = c(crosstab), row.names = NULL)
  as.numeric(df[df$count >= cutpoint, "ip"])
}
