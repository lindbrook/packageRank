#' Identify IP's that are mirroring CRAN (standalone prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date.
#' @param cutpoint Numeric. Threshold of unique packages downloaded.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

ipFilter0 <- function(date = Sys.Date() - 1, cutpoint = 5000L,
  memoization = TRUE) {

  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  sel <- !is.na(cran_log$package) & !is.na(cran_log$size)
  cran_log <- cran_log[sel, ]

  crosstab <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  df <- data.frame(ip = names(crosstab), count = c(crosstab), row.names = NULL)
  # df[df$count >= quantile(df$count, cutpoint), "ip"]
  as.numeric(df[df$count >= cutpoint, "ip"])
}
