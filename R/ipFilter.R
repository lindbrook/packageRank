#' Identify IP's that are mirroring CRAN (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @param cutpoint Numeric. Threshold number of unique packages downloaded.
#' @export

ipFilter <- function(dat, cutpoint = 15000L) {
  # number of unique packages downloaded by ip address.
  crosstab <- tapply(dat$package, dat$ip_id, function(x) length(unique(x)))
  df <- data.frame(ip = names(crosstab), count = c(crosstab), row.names = NULL)
  as.numeric(df[df$count >= cutpoint, "ip"])
}
