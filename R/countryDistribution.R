#' Tabulate package downloads by country.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

countryDistribution <- function(date = Sys.Date() - 1, memoization = TRUE) {
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  cran_log <- cran_log[!is.na(cran_log$country), ]
  freqtab <- sort(table(cran_log$country), decreasing = TRUE)
  out <- list(date = date, data = freqtab)
  class(out) <- "countryDistribution"
  out
}

#' Plot top 10 package downloads by country domain.
#'
#' Plot method for packageDistribution().
#' @param x An object of class "countryDistribution" created by \code{countryDistribution()}.
#' @param ... Additional plotting parameters.
#' @export

plot.countryDistribution <- function(x, ...) {
  ct <- x$data / 10^6
  barplot(ct[1:10], ylab = "Downloads (Millions)")
  title(paste("Top Ten Country Domains @", x$date))
}
