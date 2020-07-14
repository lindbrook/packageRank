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
  sel <- !is.na(cran_log$package) &
         !is.na(cran_log$country) &
         !is.na(cran_log$size)

  cran_log <- cran_log[sel, ]
  crosstab <- sort(table(cran_log$country), decreasing = TRUE)

  out <- list(date = date, data = crosstab)
  class(out) <- "countryDistribution"
  out
}

#' Plot to 10 package downloads by country domain.
#'
#' Plot method for packageDistribution().
#' @param x An object of class "countryDistribution" created by \code{countryDistribution()}.
#' @param ... Additional plotting parameters.
#' @export

plot.countryDistribution <- function(x, ...) {
  barplot(x$data[1:10])
  title(paste("Top Ten Country Domains @", x$date))
}
