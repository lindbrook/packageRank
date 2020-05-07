#' Tabulate a country's package downloads.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param country Character. country abbreviation.
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @export

countryPackage <- function(country = "US", date = Sys.Date() - 1,
  memoization = TRUE, sort = TRUE) {

  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  sel <- !is.na(cran_log$package) | !is.na(cran_log$country)
  cran_log <- cran_log[sel, ]
  crosstab <- table(cran_log[cran_log$country == country, "package"])

  if (sort) {
    sort(crosstab, decreasing = TRUE)
  } else {
    crosstab
  }
}
