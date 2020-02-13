#' Package download counts by country (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @param na.rm Logical. Remove NAs.
#' @export

packageCountry <- function(packages = NULL, date = Sys.Date() - 1,
  memoization = TRUE, sort = FALSE, na.rm = FALSE) {

  dat <- packageLog(packages = packages, date = date, memoization = memoization)
  if (na.rm) {
    crosstab <- table(dat$country)
  } else {
    crosstab <- table(dat$country, useNA = "always")
  }
  if (sort) {
    sort(crosstab, decreasing = TRUE)
  } else {
    crosstab
  }
}
