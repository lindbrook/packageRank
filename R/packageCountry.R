#' Package download counts by country.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @param na.rm Logical. Remove NAs.
#' @param small.filter Logical.
#' @param triplet.filter Logical.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @export

packageCountry <- function(packages = NULL, date = Sys.Date() - 1,
  memoization = TRUE, sort = TRUE, na.rm = FALSE, small.filter = TRUE,
  triplet.filter = TRUE, check.package = TRUE, dev.mode = FALSE) {

  if (!is.null(packages)) {
    if (!"R" %in% packages) {
      if (check.package) {
        packages <- checkPackage(packages, dev.mode)
      }
    }
  }

  lst <- packageLog(packages = packages, date = date, memoization = memoization,
    small.filter = small.filter, triplet.filter = triplet.filter)

  if (na.rm) {
    out <- lapply(lst, function(x) table(x$country))
  } else {
    out <- lapply(lst, function(x) table(x$country, useNA = "ifany"))
  }

  if (sort) {
    out <- lapply(out, function(x) sort(x, decreasing = TRUE))
  }

  names(out) <- packages
  out
}
