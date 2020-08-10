#' Package download counts by country.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @param na.rm Logical. Remove NAs.
#' @param triplet.filter Logical.
#' @param ip.filter Logical.
#' @param small.filter Logical.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @export

packageCountry <- function(packages = NULL, date = Sys.Date() - 1,
  memoization = TRUE, sort = TRUE, na.rm = FALSE, triplet.filter = TRUE,
  ip.filter = TRUE, small.filter = TRUE, check.package = TRUE,
  dev.mode = FALSE) {

  if (!is.null(packages)) {
    if (!"R" %in% packages) {
      if (check.package) {
        packages <- checkPackage(packages, dev.mode)
      }
    }
  }

  lst <- packageLog2(packages = packages, date = date,
    memoization = memoization, triplet.filter = triplet.filter,
    ip.filter = ip.filter, small.filter = small.filter)

  if (na.rm) {
    if (is.data.frame(lst)) {
      out <- table(lst$country)
    } else if (is.list(lst)) {
      out <- lapply(lst, function(x) table(x$country))
    }
  } else {
    if (is.data.frame(lst)) {
      out <- table(lst$country, useNA = "ifany")
    } else if (is.list(lst)) {
      out <- lapply(lst, function(x) table(x$country, useNA = "ifany"))
    }
  }

  if (sort) {
    if (is.table(out)) {
      out <- sort(out, decreasing = TRUE)
    } else if (is.list(out)) {
      out <- lapply(out, function(x) sort(x, decreasing = TRUE))
      names(out) <- packages
    }
  }

  out
}
