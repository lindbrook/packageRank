#' Package download counts by country.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. yyyy-mm-dd.
#' @param sort Logical. Sort by download count.
#' @param na.rm Logical. Remove NAs.
#' @param triplet.filter Logical.
#' @param ip.filter Logical.
#' @param small.filter Logical.
#' @param sequence.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @export

packageCountry <- function(packages = "cholera", date = Sys.Date() - 1,
  memoization = TRUE, sort = TRUE, na.rm = FALSE, triplet.filter = TRUE,
  ip.filter = TRUE, small.filter = TRUE, sequence.filter = TRUE,
  check.package = TRUE) {

  p.log <- packageLog(packages = packages, date = date,
    triplet.filter = triplet.filter, ip.filter = ip.filter,
    small.filter = small.filter, sequence.filter = sequence.filter,
    memoization = memoization, check.package = check.package)

  if (na.rm) {
    if (is.data.frame(p.log)) {
      out <- table(p.log$country)
    } else if (is.list(p.log)) {
      out <- lapply(p.log, function(x) table(x$country))
    }
  } else {
    if (is.data.frame(p.log)) {
      out <- table(p.log$country, useNA = "ifany")
    } else if (is.list(p.log)) {
      out <- lapply(p.log, function(x) table(x$country, useNA = "ifany"))
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
