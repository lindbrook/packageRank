#' Package download counts by country.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @param na.rm Logical. Remove NAs.
#' @export

packageCountry <- function(packages = NULL, date = Sys.Date() - 1,
  memoization = TRUE, sort = TRUE, na.rm = FALSE) {

  dat <- packageLog(packages = packages, date = date, memoization = memoization)

  if (length(packages) == 1) {
    if (na.rm) {
      out <- table(dat$country)
    } else {
      out <- table(dat$country, useNA = "ifany")
    }
    if (sort) {
      out <- sort(out, decreasing = TRUE)
    } else {
      out
    }
  } else if (length(packages) > 1) {
    if (na.rm) {
      out <- lapply(packages, function(pkg) {
         table(dat[dat$package == pkg, "country"])
      })
    } else {
      out <- lapply(packages, function(pkg) {
         table(dat[dat$package == pkg, "country"], useNA = "ifany")
      })
    }
    if (sort) {
      out <- lapply(out, function(x) sort(x, decreasing = TRUE))
    } else {
      out
    }
    names(out) <- packages
  }
  out
}
