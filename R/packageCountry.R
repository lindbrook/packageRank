#' Package download counts by country.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param version.filter Logical. TRUE selects only most recent version.
#' @param sort Logical. Sort by download count.
#' @param na.rm Logical. Remove NAs.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

packageCountry <- function(packages = "cholera", date = NULL,
  all.filters = FALSE, ip.filter = FALSE, sequence.filter = FALSE,
  size.filter = FALSE, small.filter = FALSE, version.filter = FALSE, 
  sort = TRUE, na.rm = FALSE, memoization = TRUE, check.package = TRUE, 
  multi.core = FALSE) {

  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
    size.filter <- TRUE
    version.filter <- TRUE
  }

  p.log <- packageLog(packages = packages, date = date,
    all.filters = all.filters, ip.filter = ip.filter, 
    sequence.filter = sequence.filter, size.filter = size.filter, 
    small.filter = small.filter, version.filter = version.filter,
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
      names(out) <- names(p.log)
    }
  }

  out
}
