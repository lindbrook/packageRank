#' Package download counts by country.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param triplet.filter Logical.
#' @param small.filter Logical.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param sort Logical. Sort by download count.
#' @param na.rm Logical. Remove NAs.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export

packageCountry <- function(packages = "cholera", date = NULL,
  all.filters = FALSE, ip.filter = FALSE, triplet.filter = FALSE,
  small.filter = FALSE, sequence.filter = FALSE, size.filter = FALSE,
  sort = TRUE, na.rm = FALSE, memoization = TRUE, check.package = TRUE,
  multi.core = TRUE, dev.mode = FALSE) {

  if (all.filters) {
    ip.filter <- TRUE
    triplet.filter <- TRUE
    small.filter <- TRUE
    sequence.filter <- TRUE
    size.filter <- TRUE
  }

  p.log <- packageLog(packages = packages, date = date,
    ip.filter = ip.filter, triplet.filter = triplet.filter,
    small.filter = small.filter, sequence.filter = sequence.filter,
    size.filter = size.filter, memoization = memoization,
    check.package = check.package, multi.core = TRUE, dev.mode = FALSE)

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
