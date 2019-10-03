#' Packages published on given date (prototype).
#'
#' Uses tools::CRAN_package_db().
#' @param date \code{yyyy-mm-dd}.
#' @export
#' @note check "2019-01-01"

packagesPublishedOnDate <- function(date = Sys.Date()) {
  if (is.na(as.Date(date, optional = TRUE))) {
    stop("Invalid date/format (yyyy-mm-dd).")
  } else if (date > Sys.Date()) {
    stop("Date in future.")
  } else {
    cran_db <- tools::CRAN_package_db()
    priority.recommended <- vapply(cran_db$Depends, function(x) {
      grepl("R \\(>= 3.7\\)", x)
    }, logical(1L))
    dat <- cran_db[!priority.recommended, ]
    dat[dat$Published == date, "Package"]
  }
}
