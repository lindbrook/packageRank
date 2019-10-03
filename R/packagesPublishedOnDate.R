#' Packages published on given date (prototype).
#'
#' Uses tools::CRAN_package_db().
#' @param date \code{yyyy-mm-dd}.
#' @export
#' @note check "2019-01-01"

packagesPublishedOnDate <- function(date = "2019-10-01") {
  if (is.na(as.Date(date, optional = TRUE))){
    stop("Invalid date.")
  } else {
    cran_db <- tools::CRAN_package_db()
    priority.recommended <- vapply(cran_db$Depends, function(x) {
      grepl("R \\(>= 3.7\\)", x)
    }, logical(1L))
    dat <- cran_db[!priority.recommended, ]
    dat[dat$Published == date, "Package"]
  }
}
