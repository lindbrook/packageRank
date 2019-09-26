#' Package download counts and rank percentiles (cross-sectional).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param memoization Logical. Use memoization when downloading logs.
#' @return An R data frame.
#' @export

packageLog <- function(packages = "HistData", date = Sys.Date() - 1,
  memoization = TRUE) {

  ymd <- as.Date(date)
  if (ymd > Sys.Date()) stop("Can't see into the future!")
  year <- as.POSIXlt(ymd)$year + 1900
  rstudio.url <- "http://cran-logs.rstudio.com/"
  url <- paste0(rstudio.url, year, '/', ymd, ".csv.gz")

  if (RCurl::url.exists(url)) {
    if (memoization) cran_log <- mfetchLog(url)
    else cran_log <- fetchLog(url)
  } else {
    msg <- "Check your internet connection or try the previous day."
    stop("Log for ", date, " not (yet) available. ", msg)
  }

  cran_log <- cran_log[-which(is.na(cran_log$package)), ]
  pkg_log <- cran_log[cran_log$package %in% packages, ]
  pkg_log[order(pkg_log$package), ]
}
