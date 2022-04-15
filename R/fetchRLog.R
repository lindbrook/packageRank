#' Fetch R download Logs.
#'
#' @param date Character. Date. yyyy-mm-dd.
#' @export

fetchRLog <- function(date) {
  year <- as.POSIXlt(date)$year + 1900
  rstudio.url <- "http://cran-logs.rstudio.com/"
  log.url <- paste0(rstudio.url, year, '/', date, "-r.csv.gz")
  if (RCurl::url.exists(log.url)) {
    mfetchLog(log.url)
  } else stop("Log not available.")
}
