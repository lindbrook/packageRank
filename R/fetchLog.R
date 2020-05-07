#' fread() to data.frame.
#'
#' @param x Character. URL
#' @import data.table memoise
#' @importFrom R.utils decompressFile
#' @export
#' @note mFetchLog() is memoized version.

fetchLog <- function(x) as.data.frame(data.table::fread(x))
mfetchLog <- memoise::memoise(fetchLog)

#' Fetch CRAN Logs.
#'
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

fetchCranLog <- function(date, memoization) {
  if (date > Sys.Date()) stop("Can't see into the future!")
  year <- as.POSIXlt(date)$year + 1900
  rstudio.url <- "http://cran-logs.rstudio.com/"
  url <- paste0(rstudio.url, year, '/', date, ".csv.gz")

  if (RCurl::url.exists(url)) {
    if (memoization) {
      cran_log <- mfetchLog(url)
    } else {
      cran_log <- fetchLog(url)
    }
  } else {
    msg <- "Check your internet connection or try the previous day."
    stop("Log for ", date, " not (yet) available. ", msg)
  }
}
