#' Fetch CRAN Logs.
#'
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param dev.mode Logical. Use Base R code.
#' @export

fetchCranLog <- function(date, memoization = FALSE, dev.mode = FALSE) {
  if (date > Sys.Date()) stop("Can't see into the future!")
  year <- as.POSIXlt(date)$year + 1900
  rstudio.url <- "http://cran-logs.rstudio.com/"
  log.url <- paste0(rstudio.url, year, '/', date, ".csv.gz")

  if (RCurl::url.exists(log.url)) {
    if (dev.mode) {
      cran_log <- mfetchLogBase(log.url)
    } else {
      if (memoization) {
        cran_log <- mfetchLog(log.url)
      } else {
        cran_log <- fetchLog(log.url)
      }
    }
  } else {
    msg <- "Check your internet connection or try the previous day."
    stop("Log for ", date, " not (yet) available. ", msg)
  }
  cran_log
}

#' fread() to data.frame.
#'
#' @param x Character. URL.
#' @import data.table memoise
#' @importFrom R.utils decompressFile
#' @export
#' @note mfetchLog() is memoized version.

fetchLog <- function(x) data.table::fread(x, data.table = FALSE)
mfetchLog <- memoise::memoise(fetchLog)

#' Get gzipped data at URL.
#'
#' @param x Character. URL.
#' @export
#' @note mfetchLogBase() is memoized version.

fetchLogBase <- function(x) {
  connection <- gzcon(url(x))
  dat <- readLines(connection)
  cran_log <- utils::read.csv(textConnection(dat))
}

mfetchLogBase <- memoise::memoise(fetchLogBase)
