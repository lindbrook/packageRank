#' Fetch CRAN Logs.
#'
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param dev.mode Logical. Use Base R code.
#' @export

fetchCranLog <- function(date, memoization = FALSE, dev.mode = FALSE) {
  if (date > Sys.Date()) stop("Can't see into the future!", call. = FALSE)
  year <- as.POSIXlt(date)$year + 1900
  rstudio.url <- "http://cran-logs.rstudio.com/"
  log.url <- paste0(rstudio.url, year, '/', date, ".csv.gz")

  if (RCurl::url.exists(log.url)) {
    if (dev.mode) {
      if (memoization) {
        cran_log <- mfetchLog2(log.url)
      } else {
        cran_log <- fetchLog2(log.url)
      }

    } else {
      if (memoization) {
        cran_log <- mfetchLog(log.url)
      } else {
        cran_log <- fetchLog(log.url)
      }
    }
  } else {
    msg <- "Check your internet connection or try the previous day."
    stop("Log for ", date, " not (yet) available. ", msg, call. = FALSE)
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
#' Base R version.
#' @param x Character. URL.
#' @export
#' @note mfetchLogBase() is memoized version.

fetchLogBase <- function(x) {
  tmp <- tempfile(fileext = ".gz")
  utils::download.file(x, tmp)
  tmp <- gzfile(tmp, "rt")
  out <- utils::read.csv(tmp)
  close(tmp)
  out
}

# mfetchLogBase <- memoise::memoise(fetchLogBase)

#' Get gzipped data at URL.
#'
#' Base R version.
#' @param x Character. URL.
#' @export
#' @note mfetchLog2() is memoized version.

fetchLog2 <- function(x) {
  con <- gzcon(url(x))
  raw <- textConnection(readLines(con))
  close(con)
  dat <- utils::read.csv(raw)
  close(raw)
  dat
}

#' Get gzipped data at URL.
#'
#' Base R version.
#' @param x Character. URL.
#' @export
#' @note mfetchLog2() is memoized version.

mfetchLog2 <- function(x) {
  con <- gzcon(url(x))
  raw <- textConnection(mreadLines(con))
  close(con)
  dat <- utils::read.csv(raw)
  close(raw)
  dat
}

mreadLines <- memoise::memoise(readLines)
