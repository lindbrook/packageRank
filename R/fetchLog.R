#' Fetch CRAN Logs.
#'
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param dev.mode Logical. Use Base R code.
#' @noRd

fetchCranLog <- function(date, memoization = FALSE, dev.mode = FALSE) {
  year <- as.POSIXlt(date)$year + 1900
  rstudio.url <- "http://cran-logs.rstudio.com/"
  log.url <- paste0(rstudio.url, year, '/', date, ".csv.gz")

  # R default is 60
  orig.timeout <- getOption("timeout")
  if (orig.timeout < 600L) options(timeout = 600L)

  if (RCurl::url.exists(log.url)) {
    if (dev.mode) {
      cran_log <- fetchLog2(log.url, memoization)
    } else {
      if (memoization) {
        cran_log <- mfetchLog(log.url)
      } else {
        cran_log <- fetchLog(log.url)
      }
    }
  } else {
    stop("Log for ", date, " not (yet) on server.", call. = FALSE)
  }

  options(timeout = orig.timeout)
  cran_log
}

#' fread() to data.frame.
#'
#' @param x Character. URL.
#' @import memoise
#' @importFrom data.table fread
#' @importFrom R.utils decompressFile
#' @note mfetchLog() is memoized version.
#' @noRd

fetchLog <- function(x) {
  # fread(colClasses != NULL) slower up to 80 MB.
  # cls <- c("Date", "character", "integer", rep("character", 6), "integer")
  # cls <- list(Date = "date", integer = c("size", "ip_id"), character = c("time", 
  #   "r_version", "r_arch", "r_os", "package", "version", "country"))
  # data.table::fread(x, data.table = FALSE, colClasses = cls)
  data.table::fread(x, data.table = FALSE)
}

mfetchLog <- memoise::memoise(fetchLog)

#' Get gzipped data at URL.
#'
#' Base R version.
#' @param x Character. URL.
#' @param memoization Logical. Use memoization
#' @noRd

fetchLog2 <- function(x, memoization = FALSE) {
  con <- gzcon(url(x))
  if (memoization) {
    raw <- textConnection(readLines(con))
  } else {
    raw <- textConnection(mreadLines(con))
  }
  close(con)
  dat <- utils::read.csv(raw)
  close(raw)
  dat
}

#' Memoized readLines().
#'
#' @noRd

mreadLines <- memoise::memoise(readLines)
