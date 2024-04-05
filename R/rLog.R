#' Get R Application Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. "yyyy-mm-dd". NULL uses last available log.
#' @export

rLog <- function(date = NULL) {
  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  log.date <- logDate(date)
  year <- as.numeric(format(log.date, "%Y"))
  url.root <- "http://cran-logs.rstudio.com/"
  url <- paste0(url.root, year, '/', log.date, "-r.csv.gz")
  log <- mfetchLog(url)
  log$date.time <- dateTime(log$date, log$time)
  log <- log[order(log$date.time), ]
  log$date.time <- NULL
  log
}
