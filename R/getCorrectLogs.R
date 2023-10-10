#' Get correct download logs to replace duplicates in 'cranlogs'.
#'
#' Correct for eight duplicates.
#' @note This documents code for the rstudio.logs R list object.
#' @return A list with eight elements.
#' @noRd

getCorrectLogs <- function() {
  cranlogs_duplicates <- c("2012-10-06", "2012-10-07", "2012-10-08",
    "2012-10-11", "2012-12-26", "2012-12-27", "2012-12-28", "2013-01-01")
  d <- lapply(cranlogs_duplicates, logDate)
  logs <- lapply(d, fetchCranLog)
  names(logs) <- cranlogs_duplicates
  logs
}

# rstudio.logs <- getCorrectLogs()
# usethis::use_data(rstudio.logs)

#' Get multiple download logs for R application or packages.
#'
#' @param first Character or Date. Start date.
#' @param last Character or Date. End date.
#' @param log Character. Type of log: "packages" or "R".
#' @note Code for replication.
#' @return A list of log data frames.
#' @noRd

getLogs <- function(first = "2023-09-19", last = "2023-10-01", 
  log = "packages") {

  dates <- seq.Date(logDate(first), logDate(last), by = "days")
  if (log == "packages") fn <- fetchCranLog
  else if (log == "R") fn <- fetchRLog
  else stop('log must be "packages" or "R".', call. = FALSE)
  logs <- lapply(dates, fn)
  names(logs) <- dates
  logs
}
