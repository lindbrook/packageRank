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

#' Get correct download logs to replace doubled results in 'cranlogs'.
#'
#' Correct for 13 days with 2x results 2023-09-19 through 2023-10-01.
#' @note This documents code for replication.
#' @return A list with thirteen elements.
#' @noRd

getCorrectLogs2023 <- function() {
  dates <- seq.Date(as.Date("2023-09-19"), as.Date("2023-10-01"), by = "days")
  logs <- lapply(dates, fetchCranLog)
  names(logs) <- dates
  logs
}
