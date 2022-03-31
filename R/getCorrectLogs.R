#' Get correct download logs to replace duplicates in 'cranlogs'.
#'
#' Correct for eight duplicates.
#' @param date Character. Date. "yyyy-mm-dd".
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
