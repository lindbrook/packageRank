#' Compute Effective CRAN Log Date Based on Local and UTC Time (prototype).
#'
#' RStudio CRAN Mirror Logs for previous day are posted at 17:00:00 UTC.
#' @param date Character. Date of desired log \code{"yyyy-mm-dd"}. NULL returns date of latest available log.
#' @param check.url Logical.
#' @param repository Character. "CRAN" or "MRAN". RStudio CRAN mirror log or Microsoft MRAN snapshot.
#' @param tz Character. Time zone. See OlsonNames().
#' @param upload.time Character. UTC upload time for logs "hh:mm" or "hh:mm:ss".
#' @param warning.msg Logical. TRUE uses warning() if the function returns the date of the previous available log.
#' @return An R date object.
#' @export

logDate <- function(date = NULL, check.url = TRUE, repository = "CRAN",
  tz = Sys.timezone(), upload.time = "17:00", warning.msg = TRUE) {

  if (is.null(date)) {
    local.time <- Sys.time()
    local.date <- as.Date(format(local.time, "%Y-%m-%d"))
    warning.msg <- FALSE

  } else {
    local.date <- as.Date(date, optional = TRUE)
    if (is.na(local.date)) {
      stop('Invalid date or format "yyyy-mm-dd".', call. = FALSE)
    }
  }

  local.date <- fixDate_2012(local.date)

  if (repository == "CRAN") {
    first.log <- as.Date("2012-10-01") # first RStudio CRAN mirror log.
  } else if (repository == "MRAN") {
     first.log <- as.Date("2014-09-17") # first MRAN snapshot.
  } else stop('repository must be "CRAN" or "MRAN".', call. = FALSE)

  if (local.date < first.log) {
    if (repository == "CRAN") {
      txt <- 'RStudio CRAN logs begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    } else if (repository == "MRAN") {
      txt <- 'MRAN snapshots begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    }
  }

  year <- as.POSIXlt(local.date)$year + 1900
  rstudio.url <- "http://cran-logs.rstudio.com/"
  log.url <- paste0(rstudio.url, year, '/', local.date, ".csv.gz")

  if (RCurl::url.exists(log.url)) {
    log.date <- local.date
  } else {
    log.date <- available_log(local.date, tz, upload.time, warning.msg)
  }
  log.date
}
