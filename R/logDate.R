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

  if (repository == "CRAN") {
    first.log <- as.Date("2012-10-01") # first RStudio CRAN mirror log.
    local.date <- fixDate_2012(local.date)
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

  if (repository == "CRAN") {
    year <- as.POSIXlt(local.date)$year + 1900
    rstudio.url <- "http://cran-logs.rstudio.com/"
    log.url <- paste0(rstudio.url, year, '/', local.date, ".csv.gz")
    if (RCurl::url.exists(log.url)) log.date <- local.date
    else log.date <- available_log(local.date, tz, upload.time, warning.msg)
  } else if (repository == "MRAN") {
    log.date <- local.date
  }

  log.date
}

available_log <- function(local.date, tz, upload.time, warning.msg) {
  current.date_time <- Sys.time()
  current.time <- format(current.date_time, "%H:%M:%S")
  current.date <- as.Date(format(current.date_time, format = "%Y-%m-%d"))

  current.utc <- as.POSIXlt(as.numeric(current.date_time),
    origin = "1970-01-01", tz = "GMT")
  current.utc.date <- as.Date(format(current.utc, format = "%Y-%m-%d"))
  current.utc.upload <- dateTime(current.utc.date, upload.time)

  nominal.date_time <- dateTime(local.date, current.time, tz = tz)
  nominal.utc <- as.POSIXlt(as.numeric(nominal.date_time),
    origin = "1970-01-01", tz = "GMT")

  delta.days <- difftime(nominal.utc, current.utc.upload,  units = "days")

  if (delta.days > 1) {
    stop("Date in future!", call. = FALSE)
  } else {
    delta.time <- difftime(nominal.utc, current.utc.upload)

    if (delta.time < 0) {
      time.data <- timeUnit(delta.time)
      current.upload <- as.POSIXlt(current.utc.upload, origin = "1970-01-01",
        tz = tz)
      current.upload <- format(current.upload, format = "%d %b %H:%M %Z")
      if (warning.msg) {
        msg <- paste0(current.utc.date - 1, " log should be available in ",
          paste(time.data$Time, time.data$Unit), " at ", current.upload,
          ". Using previous!")
        warning(msg, call. = FALSE)
      }
      log.date <- current.utc.date - 2
    } else {
      next.utc.upload <- dateTime(current.utc.date + 1, upload.time)
      next.delta.time <- difftime(nominal.utc, next.utc.upload)
      time.data <- timeUnit(next.delta.time)
      next.upload <- as.POSIXlt(next.utc.upload, origin = "1970-01-01",
        tz = tz)
      next.upload <- format(next.upload, format = "%d %b %H:%M %Z")
      if (warning.msg) {
        msg <- paste0(current.utc.date, " log should be available in ",
          paste(time.data$Time, time.data$Unit), " at ", next.upload,
          ". Using previous!")
        warning(msg, call. = FALSE)
      }
      log.date <- current.utc.date - 1
    }
  }
  log.date
}

timeUnit <- function(x) {
  Time <- ifelse(as.numeric(x) >= 0, as.numeric(x), -as.numeric(x))
  Unit <- attributes(x)$units
  if (Unit %in% c("secs", "mins")) {
    Time <- round(Time)
  } else if (Unit %in% c("hours", "days")) {
    Time <- round(Time, 1)
  } else stop("Error!")
  Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)
  data.frame(Time = Time, Unit = Unit)
}
