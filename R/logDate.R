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
    local.time <- utc()
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
    if (is.null(date)) local.date <- local.date - 1
    year <- as.POSIXlt(local.date)$year + 1900
    rstudio.url <- "http://cran-logs.rstudio.com/"
    log.url <- paste0(rstudio.url, year, '/', local.date, ".csv.gz")
    if (RCurl::url.exists(log.url)) {
      log.date <- local.date
    } else {
      if (is.null(date)) local.date <- local.date + 1
      log.date <- available_log(local.date, tz, upload.time, warning.msg)
    }
  } else if (repository == "MRAN") {
    # MRAN fixed snapshot time?
    if (local.date <= Sys.Date()) log.date <- local.date
    else stop("Date in future. Snapshot not yet available.", call. = FALSE)
  }

  log.date
}

available_log <- function(local.date, tz, upload.time, warning.msg) {
  nominal.date <- local.date
  effective.utc <- dateTime(nominal.date + 1, upload.time)

  current.date_time <- Sys.time()
  current.utc <- as.POSIXlt(as.numeric(current.date_time),
    origin = "1970-01-01", tz = "GMT")

  delta.days <- difftime(current.utc, effective.utc, units = "days")

  if (delta.days >= 0) {
    log.date <- nominal.date

  } else if (delta.days <= -2) {
    stop("Date in future. Log not yet available.", call. = FALSE)

  } else if (delta.days > -2 & delta.days <= -1) {
    delta.time <- difftime(current.utc, effective.utc)
    effective.date <- as.Date(format(effective.utc, "%Y-%m-%d"))

    effective.utc.b <- dateTime(nominal.date, upload.time)
    delta.time.b <- difftime(current.utc, effective.utc.b)

    if (delta.time.b < 0) {
      effective.date <- as.Date(format(effective.utc.b, "%Y-%m-%d")) - 1
    }

   if (delta.time < 0) {
     if (identical(nominal.date, effective.date)) {
       log.date <- effective.date
     } else {
       t.minus <- timeUnit(delta.time)
       next.upload <- as.POSIXlt(effective.utc, tz = tz)
       next.upload <- format(next.upload, "%d %b %H:%M %Z")
       if (warning.msg) {
        warning(timeMsg(nominal.date, t.minus, next.upload), call. = FALSE)
       }
       log.date <- effective.date - 1
     }
   }

  } else {
    delta.time <- difftime(current.utc, effective.utc)
    effective.date <- as.Date(format(effective.utc, "%Y-%m-%d"))

    if (delta.time >= 0) {
      if (identical(nominal.date, effective.date)) {
        log.date <- effective.date
      } else {
        next.date <- effective.date + 1
        next.utc <- dateTime(next.date, upload.time)
        t.minus <- timeUnit(difftime(current.utc, next.utc))
        next.upload <- as.POSIXlt(next.utc, tz = tz)
        next.upload <- format(next.upload, "%d %b %H:%M %Z")
        if (warning.msg) {
          warning(timeMsg(nominal.date, t.minus, next.upload), call. = FALSE)
        }
        log.date <- effective.date - 1
      }
    } else {
      if (identical(nominal.date, effective.date)) {
        log.date <- effective.date
      } else {
        t.minus <- timeUnit(delta.time)
        next.upload <- as.POSIXlt(effective.utc, tz = tz)
        next.upload <- format(next.upload, "%d %b %H:%M %Z")
        if (warning.msg) {
          warning(timeMsg(nominal.date, t.minus, next.upload), call. = FALSE)
        }
      }
      log.date <- effective.date - 2
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

timeMsg <- function(nominal.date, t.minus, next.upload) {
  paste0(nominal.date, " log arrives in ~",
    paste(t.minus$Time, t.minus$Unit), " at ", next.upload,
    ". Using previous!")
}
