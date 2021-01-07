#' Compute Effective CRAN Log Date Based on Local and UTC Time (prototype).
#'
#' RStudio CRAN Mirror Logs for previous day are posted at 17:00:00 UTC.
#' @param upload.time Character. UTC upload time for logs "hh:mm" or "hh:mm:ss".
#' @param warning.msg Logical. TRUE uses warning() if the function returns the date of the previous available log.
#' @return An R date object.
#' @export

logDate <- function(upload.time = "17:00", warning.msg = TRUE) {
  local.time <- Sys.time()
  local.date <- as.Date(format(local.time, "%Y-%m-%d"))
  local.utc <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
    tz = "GMT")

  # Info for available log (earliest date time)
  east.of.dateline <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
    tz = "Etc/GMT+12")
  intl.dateline.date <- as.Date(format(east.of.dateline, "%Y-%m-%d"))
  upload.utc <- dateTime(intl.dateline.date, upload.time)
  local.upload <- as.POSIXlt(as.numeric(upload.utc), origin = "1970-01-01",
    tz = Sys.timezone())
  local.upload <- format(local.upload, format = "%d %b %H:%M %Z")

  possible.dates <- c(intl.dateline.date - 1,
                      intl.dateline.date,
                      intl.dateline.date + 1)

  upload.yesterday <- dateTime(intl.dateline.date - 1, upload.time)
  upload.today <- dateTime(intl.dateline.date, upload.time)
  upload.tomorrow <- dateTime(intl.dateline.date + 1, upload.time)

  delta.yesterday <- difftime(local.utc, upload.yesterday)
  delta.today <- difftime(local.utc, upload.today)
  delta.tomorrow <- difftime(local.utc, upload.tomorrow)

  delta.times <- c(delta.yesterday, delta.today, delta.tomorrow)
  available.date <- possible.dates[delta.times >= 0]
  available.date <- available.date[length(available.date)] - 1

  delta.nms <-  c("delta.yesterday", "delta.today", "delta.tomorrow")
  next.log <- delta.nms[delta.times < 0][1]
  next.date <- possible.dates[delta.times < 0][1] - 1
  next.delta <- get(next.log)

  Time <- -unclass(next.delta)[1]
  Unit <- attributes(next.delta)$units

  if (Unit %in% c("secs", "mins", "days")) {
    Time <- round(Time)
  } else if (Unit == "hours") {
    Time <- round(Time, 1)
  }

  Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)

  if (delta.times[2] < 0) {
    if (warning.msg) {
      msg <- paste0(next.date, " log should be available in ",
        paste(Time, Unit), " at ", local.upload, ". Using previous!")
      warning(msg, call. = FALSE)
    }
  }

  available.date
}

#' Compute Effective CRAN Log Date Based on Local and UTC Time (manual prototype).
#'
#' RStudio CRAN Mirror Logs for previous day are posted at 17:00:00 UTC.
#' @param date Character. Local date "yyyy-mm-dd".
#' @param tz.time Character. Local time ime "hh:mm" or "hh:mm:dd".
#' @param tz Character. Local time zone.
#' @param upload.time Character. UTC upload time "hh:mm" or "hh:mm:dd".
#' @param warning.msg Logical. TRUE uses warning() if the fuction returns the previous available date.
#' @return An R date object.
#' @export

logDate0 <- function(date = Sys.Date() + 1, tz.time = "16:05",
   tz = "Australia/Sydney", upload.time = "17:00", warning.msg = TRUE) {

  local.time <- dateTime(date, tz.time, tz = tz)
  local.date <- as.Date(format(local.time, "%Y-%m-%d"))
  local.utc <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
    tz = "GMT")

  # Info for available log
  east.of.dateline <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
    tz = "Etc/GMT+12")
  intl.dateline.date <- as.Date(format(east.of.dateline, "%Y-%m-%d"))
  upload.utc <- dateTime(intl.dateline.date, upload.time)
  local.upload <- as.POSIXlt(as.numeric(upload.utc), origin = "1970-01-01",
    tz = tz)
  local.upload <- format(local.upload, format = "%d %b %H:%M %Z")

  possible.dates <- c(intl.dateline.date - 1,
                      intl.dateline.date,
                      intl.dateline.date + 1)

  upload.yesterday <- dateTime(intl.dateline.date - 1, upload.time)
  upload.today <- dateTime(intl.dateline.date, upload.time)
  upload.tomorrow <- dateTime(intl.dateline.date + 1, upload.time)

  delta.yesterday <- difftime(local.utc, upload.yesterday)
  delta.today <- difftime(local.utc, upload.today)
  delta.tomorrow <- difftime(local.utc, upload.tomorrow)

  delta.times <- c(delta.yesterday, delta.today, delta.tomorrow)
  available.date <- possible.dates[delta.times >= 0]
  available.date <- available.date[length(available.date)] - 1

  delta.nms <-  c("delta.yesterday", "delta.today", "delta.tomorrow")
  next.log <- delta.nms[delta.times < 0][1]
  next.date <- possible.dates[delta.times < 0][1] - 1
  next.delta <- get(next.log)

  Time <- -unclass(next.delta)[1]
  Unit <- attributes(next.delta)$units

  if (Unit %in% c("secs", "mins", "days")) {
    Time <- round(Time)
  } else if (Unit == "hours") {
    Time <- round(Time, 1)
  }

  Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)

  if (delta.times[2] < 0) {
    if (warning.msg) {
      msg <- paste0(next.date, " log should be available in ",
        paste(Time, Unit), " at ", local.upload, ". Using previous!")
      warning(msg, call. = FALSE)
    }
  }

  available.date
}
