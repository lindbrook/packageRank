#' Compute Effective CRAN Log Date Based on Local and UTC Time (prototype).
#'
#' RStudio CRAN Mirror Logs for previous day are posted at 17:00:00 UTC.
#' @param upload.time Character. UTC upload time "hh:mm" or "hh:mm:dd".
#' @return An R date object.
#' @export

logDate <- function(upload.time = "17:00:00") {
  local.time <- Sys.time()
  local.date <- as.Date(format(local.time, "%Y-%m-%d"))
  local.utc <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
    tz = "GMT")

  # Info for yesterday's log
  east.of.dateline <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
    tz = "Etc/GMT+12")
  intl.dateline.date <- as.Date(format(east.of.dateline, "%Y-%m-%d"))
  upload.utc <- dateTime(intl.dateline.date, upload.time)
  local.upload <- as.POSIXlt(as.numeric(upload.utc), origin = "1970-01-01",
    tz = Sys.timezone())
  local.upload <- format(local.upload, format = "%H:%M:%S %Z")

  available.date <- intl.dateline.date - 1
  delta <- difftime(local.utc, upload.utc)

  date.test <- identical(local.date, intl.dateline.date) |
               local.date - intl.dateline.date == 1

  if (date.test) {
    if (delta < 0) {
      Time <- -round(unclass(delta)[1])
      Unit <- attributes(delta)$units
      Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)
      msg <- paste0("Log for ", available.date, " should be available in ",
        paste(Time, Unit), " at ", local.upload, ".")
      stop(msg, call. = FALSE)
    } else available.date
  }
}

#' Compute Effective CRAN Log Date Based on Local and UTC Time (manual prototype).
#'
#' RStudio CRAN Mirror Logs for previous day are posted at 17:00:00 UTC.
#' @param date Character. Local date "yyyy-mm-dd".
#' @param tz.time Character. Local time ime "hh:mm" or "hh:mm:dd".
#' @param tz Character. Local time zone.
#' @param upload.time Character. UTC upload time "hh:mm" or "hh:mm:dd".
#' @return An R date object.
#' @export

logDate0 <- function(date = Sys.Date() + 1, tz.time = "16:05:00",
   tz = "Australia/Sydney", upload.time = "17:00:00") {

  local.time <- dateTime(date, tz.time, tz = tz)
  local.date <- as.Date(format(local.time, "%Y-%m-%d"))
  local.utc <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
    tz = "GMT")

  # Info for yesterday's log
  east.of.dateline <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
    tz = "Etc/GMT+12")
  intl.dateline.date <- as.Date(format(east.of.dateline, "%Y-%m-%d"))
  upload.utc <- dateTime(intl.dateline.date, upload.time)
  local.upload <- as.POSIXlt(as.numeric(upload.utc), origin = "1970-01-01",
    tz = Sys.timezone())
  local.upload <- format(local.upload, format = "%H:%M:%S %Z")

  available.date <- intl.dateline.date - 1
  delta <- difftime(local.utc, upload.utc)

  date.test <- identical(local.date, intl.dateline.date) |
               local.date - intl.dateline.date == 1

  if (date.test) {
    if (delta < 0) {
      Time <- -round(unclass(delta)[1])
      Unit <- attributes(delta)$units
      Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)
      msg <- paste0("Log for ", available.date, " should be available in ",
        paste(Time, Unit), " at ", local.upload, ".")
      stop(msg, call. = FALSE)
    } else available.date
  }
}
