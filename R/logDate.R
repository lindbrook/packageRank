#' Compute Effective CRAN Log Date Based on Local and UTC Time (prototype).
#'
#' RStudio CRAN Mirror Logs for previous day are posted at 17:00:00 UTC.
#' @param upload.time Character. UTC upload time for logs "hh:mm" or "hh:mm:ss".
#' @param warning.msg Logical. TRUE uses warning() if the fuction returns the previous available date.
#' @return An R date object.
#' @export

logDate <- function(upload.time = "17:00", warning.msg = TRUE) {
  local.time <- Sys.time()
  local.date <- as.Date(format(local.time, "%Y-%m-%d"))
  local.utc <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
    tz = "GMT")

  # Info for available log
  east.of.dateline <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
    tz = "Etc/GMT+12")
  intl.dateline.date <- as.Date(format(east.of.dateline, "%Y-%m-%d"))
  upload.utc <- dateTime(intl.dateline.date, upload.time)
  local.upload <- as.POSIXlt(as.numeric(upload.utc), origin = "1970-01-01",
    tz = Sys.timezone())
  local.upload <- format(local.upload, format = "%d %b %H:%M %Z")

  available.date <- intl.dateline.date - 1
  delta <- difftime(local.utc, upload.utc)

  if (delta < 0) {
    Time <- -round(unclass(delta)[1])
    Unit <- attributes(delta)$units

    if (Unit %in% c("secs", "mins")) {
      Time <- round(Time)
    } else if (Unit == "hours") {
      Time <- round(Time, 1)
    }

    Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)

    if (warning.msg) {
      msg <- paste0(available.date, " log should be available in ",
        paste(Time, Unit), " at ", local.upload, ". Using previous!")
      warning(msg, call. = FALSE)
      available.date - 1
    } else available.date - 1
  } else available.date
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

  available.date <- intl.dateline.date - 1
  delta <- difftime(local.utc, upload.utc)

  if (delta < 0) {
    Time <- -unclass(delta)[1]
    Unit <- attributes(delta)$units

    if (Unit %in% c("secs", "mins", "days")) {
      Time <- round(Time)
    } else if (Unit == "hours") {
      Time <- round(Time, 1)
    }

    Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)

    if (warning.msg) {
      msg <- paste0(available.date, " log should be available in ",
        paste(Time, Unit), " at ", local.upload, ". Using previous!")
      warning(msg, call. = FALSE)
      available.date - 1
    } else available.date - 1
  } else available.date
}
