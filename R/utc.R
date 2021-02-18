#' Compute Coordinated Universal Time (UTC/GMT) for Your Local Time.
#'
#' @export

utc <- function() {
  as.POSIXlt(as.numeric(Sys.time()), origin = "1970-01-01", tz = "GMT")
}

#' Compute Coordinated Universal Time (UTC/GMT) for Specified Local Time.
#'
#' @param date Character. Date "yyyy-mm-dd".
#' @param time Character. Local time "hh:mm" or "hh:mm:ss".
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @export

utc0 <- function(date = "2020-01-01", time = "12:00:00", tz = "Europe/Vienna") {
  local.date <- as.Date(date, optional = TRUE)
  if (is.na(local.date)) {
    stop('Invalid date or format "yyyy-mm-dd".', call. = FALSE)
  }
  x <- dateTime(date = local.date, time = time, tz = tz)
  as.POSIXlt(as.numeric(x), origin = "1970-01-01", tz = "GMT")
}

#' Compute Current Time in Selected Time Zone.
#'
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @export

currentTime <- function(tz = "Australia/Sydney") {
  as.POSIXct(as.numeric(Sys.time()), origin = "1970-01-01", tz = tz)
}

#' Compute Local Time from Coordinated Universal Time (UTC/GMT).
#'
#' @param date Character. Date "yyyy-mm-dd".
#' @param time Character. Local time "hh:mm" or "hh:mm:ss".
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @export

localTime <- function(date = "2021-1-1", time = "12:00", tz = Sys.timezone()) {
  local.date <- as.Date(date, optional = TRUE)
  if (is.na(local.date)) {
    stop('Invalid date or format "yyyy-mm-dd".', call. = FALSE)
  }
  x <- dateTime(local.date, time)
  as.POSIXlt(as.numeric(x), origin = "1970-01-01", tz = tz)
}

#' Compute Date and Time of Latest Available Log.
#'
#' GMT and Local Posting Times.
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @export

logPostInfo <- function(tz = Sys.timezone()) {
  x <- dateTime(Sys.Date(), time = "17:00", tz = "GMT")
  local <- as.POSIXlt(as.numeric(x), origin = "1970-01-01", tz = tz)
  list(log.date = as.Date(format(x, "%Y-%m-%d")) - 1, GMT = x, local = local)
}
