#' Compute Coordinated Universal Time (UTC/GMT) for Your Local Time.
#'
#' @export

utc <- function() as.POSIXct(Sys.time(), tz = "UTC")

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
  } else as.POSIXct(paste(local.date, time), tz = tz)
}

#' Compute Current Time in Selected Time Zone.
#'
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @export

currentTime <- function(tz = "Australia/Sydney") as.POSIXct(Sys.time(), tz = tz)

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
  } else dateTime(local.date, time, tz = tz)
}
