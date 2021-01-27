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
  x <- dateTime(date = date, time = time, tz = tz)
  as.POSIXlt(as.numeric(x), origin = "1970-01-01", tz = "GMT")
}

#' Compute Current Local Time in Selected Time Zone.
#'
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @export

currentTime <- function(tz = "Australia/Sydney") {
  as.POSIXct(as.numeric(Sys.time()), origin = "1970-01-01", tz = tz)
}

#' Compute Date of Log, GMT and Local Posting Time.
#'
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @export

logPostInfo <- function(tz = Sys.timezone()) {
  x <- dateTime(Sys.Date(), time = "17:00", tz = "GMT")
  local <- as.POSIXlt(as.numeric(x), origin = "1970-01-01", tz = tz)
  list(log.date = as.Date(format(x, "%Y-%m-%d")) - 1, GMT = x, local = local)
}
