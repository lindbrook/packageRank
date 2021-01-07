#' Compute Coordinated Universal Time (UTC/GMT) for your local time.
#'
#' @param local.time Logical. Use current local time.
#' @param date Character. Date "yyyy-mm-dd".
#' @param time Character. Local time ime "hh:mm" or "hh:mm:ss".
#' @param tz Character. Local time zone. See OlsonNames().
#' @export

utc <- function(local.time = TRUE, date = "2020-01-01", time = "12:00:00",
  tz = "Europe/Vienna") {
  if (local.time) {
    x <- Sys.time()
  } else {
    x <- dateTime(date = date, time = time, tz = tz)
  }
  as.POSIXlt(as.numeric(x), origin = "1970-01-01", tz = "GMT")
}

#' Compute Current Local Time in Selected Time Zone.
#'
#' @param tz Character. Local time zone. See OlsonNames().
#' @export

currentTime <- function(tz = "Australia/Sydney") {
  x <- Sys.time()
  as.POSIXlt(as.numeric(x), origin = "1970-01-01", tz = tz)
}
