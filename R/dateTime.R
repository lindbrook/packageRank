#' Convert Nominal Date and Time to Date-Time Object.
#'
#' Character(s) to POSIXlt date.
#' @param date Character. "yyyy-mm-dd".
#' @param time Character. "hh:mm" or "hh:mm:ss".
#' @param tz Character. Time zone.
#' @noRd

dateTime <- function(date = "2021-01-01", time = "09:00", tz = "UTC") {
  fasttime::fastPOSIXct(paste(date, time), tz = tz)
}
