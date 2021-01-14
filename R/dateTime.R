#' Nominal time to clock time
#'
#' Time via POSIXlt date.
#' @param date Character. "yyyy-mm-dd".
#' @param time Character. "hh:mmm" or "hh:mmm:ss".
#' @param tz Character. Time zone.
#' @export

dateTime <- function(date = "2021-01-01", time = "09:00", tz = "GMT") {
  as.POSIXct(paste(date, time), tz = tz)
}
