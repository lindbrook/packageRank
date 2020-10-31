#' Nominal time to clock time
#'
#' Time via POSIXlt date.
#' @param date Character. "yyyy-mm-dd".
#' @param time Character. "hh:mmm:ss".
#' @param tz Character. Time zone.
#' @export

dateTime <- function(date = "2020-08-14", time = "09:00:00", 
  tz = "Europe/Vienna") {
 as.POSIXlt(paste(date, time), tz = tz)
}
