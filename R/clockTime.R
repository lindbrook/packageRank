#' Nominal time to clock time
#'
#' Time via POSIXlt date.
#' @param time Character. "hh:mmm:ss".
#' @param date Character. "yyyy-mm-dd".
#' @param tz Character. Time zone.
#' @export

clockTime <- function(time = "09:00:00", date = "2020-08-14",
  tz = "Europe/Vienna") {
 as.POSIXlt(paste(date, time), tz = tz)
}
