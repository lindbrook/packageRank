#' Extract Time Components from difftime() object.
#'
#' @param x Object. difftime() object.
#' @noRd

timeUnit <- function(x) {
  Time <- ifelse(as.numeric(x) >= 0, as.numeric(x), -as.numeric(x))
  Unit <- attributes(x)$units
  if (Unit %in% c("secs", "mins")) {
    Time <- round(Time)
  } else if (Unit == "hours") {
    Time <- round(Time, 1)
  } else stop("Error!")
  Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)
  data.frame(Time = Time, Unit = Unit)
}
