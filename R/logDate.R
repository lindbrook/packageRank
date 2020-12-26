#' Compute Effective CRAN Log Date Based on Local and UTC Time (prototype).
#'
#' RStudio CRAN Mirror Logs for previous day are posted at 17:00:00 UTC.
#' @return An R date object.
#' @export

logDate <- function() {
  local.time <- Sys.time()
  utc.time <- as.POSIXlt(as.numeric(local.time),
    origin = "1970-01-01", tz = "GMT")
  utc.date <- as.Date(strftime(utc.time, "%Y-%m-%d"))
  upload.time <- dateTime(utc.date, "17:00:00")
  effective.date <- utc.date - 1

  if (utc.time < upload.time) {
    delta <- upload.time - local.time
    Time <- round(unclass(delta)[1])
    Unit <- attributes(delta)$units
    Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)
    msg <- paste0("Log for ", effective.date, " should be available in ",
      paste(Time, Unit), " at 17:00:00 UTC.")
    stop(msg, call. = FALSE)
  } else effective.date
}
