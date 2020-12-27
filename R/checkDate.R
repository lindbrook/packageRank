#' Check and validate "yyyy-mm-dd" date.
#'
#' @param date Character. \code{"yyyy-mm-dd"}.
#' @param repository Character. "CRAN" or "MRAN".
#' @export

checkDate <- function(date, repository = "CRAN") {
  date <- as.Date(date, optional = TRUE)
  if (is.na(date)) stop('Not a valid date or format "yyyy-mm-dd".',
    call. = FALSE)

  if (repository == "CRAN") {
    first.log <- as.Date("2012-10-01") # first log on RStudio CRAN mirror
  } else if (repository == "MRAN") {
     first.log <- as.Date("2014-09-17") # MRAN timemachine
  } else stop('repository must be "CRAN" or "MRAN".', call. = FALSE)

  if (date < first.log) {
    if (repository == "CRAN") {
      txt <- 'RStudio CRAN logs begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    } else if (repository == "MRAN") {
      txt <- 'MRAN snapshots begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    }
  } else if (date - Sys.Date() > 1) {
    stop("Date in future!", call. = FALSE)
  } else if (date - Sys.Date() == 0 | date - Sys.Date() == 1) {
    local.time <- Sys.time()
    utc.time <- as.POSIXlt(as.numeric(local.time),
      origin = "1970-01-01", tz = "GMT")
    utc.date <- as.Date(strftime(utc.time, "%Y-%m-%d"))
    upload.time <- dateTime(utc.date, "17:00:00")

    if (utc.time < upload.time) {
      delta <- upload.time - local.time
      Time <- round(unclass(delta)[1])
      Unit <- attributes(delta)$units
      Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)
      msg <- paste0("Log for ", date, " should be available in ",
        paste(Time, Unit), " at 17:00:00 UTC.")
      stop(msg, call. = FALSE)
    } else date
  } else date
}
