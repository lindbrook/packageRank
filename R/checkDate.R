#' Check and validate "yyyy-mm-dd" date.
#'
#' @param date Character. \code{"yyyy-mm-dd"}.
#' @param repository Character. "CRAN" or "MRAN".
#' @param upload.time Character. UTC upload time for logs "hh:mm" or "hh:mm:dd".
#' @export

checkDate <- function(date, repository = "CRAN", upload.time = "17:00:00") {
  date <- as.Date(date, optional = TRUE)

  if (is.na(date)) {
    stop('Not a valid date or format "yyyy-mm-dd".', call. = FALSE)
  }

  if (repository == "CRAN") {
    first.log <- as.Date("2012-10-01") # first RStudio CRAN mirror log.
  } else if (repository == "MRAN") {
     first.log <- as.Date("2014-09-17") # first MRAN snapshot.
  } else stop('repository must be "CRAN" or "MRAN".', call. = FALSE)

  if (date < first.log) {
    if (repository == "CRAN") {
      txt <- 'RStudio CRAN logs begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    } else if (repository == "MRAN") {
      txt <- 'MRAN snapshots begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    }
  }

  delta.days <- difftime(date, logDate())

  if (delta.days > 0) {
    local.time <- Sys.time()
    nominal.time <- dateTime(date, format(local.time, "%H:%M:%S"),
      tz = Sys.timezone())
    nominal.utc <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
      tz = "GMT")

    # Info for "yesterday"'s log
    east.of.dateline <- as.POSIXlt(as.numeric(nominal.time),
      origin = "1970-01-01", tz = "Etc/GMT+12")
    intl.dateline.date <- as.Date(format(east.of.dateline, "%Y-%m-%d"))
    upload.utc <- dateTime(intl.dateline.date, upload.time)

    local.upload <- as.POSIXlt(as.numeric(upload.utc), origin = "1970-01-01",
      tz = Sys.timezone())
    local.upload <- format(local.upload, format = "%Y-%m-%d %H:%M:%S %Z")

    effective.date <- intl.dateline.date - 1
    delta <- difftime(nominal.utc, upload.utc)

    if (delta < 0) {
      Time <- -round(unclass(delta)[1])
      Unit <- attributes(delta)$units
      if (Unit == "hours") {
        Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)
        msg <- paste0("Log for ", effective.date, " in available in ",
          paste(Time, Unit), " at ", local.upload, ".")
      } else {
        Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)
        msg <- paste0(effective.date, " in future! Log available in ",
          paste(Time, Unit), ".")
      }
      stop(msg, call. = FALSE)
    } else effective.date
  } else date
}
