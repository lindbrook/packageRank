#' Check and validate "yyyy-mm-dd" date.
#'
#' @param date Character. Date of desired log \code{"yyyy-mm-dd"}.
#' @param repository Character. "CRAN" or "MRAN".
#' @param upload.time Character. UTC upload time for logs "hh:mm" or "hh:mm:dd".
#' @param tz Character. Local time zone. See OlsonNames().
#' @export

checkDate <- function(date, repository = "CRAN", upload.time = "17:00",
  tz = Sys.timezone()) {

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

  available.date <- logDate()
  local.datetime <- dateTime(date, format(Sys.time(), "%H:%M:%S"), tz = tz)
  local.utc <- as.POSIXlt(as.numeric(local.datetime),
    origin = "1970-01-01", tz = "GMT")
  local.date <- as.Date(format(local.utc, "%Y-%m-%d"))

  yesterday.utc <- dateTime(Sys.Date() - 1, upload.time)
  today.utc <- dateTime(Sys.Date(), upload.time)
  tomorrow.utc <- dateTime(Sys.Date() + 1, upload.time)
  day_after.utc <- dateTime(Sys.Date() + 2, upload.time)

  if (local.utc < yesterday.utc) {
    date

  } else if (local.utc >= yesterday.utc & local.utc < today.utc) {
    if (local.date > available.date) {
      delta <- -difftime(local.utc, today.utc)

      Time <- unclass(delta)[1]
      Unit <- attributes(delta)$units

      if (Unit %in% c("secs", "mins")) {
        Time <- round(Time)
      } else if (Unit == "hours") {
        Time <- round(Time, 1)
      }

      Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)

      local.upload <- as.POSIXlt(as.numeric(yesterday.utc),
        origin = "1970-01-01", tz = tz)
      local.upload <- format(local.upload, format = "%H:%M %Z")

      msg <- paste0(date, " log should be available in ", paste(Time, Unit),
        " at ", local.upload, ". Using previous log.")
      warning(msg, call. = FALSE)
      available.date
    } else if (local.date <= available.date) {
      available.date
    }

  } else if (local.utc >= today.utc & local.utc < tomorrow.utc) {
    delta <- -difftime(local.utc, tomorrow.utc)

    Time <- unclass(delta)[1]
    Unit <- attributes(delta)$units

    if (Unit %in% c("secs", "mins")) {
      Time <- round(Time)
    } else if (Unit == "hours") {
      Time <- round(Time, 1)
    }

    Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)

    local.upload <- as.POSIXlt(as.numeric(yesterday.utc),
      origin = "1970-01-01", tz = tz)
    local.upload <- format(local.upload, format = "%H:%M %Z")

    msg <- paste0(date, " log should be available in ", paste(Time, Unit),
      " at ", local.upload, ". Using previous log.")
    warning(msg, call. = FALSE)
    available.date

  } else if (local.utc >= tomorrow.utc & local.utc < day_after.utc) {
    delta <- difftime(local.utc, day_after.utc)

    Time <- unclass(delta)[1]
    Unit <- attributes(delta)$units

    if (Unit %in% c("secs", "mins", "days")) {
      Time <- round(Time)
    } else if (Unit == "hours") {
      Time <- round(Time, 1)
    }

    Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)

    local.upload <- as.POSIXlt(as.numeric(tomorrow.utc), origin = "1970-01-01",
      tz = tz)
    local.upload <- format(local.upload, format = "%d %b %H:%M %Z")

    msg <- paste0("Date in future! ", date, " log should be available ",
      local.upload, ".")
    stop(msg, call. = FALSE)

  } else if (local.utc >= day_after.utc) {
     msg <- paste0("Date in future! ")
     stop(msg, call. = FALSE)
  }
}
