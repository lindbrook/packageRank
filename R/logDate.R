#' Compute Effective CRAN Log Date Based on Local and UTC Time (prototype).
#'
#' RStudio CRAN Mirror Logs for previous day are posted at 17:00:00 UTC.
#' @param date Character. Date of desired log \code{"yyyy-mm-dd"}. NULL returns date of latest available log.
#' @param check.url Logical.
#' @param repository Character. "CRAN" or "MRAN". RStudio CRAN mirror log or Microsoft MRAN snapshot.
#' @param upload.time Character. UTC upload time for logs "hh:mm" or "hh:mm:ss".
#' @param warning.msg Logical. TRUE uses warning() if the function returns the date of the previous available log.
#' @param tz Character. Time zone. See OlsonNames().
#' @return An R date object.
#' @export

logDate <- function(date = NULL, check.url = TRUE,
  repository = "CRAN", upload.time = "17:00", warning.msg = TRUE,
  tz = Sys.timezone()) {

  if (is.null(date)) {
    local.time <- Sys.time()
    local.date <- as.Date(format(local.time, "%Y-%m-%d"))
    warning.msg <- FALSE
  } else {
    local.date <- as.Date(date, optional = TRUE)
    if (is.na(local.date)) {
      stop('Invalid date or format "yyyy-mm-dd".', call. = FALSE)
    }
  }

  if (check.url) {
    year <- as.POSIXlt(local.date)$year + 1900
    rstudio.url <- "http://cran-logs.rstudio.com/"
    log.url <- paste0(rstudio.url, year, '/', local.date, ".csv.gz")
    if (RCurl::url.exists(log.url)) {
      log.date <- local.date
    } else log.date <- available_log(local.date, upload.time, warning.msg, tz)
  } else log.date <- available_log(local.date, upload.time, warning.msg, tz)

  if (repository == "CRAN") {
    first.log <- as.Date("2012-10-01") # first RStudio CRAN mirror log.
  } else if (repository == "MRAN") {
     first.log <- as.Date("2014-09-17") # first MRAN snapshot.
  } else stop('repository must be "CRAN" or "MRAN".', call. = FALSE)

  if (log.date < first.log) {
    if (repository == "CRAN") {
      txt <- 'RStudio CRAN logs begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    } else if (repository == "MRAN") {
      txt <- 'MRAN snapshots begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    }
  } else log.date
}

available_log <- function(local.date, tz = Sys.timezone(),
  upload.time = "17:00", warning.msg = TRUE) {

  clock.time <- format(Sys.time(), "%H:%M:%S")
  nominal.time <- dateTime(local.date, clock.time, tz = tz)
  nominal.utc <- as.POSIXlt(as.numeric(nominal.time), origin = "1970-01-01",
    tz = "GMT")

  now.utc <- as.Date(format(utc(), format = "%Y-%m-%d"))
  upload.utc <- dateTime(now.utc, upload.time)

  delta.days <- difftime(upload.utc, nominal.utc, units = "days")
  delta <- difftime(upload.utc, nominal.utc)

  if (delta.days < -1) {
    stop("Date in future!", call. = FALSE)
  } else if (delta.days >= 1) {
    local.date - 1
  } else {
    now.utc <- as.Date(format(nominal.utc, format = "%Y-%m-%d"))
    upload.utc <- dateTime(now.utc, upload.time)
    delta2 <- difftime(nominal.utc, upload.utc)

    if (delta2 >= 0) {
      now.utc - 1
    } else if (delta2 < 0) {
      Time <- -as.numeric(delta2)
      Unit <- attributes(delta2)$units

      if (Unit %in% c("secs", "mins")) {
        Time <- round(Time)
      } else if (Unit == "hours") {
        Time <- round(Time, 1)
      }

      Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)
      local.upload <- as.POSIXlt(upload.utc, origin = "1970-01-01", tz = tz)
      local.upload <- format(local.upload, format = "%d %b %H:%M %Z")

      if (warning.msg) {
        msg <- paste0(now.utc, " log should be available in ",
          paste(Time, Unit), " at ", local.upload, ". Using previous!")
        warning(msg, call. = FALSE)
      }
      now.utc - 1
    }
  }
}
