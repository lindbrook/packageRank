#' Compute Effective CRAN Log Date Based on Local and UTC Time (prototype).
#'
#' RStudio CRAN Mirror Logs for previous day are posted at 17:00:00 UTC.
#' @param date Character. Date of desired log \code{"yyyy-mm-dd"}.
#' @param check.url Logical.
#' @param repository Character. "CRAN" or "MRAN". RStudio CRAN mirror log or Microsoft MRAN snapshot.
#' @param upload.time Character. UTC upload time for logs "hh:mm" or "hh:mm:ss".
#' @param warning.msg Logical. TRUE uses warning() if the function returns the date of the previous available log.
#' @return An R date object.
#' @export

logDate <- function(date = NULL, check.url = TRUE,
  repository = "CRAN", upload.time = "17:00", warning.msg = TRUE) {

  if (is.null(date)) {
    local.time <- Sys.time()
    local.date <- as.Date(format(local.time, "%Y-%m-%d"))
  } else {
    local.date <- as.Date(date, optional = TRUE)
    if (is.na(local.date)) {
      stop('Invalid date or format "yyyy-mm-dd".', call. = FALSE)
    }
  }

  if (repository == "CRAN") {
    first.log <- as.Date("2012-10-01") # first RStudio CRAN mirror log.
  } else if (repository == "MRAN") {
     first.log <- as.Date("2014-09-17") # first MRAN snapshot.
  } else stop('repository must be "CRAN" or "MRAN".', call. = FALSE)

  if (local.date < first.log) {
    if (repository == "CRAN") {
      txt <- 'RStudio CRAN logs begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    } else if (repository == "MRAN") {
      txt <- 'MRAN snapshots begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    }
  }

  if (check.url) {
    year <- as.POSIXlt(local.date)$year + 1900
    rstudio.url <- "http://cran-logs.rstudio.com/"
    log.url <- paste0(rstudio.url, year, '/', local.date, ".csv.gz")
    if (RCurl::url.exists(log.url)) {
      out <- local.date
    } else out <- available_date(local.date, upload.time, warning.msg)
  } else out <- available_date(local.date, upload.time, warning.msg)
  out
}

available_date <- function(local.date, upload.time, warning.msg) {
  candidate.days <- seq.Date(local.date - 1, local.date + 1, by = "days")
  uploads <- lapply(candidate.days, function(x) dateTime(x, upload.time))
  local.time <- Sys.time()
  local.utc <- as.POSIXlt(local.time, tz = "GMT")
  deltas <- lapply(uploads, function(x) difftime(local.utc, x))
  pos <- vapply(deltas, function(x) x >= 0, logical(1L))

  if (all(pos)) {
    out <- date
  } else if (all(!pos)) {
    stop("Date in future!", call. = FALSE)
  } else {
    out <- candidate.days[pos][sum(pos)]
    next.date <- candidate.days[!pos][1]

    if (identical(date, next.date)) {
      Time <- -unlist(deltas[!pos][1])
      Unit <- attributes(deltas[!pos][1][[1]])$units

      if (Unit %in% c("secs", "mins", "days", "weeks")) {
        Time <- round(Time)
      } else if (Unit == "hours") {
        Time <- round(Time, 1)
      }

      Unit <- ifelse(Time == 1, substr(Unit, 1, nchar(Unit) - 1), Unit)

      utc.upload <- as.numeric(uploads[!pos][1][[1]])
      local.upload <- as.POSIXlt(utc.upload, origin = "1970-01-01",
        tz = Sys.timezone())
      local.upload <- format(local.upload, format = "%d %b %H:%M %Z")

      if (warning.msg) {
        msg <- paste0(next.date, " log should be available in ",
          paste(Time, Unit), " at ", local.upload, ". Using previous!")
        warning(msg, call. = FALSE)
      }
    }
  }
  out
}

#' Compute Effective CRAN Log Date Based on Local and UTC Time (manual prototype).
#'
#' RStudio CRAN Mirror Logs for previous day are posted at 17:00:00 UTC.
#' @param date Character. Local date "yyyy-mm-dd".
#' @param time Character. Local time ime "hh:mm" or "hh:mm:dd".
#' @param tz Character. Local time zone.
#' @param check.url Logical.
#' @param warning.msg Logical. TRUE uses warning() if the function returns the date of the previous available log.
#' @param upload.time Character. UTC upload time for logs "hh:mm" or "hh:mm:ss".
#' @return An R date object.
#' @noRd 

# logDate0 <- function(date = Sys.Date() + 1, time = "00:21",
#   tz = "Australia/Sydney", check.url = FALSE, warning.msg = TRUE,
#   upload.time = "17:00") {
#
#   date <- as.Date(date, optional = TRUE)
#   if (is.na(date)) stop('Invalid date or format "yyyy-mm-dd".', call. = FALSE)
#   local.time <- dateTime(date, time, tz = tz)
#   local.date <- as.Date(format(local.time, "%Y-%m-%d"))
#   local.utc <- as.POSIXlt(as.numeric(local.time), origin = "1970-01-01",
#     tz = "GMT")
#
#   east.of.dateline <- as.POSIXlt(Sys.time(), tz = "Etc/GMT+12")
#   intl.dateline.date <- as.Date(format(east.of.dateline, "%Y-%m-%d"))
#   upload.utc <- dateTime(intl.dateline.date, upload.time)
#   delta <- difftime(local.utc, upload.utc, units = "days")
#
#   if (delta > 1) {
#     stop("Date in future!", call. = FALSE)
#   } else {
#     if (check.url) {
#       year <- as.POSIXlt(date)$year + 1900
#       rstudio.url <- "http://cran-logs.rstudio.com/"
#       log.url <- paste0(rstudio.url, year, '/', date, ".csv.gz")
#       if (RCurl::url.exists(log.url)) out <- date
#       else {
#         out <- available_date(local.time, local.date, local.utc, tz,
#           upload.time, warning.msg)
#       }
#     } else {
#       out <- available_date(local.time, local.date, local.utc, tz,
#         upload.time, warning.msg)
#     }
#   }
#   out
# }
