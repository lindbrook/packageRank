#' Compute Coordinated Universal Time (UTC/GMT) for Your Local Time.
#'
#' @export

utc <- function() {
  as.POSIXlt(as.numeric(Sys.time()), origin = "1970-01-01", tz = "GMT")
}

#' Compute Coordinated Universal Time (UTC/GMT) for Specified Local Time.
#'
#' @param date Character. Date "yyyy-mm-dd".
#' @param time Character. Local time "hh:mm" or "hh:mm:ss".
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @export

utc0 <- function(date = "2020-01-01", time = "12:00:00", tz = "Europe/Vienna") {
  local.date <- as.Date(date, optional = TRUE)
  if (is.na(local.date)) {
    stop('Invalid date or format "yyyy-mm-dd".', call. = FALSE)
  }
  x <- dateTime(date = local.date, time = time, tz = tz)
  as.POSIXlt(as.numeric(x), origin = "1970-01-01", tz = "GMT")
}

#' Compute Current Time in Selected Time Zone.
#'
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @export

currentTime <- function(tz = "Australia/Sydney") {
  as.POSIXct(as.numeric(Sys.time()), origin = "1970-01-01", tz = tz)
}

#' Compute Local Time from Coordinated Universal Time (UTC/GMT).
#'
#' @param date Character. Date "yyyy-mm-dd".
#' @param time Character. Local time "hh:mm" or "hh:mm:ss".
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @export

localTime <- function(date = "2021-1-1", time = "12:00", tz = Sys.timezone()) {
  local.date <- as.Date(date, optional = TRUE)
  if (is.na(local.date)) {
    stop('Invalid date or format "yyyy-mm-dd".', call. = FALSE)
  }
  x <- dateTime(local.date, time)
  as.POSIXlt(as.numeric(x), origin = "1970-01-01", tz = tz)
}

#' Compute Availability, Date, Time of "Today's" Log.
#'
#' Also checks availability of RStudio logs and 'cranlogs' data.
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @param upload.time Character. UTC upload time for logs "hh:mm" or "hh:mm:ss".
#' @export

logInfo <- function(tz = Sys.timezone(), upload.time = "17:00") {
  utc.data.time <- utc()
  utc.date <- as.Date(format(utc.data.time, "%Y-%m-%d"))

  today.utc <- dateTime(utc.date, time = upload.time, tz = "GMT")
  today.date <- as.Date(format(today.utc, "%Y-%m-%d"))
  today.delta.time <- difftime(today.utc, utc.data.time)
  today.t.minus <- timeUnit(today.delta.time)
  today.log <- utc.date - 1

  today.upload <- as.POSIXlt(today.utc, tz = tz)

  year <- format(today.log, "%Y")
  rstudio.url <- "http://cran-logs.rstudio.com/"
  log.url <- paste0(rstudio.url, year, '/', today.log, ".csv.gz")
  rstudio.test <- RCurl::url.exists(log.url)

  clogs <- cranlogs::cran_downloads(from = today.log, to = today.log)
  cranlogs.test <- ifelse(clogs$count != 0, TRUE, FALSE)

  if (today.delta.time > 0) {
    note <- paste0("Today's log is typically posted by ",
      format(as.POSIXlt(today.utc, tz = tz), "%H:%M %Z"), " (",
      format(today.utc, "%d %b %H:%M %Z"), ").")
  } else if (all(rstudio.test, cranlogs.test)) {
    note <- "Everything OK."
  } else if (rstudio.test & !cranlogs.test) {
    note <- paste0("'cranlogs' usually posts a bit after ",
      format(as.POSIXlt(today.utc, tz = tz), "%H:%M %Z"), " (",
      format(today.utc, "%d %b %H:%M %Z"), ").")
  } else if (!rstudio.test) {
    note <- paste0("Log for ", today.log, " not (yet) on server.")
  }
  
  last.wk <- seq(utc.date - 1, utc.date - 8, by = -1)
  
  log.chk <- vapply(last.wk, function(x) {
    tmp.url <- paste0(rstudio.url, year, '/', x, ".csv.gz")
    RCurl::url.exists(tmp.url)
  }, logical(1L))
  
  list("Available log" = last.wk[log.chk][1],
       "Today's log" = utc.date - 1,
       "Today's log posted?" = ifelse(rstudio.test, "Yes", "No"),
       "Today's results on 'cranlogs'?" = ifelse(cranlogs.test, "Yes", "No"),
       note = note)
}
