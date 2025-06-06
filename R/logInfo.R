#' Compute Availability, Date, Time of "Today's" Log.
#'
#' Also checks availability of Posit/RStudio logs and 'cranlogs' data.
#' @param details Logical. Check available logs and results.
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @param upload.time Character. UTC upload time for logs "hh:mm" or "hh:mm:ss".
#' @param check.days Numeric or Integer. Number of days back to check.
#' @export

logInfo <- function(details = FALSE, tz = Sys.timezone(), 
  upload.time = "17:00", check.days = 7) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  utc.date.time <- utc()
  utc.date <- as.Date(format(utc.date.time, "%Y-%m-%d"))

  upload.utc <- dateTime(utc.date, upload.time, tz = "UTC")
  upload.date <- as.Date(format(upload.utc, "%Y-%m-%d"))
 
  today.log <- utc.date - 1
  today.upload <- as.POSIXlt(upload.utc, tz = tz)
  
  year <- format(today.log, "%Y")
  rstudio.url <- "http://cran-logs.rstudio.com/"
  log.url <- paste0(rstudio.url, year, '/', today.log, ".csv.gz")
  rstudio.server.available <- RCurl::url.exists(rstudio.url)
  rstudio.results.available <- RCurl::url.exists(log.url)
  
  clogs <- try(cranlogs::cran_downloads(from = today.log, to = today.log),
    silent = TRUE)

  if (any(class(clogs) == "try-error")) {
    cranlogs.server.available <- FALSE
    cranlogs.results.available <- FALSE
  } else {
    cranlogs.server.available <- TRUE
    cranlogs.results.available <- ifelse(clogs$count != 0, TRUE, FALSE)
  }

  if (!rstudio.server.available & !cranlogs.server.available) {
    status <- "'cranlogs' and Posit/RStudio servers unavailable."
  } else if (rstudio.server.available & !cranlogs.server.available) {
    status <- "'cranlogs' server unavailable."
  } else if (!rstudio.server.available & cranlogs.server.available) {
    status <- "Posit/RStudio server unavailable."
  }

  date.fmt <- "%d %b %H:%M %Z"
  upload.loc <- localTime(upload.date, time = upload.time, tz = tz)
  
  if (rstudio.server.available & cranlogs.server.available) {
    if (rstudio.results.available & cranlogs.results.available) {
      status <- "Everything OK." 
    } else if (rstudio.results.available & !cranlogs.results.available) {
      status <- paste0("Today's 'cranlogs' usually posts by ",
                       format(upload.loc + 3600L, date.fmt), " | ", 
                       format(upload.utc + 3600L, date.fmt), ".")
    } else if (!rstudio.results.available) {
      status <- paste0("Today's log usually posts by ",
                       format(upload.loc, date.fmt), " | ",
                       format(upload.utc, date.fmt), ".")
    }
  }
  
  log.status <- ifelse(rstudio.results.available, "Yes.", "No.")
  cranlogs.status <- ifelse(cranlogs.results.available, "Yes.", "No.")
  
  out <- list("Today's log/result" = today.log,
              "Today's log on Posit/RStudio?" = log.status,
              "Today's result on 'cranlogs'?" = cranlogs.status,
              status = status)
  
  if (details) {
    rev.dates <- seq(utc.date - 1, utc.date - check.days, by = -1)
    
    logs.available <- vapply(rev.dates, function(x) {
      tmp.url <- paste0(rstudio.url, year, '/', x, ".csv.gz")
      RCurl::url.exists(tmp.url)
    }, logical(1L))

    logs.last.available <- rev.dates[logs.available][1]

    cranlogs.available <- try(cranlogs::cran_downloads(
      from = rev.dates[check.days], to = rev.dates[1]), silent = TRUE)
    
    if (any(class(cranlogs.available) == "try-error")) {
      cran.last.available <- NA
    } else {
      obs <- cranlogs.available$count != 0
      if (any(obs)) {
        cran.last.available <- max(cranlogs.available[obs, "date"])  
      } else if (all(!obs)) {
        cran.last.available <- NA
      }
    }
    
    note <- paste0("Posit/RStudio ", "(", logs.last.available, ")",
                   "; 'cranlogs' ", "(", cran.last.available, ").")
    
    out <- list("Today's log/result" = today.log,
                "Today's log on Posit/RStudio?" = log.status,
                "Today's results on 'cranlogs'?" = cranlogs.status,
                "Available log/result" = note,
                "Current date-time" = paste0(format(Sys.time(), date.fmt)),
                status = status)
  }
  out
}
