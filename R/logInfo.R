#' Compute Availability, Date, Time of "Today's" Log.
#'
#' Also checks availability of Posit/RStudio logs and 'cranlogs' data.
#' @param tz Character. Local time zone. See OlsonNames() or use Sys.timezone().
#' @param upload.time Character. UTC upload time for logs "hh:mm" or "hh:mm:ss".
#' @param show.available Logical. Check available logs and results.
#' @export

logInfo <- function(tz = Sys.timezone(), upload.time = "17:00", 
  show.available = FALSE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  utc.date.time <- utc()
  utc.date <- as.Date(format(utc.date.time, "%Y-%m-%d"))

  upload.utc <- dateTime(utc.date, time = upload.time, tz = "GMT")
  upload.date <- as.Date(format(upload.utc, "%Y-%m-%d"))
  
  today.delta.time <- difftime(upload.utc, utc.date.time)
  upload.delta.time <- difftime(utc.date.time, upload.utc)
  
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
  
  if (rstudio.server.available & cranlogs.server.available) {
    if (rstudio.results.available & cranlogs.results.available) {
      status <- "Everything OK." 
    } else if (rstudio.results.available & !cranlogs.results.available) {
      status <- paste0("'cranlogs' usually posts a bit after ",
                       format(as.POSIXlt(upload.utc, tz = tz), "%H:%M %Z"), 
                       " (",
                       format(upload.utc, "%d %b %H:%M %Z"), ").")
    } else if (!rstudio.results.available) {
      status <- paste0("Today's log is typically posted by ",
                       format(as.POSIXlt(upload.utc, tz = tz), "%H:%M %Z"), 
                       " (",
                       format(upload.utc, "%d %b %H:%M %Z"), ").")
    }
  }
  
  rstudio.status <- ifelse(rstudio.results.available, "Yes.", "No.")
  cranlogs.status <- ifelse(cranlogs.results.available, "Yes.", "No.")
  
  out <- list("Today's log/result" = today.log,
              "Today's log on Posit/RStudio?" = rstudio.status,
              "Today's results on 'cranlogs'?" = cranlogs.status,
              status = status)

  if (show.available) {
    rev.last.wk <- seq(utc.date - 1, utc.date - 8, by = -1)
    
    last.available <- vapply(rev.last.wk, function(x) {
      tmp.url <- paste0(rstudio.url, year, '/', x, ".csv.gz")
      RCurl::url.exists(tmp.url)
    }, logical(1L))
    
    rstudio.last.available <- rev.last.wk[last.available][1]
    
    cranlogs.available <- try(cranlogs::cran_downloads(from = rev.last.wk[8],
      to = rev.last.wk[1]), silent = TRUE)
    if (any(class(cranlogs.available) == "try-error")) {
      cran.last.available <- NA
    } else {
      sel <- cranlogs.available$count != 0
      cran.last.available <- max(cranlogs.available[sel, "date"])
    }
    
    note <- paste0("Posit/RStudio ", "(", rstudio.last.available, ")",
                   "; 'cranlogs' ", "(", cran.last.available, ").")
    
    out <- list("Today's log/result" = today.log,
                "Today's log on Posit/RStudio?" = rstudio.status,
                "Today's results on 'cranlogs'?" = cranlogs.status,
                "Available log/result" = note,
                status = status)
  }
  out
}
