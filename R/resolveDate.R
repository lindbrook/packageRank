#' Check and resolve start date (prototype).
#'
#' @param date Date \code{yyyy-mm-dd}.
#' @export

resolveFromDate <- function(date) {
  first.log <- as.Date("2012-10-01") # first log on RStudio CRAN mirror.
  cal.date <- Sys.Date() - 1

  if (nchar(date) == 10L & grepl("-", date)) {
    start.date <- as.Date(date, optional = TRUE)
  } else if (nchar(date) == 7L & grepl("-", date)) {
    start.date <- dayOfMonth(date, first.log)
  } else if (nchar(date) == 4L) {
    start.date <- as.Date(paste0(date, "-01-01"), optional = TRUE)
  }

  date.err.msg <- 'Invalid date or format: "yyyy-mm-dd", "yyyy-mm" or "yyyy".'
  if (is.na(start.date)) stop(date.err.msg)

  if (start.date < as.Date(first.log)) {
    warning(paste0('RStudio CRAN logs begin on ', first.log, "."))
    start.date <- first.log
  } else if (start.date > cal.date) {
    stop("Date in future.")
  } else start.date
}

#' Check and resolve end date (prototype).
#'
#' @param date Date \code{yyyy-mm-dd}.
#' @export

resolveToDate <- function(date) {
  first.log <- as.Date("2012-10-01") # first log on RStudio CRAN mirror.
  cal.date <- Sys.Date() - 1

  if (nchar(date) == 10L & grepl("-", date)) {
    end.date <- as.Date(date, optional = TRUE)
  } else if (nchar(date) == 7L & grepl("-", date)) {
    end.date <- dayOfMonth(date, first.log, end.of.month = TRUE)
  } else if (nchar(date) == 4L) {
    end.date <- as.Date(paste0(date, "-12-31"), optional = TRUE)
    if (end.date > cal.date) end.date <- cal.date
  }

  date.err.msg <- 'Invalid date or format: "yyyy-mm-dd", "yyyy-mm" or "yyyy".'
  if (is.na(end.date)) stop(date.err.msg)

  if (end.date < as.Date(first.log)) {
    warning(paste0('RStudio CRAN logs begin on ', first.log, "."))
    end.date <- first.log
  } else if (end.date > cal.date) {
    warning("Date in future.")
  } else end.date
}

dayOfMonth <- function(string, first.log, end.of.month = FALSE) {
  if (is.character(string) == FALSE) stop("string must a text string.")

  if (nchar(string) != 7 | (grepl("-", string) == FALSE)) {
    stop('Format must be "yyyy-mm".')
  } else {
    date.parts <- unlist(strsplit(string, "-"))
    if (date.parts[2] %in% c(paste0(0, 1:9), paste(10:12)) == FALSE) {
      stop("Month must be between 01 and 12.")
    }
    if (date.parts[1] < data.table::year(first.log)) {
      warning(paste0('RStudio CRAN logs begin on ', first.log, "."))
    }
  }

  if (end.of.month) {
    end.candidates <- lapply(28:31, function(day) {
      as.Date(paste0(string , "-", day), optional = TRUE)
    })
    end.candidates <- do.call(c, end.candidates)
    end.selelct <- rev(end.candidates[!is.na(end.candidates)])[1]
    out <- as.Date(end.selelct, optional = TRUE)
  } else {
    out <- as.Date(paste0(string , "-01"), optional = TRUE)
  }

  out
}
