#' Resolve date.
#'
#' Check date format and validate date.
#' @param date Character. \code{"yyyy-mm-dd"}, \code{"yyyy-mm"}, \code{"yyyy"} or \code{yyyy} (numeric).
#' @param type Character. Type of date "to" or "from".
#' @param fix.date. Fix date when directly accessing RStudio logs.
#' @noRd

resolveDate <- function(date, type = "from", fix.date = FALSE) {
  if (!type %in% c("to", "from")) {
    stop('type must be "to" or "from".', call. = FALSE)
  }

  first.log <- as.Date("2012-10-01") # first log on RStudio CRAN mirror.
  date.txt <- as.character(date)
  cal.date <- logDate()
  mm <- c(paste0(0, 1:9), paste(10:12))

  if (nchar(date.txt) == 7L & grepl("-", date.txt)) {
    err.format <- 'Check format. Must be "yyyy-mm".'
    date.check <- unlist(strsplit(date.txt, "-"))
    if (!length(date.check) == 2) {
      stop(err.format, call. = FALSE)
    } else if (!all(vapply(date.check, nchar, integer(1L)) == c(4, 2))) {
      stop(err.format, call. = FALSE)
    } else if (date.check[2] %in% mm == FALSE) {
      stop("Month must be between 01 and 12.", call. = FALSE)
    } else if (type == "from") {
      x.date <- dayOfMonth(date.txt, first.log)
    } else if (type == "to") {
      x.date <- dayOfMonth(date.txt, first.log, end.of.month = TRUE)
    }
  } else if (nchar(date.txt) == 4L) {
    if (is.na(suppressWarnings(as.numeric(date.txt)))) {
      msg1 <- 'yyyy must either be a 4 digit number'
      msg2 <- 'or string of 4 numbers ("yyyy").'
      stop(msg1, msg2, call. = FALSE)
    } else if (type == "from") {
      x.date <- as.Date(paste0(date.txt, "-01-01"), optional = TRUE)
    } else if (type == "to") {
      x.date <- as.Date(paste0(date.txt, "-12-31"), optional = TRUE)
      if (x.date > cal.date) x.date <- cal.date
    }
  } else {
    date <- as.Date(date, optional = TRUE)
    if (!is.na(date)) {
      x.date <- date
    } else {
      msg1 <- 'Not a valid date or format:'
      msg2 <- ' "yyyy-mm-dd", "yyyy-mm", "yyyy", or yyyy.'
      stop(paste0(msg1, msg2), call. = FALSE)
    }
  }

  if (x.date < first.log) {
    if (x.date < first.log) x.date <- first.log
    message(paste0('Note: RStudio CRAN logs begin on ', first.log, "."))
  } else x.date

  logDate(x.date, warning.msg = FALSE, fix.date = fix.date)
}

dayOfMonth <- function(string, first.log, end.of.month = FALSE) {
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
