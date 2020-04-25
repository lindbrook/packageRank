#' Resolve date.
#'
#' Check date format and validate date.
#' @param date Character. \code{"yyyy-mm-dd"}, \code{"yyyy-mm"}, \code{"yyyy"} or \code{yyyy} (numeric).
#' @param type Character. Type of date "to" or "from".
#' @export

resolveDate <- function(date, type = "from") {
  first.log <- as.Date("2012-10-01") # first log on RStudio CRAN mirror.
  cal.date <- Sys.Date() - 1
  mm <- c(paste0(0, 1:9), paste(10:12))

  if (type %in% c("to", "from") == FALSE) {
    stop('type must be "to" or "from".')
  }

  if (nchar(date) == 10L & grepl("-", date)) {
    err.format <- 'Invalid format. Must be "yyyy-mm-dd".'
    date.check <- unlist(strsplit(date, "-"))
    if (!length(date.check) == 3) {
      stop(err.format)
    } else if (!all(vapply(date.check, nchar, integer(1L)) == c(4, 2, 2))) {
      stop(err.format)
    } else if (date.check[2] %in% mm == FALSE) {
      stop("Month must be between 01 and 12.")
    } else {
      x.date <- as.Date(date, optional = TRUE)
    }
  } else if (nchar(date) == 7L & grepl("-", date)) {
    err.format <- 'Invalid format. Must be "yyyy-mm".'
    date.check <- unlist(strsplit(date, "-"))
    if (!length(date.check) == 2) {
      stop(err.format)
    } else if (!all(vapply(date.check, nchar, integer(1L)) == c(4, 2))) {
      stop(err.format)
    } else if (date.check[2] %in% mm == FALSE) {
      stop("Month must be between 01 and 12.")
    } else if (type == "from") {
      x.date <- dayOfMonth(date, first.log)
    } else if (type == "to") {
      x.date <- dayOfMonth(date, first.log, end.of.month = TRUE)
    }
  } else if (nchar(date) == 4L) {
    if (is.na(suppressWarnings(as.numeric(date)))) {
      msg1 <- 'yyyy must either be a 4 digit number'
      msg2 <- 'or string of 4 numbers ("yyyy").'
      stop(msg1, msg2)
    } else if (type == "from") {
      x.date <- as.Date(paste0(date, "-01-01"), optional = TRUE)
    } else if (type == "to") {
      x.date <- as.Date(paste0(date, "-12-31"), optional = TRUE)
      if (x.date > cal.date) x.date <- cal.date
    }
  } else stop('Format must be "yyyy-mm-dd", "yyyy-mm", "yyyy", or yyyy.')

  if (is.na(x.date)) {
    stop('Not a valid date.')
  } else if (x.date < as.Date(first.log)) {
    warning(paste0('RStudio CRAN logs begin on ', first.log, "."))
    x.date <- first.log
  } else if (x.date > cal.date) {
    stop("Date in future!")
  } else x.date
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

#' Check and validate "yyyy-mm-dd" date.
#'
#' @param date Character. \code{"yyyy-mm-dd"}.
#' @noRd

check10CharDate <- function(date) {
  first.log <- as.Date("2012-10-01") # first log on RStudio CRAN mirror.
  cal.date <- Sys.Date() - 1
  mm <- c(paste0(0, 1:9), paste(10:12))
  date <- as.character(date)

  if (nchar(date) == 10L & grepl("-", date)) {
    err.format <- 'Invalid format. Must be "yyyy-mm-dd".'
    date.check <- unlist(strsplit(date, "-"))
    if (!length(date.check) == 3) {
      stop(err.format)
    } else if (!all(vapply(date.check, nchar, integer(1L)) == c(4, 2, 2))) {
      stop(err.format)
    } else if (date.check[2] %in% mm == FALSE) {
      stop("Month must be between 01 and 12.")
    } else {
      x.date <- as.Date(date, optional = TRUE)
    }
  } else stop('Format must be "yyyy-mm-dd".')

  if (is.na(x.date)) {
    stop('Not a valid date.')
  } else if (x.date < as.Date(first.log)) {
    warning(paste0('RStudio CRAN logs begin on ', first.log, "."))
    x.date <- first.log
  } else if (x.date > cal.date) {
    stop("Date in future!")
  } else x.date
}
