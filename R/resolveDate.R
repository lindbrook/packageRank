#' Check and resolve date (prototype).
#'
#' @param date Date \code{yyyy-mm-dd}.
#' @param type Character. type of data "to" or "from".
#' @export

resolveDate <- function(date, type = "from") {
  if (type %in% c("to", "from") == FALSE) stop('type must be "to" or "from".')
  first.log <- as.Date("2012-10-01") # first log on RStudio CRAN mirror.
  cal.date <- Sys.Date() - 1

  if (nchar(date) == 10L & grepl("-", date)) {
    x.date <- as.Date(date, optional = TRUE)
  } else if (nchar(date) == 7L & grepl("-", date)) {
    if (type == "from") {
      x.date <- dayOfMonth(date, first.log)
    } else if (type == "to") {
      x.date <- dayOfMonth(date, first.log, end.of.month = TRUE)
    }
  } else if (nchar(date) == 4L) {
    if (type == "from") {
      x.date <- as.Date(paste0(date, "-01-01"), optional = TRUE)
    } else if (type == "to") {
      x.date <- as.Date(paste0(date, "-12-31"), optional = TRUE)
      if (x.date > cal.date) x.date <- cal.date
    }
  }

  date.err.msg <- 'Invalid date or format: "yyyy-mm-dd", "yyyy-mm" or "yyyy".'
  if (is.na(x.date)) stop(date.err.msg)

  if (x.date < as.Date(first.log)) {
    warning(paste0('RStudio CRAN logs begin on ', first.log, "."))
    x.date <- first.log
  } else if (x.date > cal.date) {
    stop("Date in future.")
  } else x.date
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
