#' Check and validate "yyyy-mm-dd" date.
#'
#' @param date Character. \code{"yyyy-mm-dd"}.
#' @param repository Character. "CRAN" or "MRAN".
#' @export

check10CharDate <- function(date, repository = "CRAN") {
  if (repository == "CRAN") {
    first.log <- as.Date("2012-10-01") # first log on RStudio CRAN mirror
  } else if (repository == "MRAN") {
     first.log <- as.Date("2014-09-17") # MRAN timemachine
  } else stop('repository must be "CRAN" or "MRAN".')

  cal.date <- Sys.Date() - 1
  mm <- c(paste0(0, 1:9), paste(10:12))
  date <- as.character(date)

  if (nchar(date) == 10L & grepl("-", date)) {
    err.format <- 'Invalid format. Must be "yyyy-mm-dd".'
    date.check <- unlist(strsplit(date, "-"))
    if (!length(date.check) == 3) {
      stop(err.format, call. = FALSE)
    } else if (!all(vapply(date.check, nchar, integer(1L)) == c(4, 2, 2))) {
      stop(err.format, call. = FALSE)
    } else if (date.check[2] %in% mm == FALSE) {
      stop("Month must be between 01 and 12.", call. = FALSE)
    } else {
      x.date <- as.Date(date, optional = TRUE)
    }
  } else stop('Format must be "yyyy-mm-dd".', call. = FALSE)

  if (is.na(x.date)) {
    stop('Not a valid date.', call. = FALSE)
  } else if (x.date < as.Date(first.log)) {
    if (repository == "CRAN") {
      txt <- 'RStudio CRAN logs begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    } else if (repository == "MRAN") {
      txt <- 'MRAN snapshots begin on '
      stop(paste0(txt, first.log, "."), call. = FALSE)
    }
    # x.date <- first.log
  } else if (x.date > cal.date) {
    stop("Date in future!", call. = FALSE)
  } else x.date
}
