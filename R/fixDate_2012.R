#' Re-map filename/URL (date) for 2012 download logs.
#'
#' To get the "right" log.
#' @param date Character. Date. "yyyy-mm-dd".
#' @note This date problem does not affect \code{cranDownlaods()}.
#' @return A one unit R date or character vector.
#' @noRd

fixDate_2012 <- function(date = "2012-12-31") {
  if (!inherits(date, "Date")) ymd <- as.Date(date)
  else ymd <- date

  if (format(ymd, "%Y") == "2012") {
    if (ymd %in% as.Date(c("2012-12-29", "2012-12-30", "2012-12-31"))) {
      stop("Log for ", ymd, " is missing.", call. = FALSE)
    } else if (ymd >= as.Date("2012-10-13") & ymd <= as.Date("2012-12-28")) {
      ymd <- ymd + 3
    } else if (ymd %in% as.Date(c("2012-10-11", "2012-10-12"))) {
      # Nominal Actual
      # 11 ----- 07
      # 12 ----- 11
      # 13 ----- 08
      # 14 ----- 12
      # 15 ----- 11
      if (identical(ymd, as.Date("2012-10-11"))) {
        ymd <- as.Date("2012-10-12")
      } else if (identical(ymd, as.Date("2012-10-12"))) {
        ymd <- as.Date("2012-10-14")
      }
    }
  }
  ymd
}

#' Reverse map filename/URL (date) for 2012 download logs.
#'
#' To print the desired/expected date.
#' @param file.url.date Date. "yyyy-mm-dd".
#' @return A one unit R date vector.
#' @noRd

rev_fixDate_2012 <- function(file.url.date) {
  if (format(file.url.date, "%Y") == "2012") {
    offset.date <- file.url.date >= as.Date("2012-10-16") &
                   file.url.date <= as.Date("2012-12-31")

    if (offset.date) ymd <- file.url.date - 3L
    else ymd <- file.url.date

    if (file.url.date == as.Date("2012-10-12")) {
      ymd <- as.Date("2012-10-11")
    } else if (file.url.date == as.Date("2012-10-14")) {
      ymd <- as.Date("2012-10-12")
    }
    ymd
  } else file.url.date
}
