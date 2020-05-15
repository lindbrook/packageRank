#' Create a time window for download log files (prototype).
#'
#' @param t Character. Time as "hh:mm:ss".
#' @param window Numeric. Delta in seconds for time window.
#' @examples
#' timeWindow("00:02:00")
#' timeWindow("09:12:53", 2)
#' @export

timeWindow <- function(t, window = 1) {
  if (window > 5) stop("window <= 5.")
  hms <- as.numeric(unlist(strsplit(t, ":")))
  secs.above <- (hms[3] + 1):(hms[3] + window)
  secs.below <- (hms[3] - window):(hms[3] - 1)

  if (any(secs.above >= 60)) {
    mins.above <- rep(hms[2], length(secs.above))
    mins.above <- mins.above[secs.above >= 60] + 1
    secs.above <- secs.above - 60
    if (any(mins.above >= 60)) {
      hrs.above <- rep(hms[1], length(mins.above))
      hrs.above <- hrs.above[mins.above >= 60] + 1
      if (any(hrs.above > 23)) stop("day change.")
    } else {
      hours.above <- hms[1]
    }
    h <- ifelse(nchar(hours.above) == 1, paste0("0", hours.above), hours.above)
    m <- ifelse(nchar(mins.above) == 1, paste0("0", mins.above), mins.above)
    s <- ifelse(nchar(secs.above) == 1, paste0("0", secs.above), secs.above)
    t.above <- paste0(h, ":", m, ":", s)
  } else {
    h <- ifelse(nchar(hms[1]) == 1, paste0("0", hms[1]), hms[1])
    m <- ifelse(nchar(hms[2]) == 1, paste0("0", hms[2]), hms[2])
    s <- ifelse(nchar(secs.above) == 1, paste0("0", secs.above), secs.above)
    t.above <- paste0(h, ":", m, ":", s)
  }

  if (any(secs.below < 0)) {
    mins.below <- rep(hms[2], length(secs.below))
    mins.below <- mins.below[secs.below < 0] - 1
    secs.below <- secs.below + 60
    if (any(mins.below < 0)) {
      hrs.below <- rep(hms[1], length(mins.below))
      hrs.below <- hrs.below[mins.below < 0] - 1
      if (any(hrs.below < 0)) stop("day change.")
    } else {
      hours.below <- hms[1]
    }
    h <- ifelse(nchar(hours.below) == 1, paste0("0", hours.below), hours.below)
    m <- ifelse(nchar(mins.below) == 1, paste0("0", mins.below), mins.below)
    s <- ifelse(nchar(secs.below) == 1, paste0("0", secs.below), secs.below)
    t.below <- paste0(h, ":", m, ":", s)
  } else {
    h <- ifelse(nchar(hms[1]) == 1, paste0("0", hms[1]), hms[1])
    m <- ifelse(nchar(hms[2]) == 1, paste0("0", hms[2]), hms[2])
    s <- ifelse(nchar(secs.below) == 1, paste0("0", secs.below), secs.below)
    t.below <- paste0(h, ":", m, ":", s)
  }

  c(t.below, t, t.above)
}
