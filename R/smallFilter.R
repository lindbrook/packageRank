#' Filter out small downloads (prototype).
#'
#' @param dat Object. Package log entries.
#' @param threshold Numeric. Bytes.
#' @noRd

smallFilter <- function(dat, threshold = 1000L) {
  if (any(dat$size >= threshold)) dat[dat$size >= threshold, ]
  else dat
}