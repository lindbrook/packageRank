#' Filter out small downloads (prototype).
#'
#' @param dat Object. Package log entries.
#' @param threshold Numeric. Bytes.
#' @noRd

smallFilterB <- function(dat, threshold = 1000L) dat[dat$size >= threshold, ]
