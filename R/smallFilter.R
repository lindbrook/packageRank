#' Filter out small downloads (prototype).
#'
#' @param dat Object. Package log entries.
#' @param threshold Numeric. Bytes.
#' @export

smallFilter <- function(dat, threshold = 1000L) {
  dat[dat$size >= threshold, ]
}
