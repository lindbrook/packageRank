#' Filter out small downloads (prototype).
#'
#' @param dat Object. Package log entries.
#' @param threshold Numeric. Bytes.
#' @noRd

smallFilter <- function(dat, threshold = 1000L) {
  if (is.data.frame(dat)) dat[dat$size >= threshold, ]
  else if (is.list(dat)) lapply(dat, function(x) x[x$size >= threshold, ])
}
