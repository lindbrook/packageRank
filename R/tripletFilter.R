#' Filter out small downloads triplets (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @param time.window Numeric. Seconds.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

tripletFilter <- function(dat, time.window = 2, multi.core = TRUE) {
  triplets <- identifyTriplets(dat, time.window = time.window,
    multi.core = multi.core)
  if (!is.null(triplets)) {
    delete <- row.names(triplets[seq_len(nrow(triplets)) %% 3 != 0, ])
    if (!is.null(delete)) {
      dat[row.names(dat) %in% delete == FALSE, ]
    } else dat
  } else dat
}
