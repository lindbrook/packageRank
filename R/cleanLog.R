#' Pre-clean CRAN Logs.
#'
#' package and size NAs; package size = 0
#' @param cran_log Object.
#' @noRd

cleanLog <- function(cran_log) {
  sel <- !is.na(cran_log$package) & !is.na(cran_log$size) & cran_log$size != 0
  cran_log[sel, ]
}
