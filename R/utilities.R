#' Extract Package Logs.
#'
#' @param i Numeric. Day/ID.
#' @param pkg Character.
#' @param dat Object. List of logs.
#' @export

pkgLog <- function(i = 1, pkg = "cholera", dat = july_log) {
  cran_log <- dat[[i]]
  tmp <- cran_log[!is.na(cran_log$package) & cran_log$package == pkg, ]
  tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "Europe/Vienna")
  tmp[order(tmp$t2), c(1:6, 8:10)]
}
