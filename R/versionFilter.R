#' Filter out prior package version(s) (prototype).
#'
#' Select/return latest version.
#' @param dat Object. Package log entries.
#' @param packages Character. Package name.
#' @param ymd Date. Requested date.
#' @noRd

versionFilter <- function(dat, packages, ymd) {
  history <- packageHistory(packages, check.package = FALSE)
  ver.test <- history$Date <= ymd
  if (length(ver.test) == 1) {
    ver <- history[ver.test, "Version"]
  } else {
    ver <- history[max(which(ver.test)), "Version"]
  }
  dat[dat$version == ver, ]
}
