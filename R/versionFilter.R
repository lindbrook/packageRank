#' Filter out prior package version(s) (prototype).
#'
#' Select/return latest version.
#' @param dat Object. Package log entries.
#' @param packages Character. Package name.
#' @param ymd Date. Requested date.
#' @noRd

versionFilter <- function(dat, packages, ymd) {
  cran <- packageCRAN(packages)
  if (!is.null(cran) & ymd >= cran$Date) {
    ver <- cran$Version
  } else {
    arch <- packageArchive(packages)
    ver <- arch[max(which(arch$Date <= ymd)), "Version"]
  }
  dat[dat$version == ver, ]
}
