#' Filter out prior package version(s) (prototype).
#'
#' Select/return latest version.
#' @param dat Object. Package log entries.
#' @param packages Character. Package name.
#' @noRd

versionFilter <- function(dat, packages) {
  cran <- packageCRAN(packages)
  if (!is.null(cran)) {
    ver <- cran$Version
  } else {
    arch <- packageArchive(packages)
    ver <- arch[nrow(arch), "Version"]
  }
  dat[dat$version == ver, ]
}
