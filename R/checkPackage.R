#' Check for valid package names.
#'
#' "spell check" package names.
#' @param packages Character. Vector of package name(s).
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @export

checkPackage <- function(packages, dev.mode = FALSE) {
  if (dev.mode) {
    pkg.chk <- validatePackage0(packages)
  } else {
    pkg.chk <- validatePackage(packages)
  }
  if (is.list(pkg.chk)) {
    error <- paste(pkg.chk$invalid, collapse = ", ")
    if (length(pkg.chk$valid) == 0) {
      stop(error, ": misspelled or not on CRAN/Archive.")
    } else {
      warning(error, ": misspelled or not on CRAN/Archive.")
      packages <- pkg.chk$valid
    }
  }
  unique(packages)
}
