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

  # 'pkgsearch' errors
  archive.err <- c("dseplus", "empiricalBayes", "forecasting", "VR")

  if (any(packages %in% archive.err)) {
    pkgs <- packages[!packages %in% archive.err]
    pkgs0 <- packages[packages %in% archive.err]
    pkg.chk <- validatePackage(pkgs)
    pkg.chk0 <- validatePackage0(pkgs0)
    if (!is.list(pkg.chk)) {
      delta <- setdiff(packages, c(pkg.chk, pkg.chk0))
      if (length(delta) != 0) {
        err2 <- paste(delta, collapse = ", ")
      }
    }
  }

  if (is.list(pkg.chk) & !"err2" %in% ls()) {
    error <- paste(pkg.chk$invalid, collapse = ", ")
  } else if (!is.list(pkg.chk) & "err2" %in% ls()) {
    error <- paste(err2, collapse = ", ")
  } else if (is.list(pkg.chk) & "err2" %in% ls()) {
    error <- paste(c(pkg.chk$invalid, err2), collapse = ", ")
  }

  msg <- ": misspelled or not on CRAN/Archive."

  if (setdiff(error, packages) == 0) {
    stop(error, msg, call. = FALSE)
  } else {
    warning(error, msg, call. = FALSE)
    if ("pkg.chk0" %in% ls()) {
      packages <- c(pkg.chk$valid, pkg.chk0)
    } else {
      packages <- pkg.chk$valid
    }
  }

  unique(packages)
}
