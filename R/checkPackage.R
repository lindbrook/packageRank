#' Check for valid package names.
#'
#' "spell check" package names.
#' @param packages Character. Vector of package name(s).
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @noRd

checkPackage <- function(packages, dev.mode = FALSE) {
  if (dev.mode) {
    pkg.chk <- validatePackage0(packages)
  } else {
    pkg.chk <- validatePackage(packages)
  }

  # 'pkgsearch' errors
  archive.package.errors <- c("dseplus", "empiricalBayes", "forecasting", "VR")

  if (any(packages %in% archive.package.errors)) {
    pkgs <- packages[!packages %in% archive.package.errors]
    pkgs0 <- packages[packages %in% archive.package.errors]
    pkg.chk <- validatePackage(pkgs)
    pkg.chk0 <- validatePackage0(pkgs0)
    if (!is.list(pkg.chk)) {
      delta <- setdiff(packages, c(pkg.chk, pkg.chk0))
      if (length(delta) != 0) arch.pkg.err <- delta
    }
  }

  if (is.list(pkg.chk) & "arch.pkg.err" %in% ls()) {
    pkg.err <- c(pkg.chk$invalid, arch.pkg.err)
  } else if (is.list(pkg.chk) & !"arch.pkg.err" %in% ls()) {
    pkg.err <- pkg.chk$invalid
  } else if (!is.list(pkg.chk) & "arch.pkg.err" %in% ls()) {
    pkg.err <- arch.pkg.err
  }

  if ("pkg.err" %in% ls()) {
    pkg.err.msg <- paste(pkg.err, collapse = ", ")
    msg <- ": misspelled or not on CRAN/Archive."
    err.test <- length(setdiff(packages, pkg.err)) == 0

    if (err.test) {
      stop(pkg.err.msg, msg, call. = FALSE)
    } else {
      warning(pkg.err.msg, msg, call. = FALSE)
      if ("pkg.chk0" %in% ls()) {
        packages <- c(pkg.chk$valid, pkg.chk0)
      } else {
        packages <- pkg.chk$valid
      }
    }
  }

  unique(packages)
}
