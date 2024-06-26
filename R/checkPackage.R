#' Check for valid package names.
#'
#' "spell check" package names.
#' @param packages Character. Vector of package name(s).
#' @param print.message Logical. Print availablity/spell check messages.
#' @noRd

checkPackage <- function(packages, print.message = TRUE) {
  orig.timeout <- getOption("timeout") # R default is 60
  if (orig.timeout < 600L) options(timeout = 600L)

  packages0 <- packages
  pkg.chk <- validatePackage(packages)

  if (any(pkg.chk$pkgsearch == FALSE)) {
    # 'pkgsearch' errors (not in db); removed (old w/o details) 
    # archive.package.errors <- c("dseplus", "empiricalBayes", "forecasting")
    pkgsearch.err <- pkg.chk[pkg.chk$pkgsearch == FALSE, "package"]
    
    # directly check Archive
    pkg.chk0 <- lapply(pkgsearch.err, validatePackage0)
    no.cran.archive <- vapply(pkg.chk0, is.list, logical(1L))
    true.err <- pkgsearch.err[no.cran.archive]
    false.err <- pkgsearch.err[!no.cran.archive]
  }
  
  if (any(pkg.chk$pkgsearch == TRUE)) {
    no.err <- pkg.chk[pkg.chk$pkgsearch == TRUE, "package"]
  }
  
  test1 <- exists("no.err") & exists("false.err") & length("false.err") > 0
  test2 <- exists("no.err") & exists("false.err") & length("false.err") == 0
  test3 <- !exists("no.err") & exists("false.err") & length("false.err") > 0

  if (test1) {
    packages <- c(no.err, false.err)
  } else if (test2) {
    packages <- no.err
  } else if (test3) {
    packages <- false.err 
  } 

  if (exists("true.err")) {
    if (length(true.err) > 0) {
      pkg.err.msg <- paste(true.err, collapse = ", ")
      msg <- "Misspelled or not on CRAN/Archive: "
      
      err.test <- length(setdiff(packages, true.err)) == 0
      
      if (err.test) {
        stop(msg, pkg.err.msg, call. = FALSE)
      } else {
        if (print.message) message(msg, pkg.err.msg)
      }
      packages <- packages0[!packages0 %in% true.err]
    }
  }
  
  options(timeout = orig.timeout)
  unique(packages)
}
