#' Check for valid package names.
#'
#' "spell check" package names.
#' @param package Character. Vector of package name(s).
#' @param print.message Logical. Print availablity/spell check messages.
#' @noRd

checkPackage <- function(package, print.message = TRUE) {
  orig.timeout <- getOption("timeout") # R default is 60
  if (orig.timeout < 600L) options(timeout = 600L)

  package0 <- package
  pkg.chk <- validatePackage(package)

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
    package <- c(no.err, false.err)
  } else if (test2) {
    package <- no.err
  } else if (test3) {
    package <- false.err 
  } 

  if (exists("true.err")) {
    if (length(true.err) > 0) {
      pkg.err.msg <- paste(true.err, collapse = ", ")
      msg <- "Misspelled or not on CRAN/Archive: "
      
      err.test <- length(setdiff(package, true.err)) == 0
      
      if (err.test) {
        stop(msg, pkg.err.msg, call. = FALSE)
      } else {
        if (print.message) message(msg, pkg.err.msg)
      }
      package <- package0[!package0 %in% true.err]
    }
  }
  
  options(timeout = orig.timeout)
  unique(package)
}
