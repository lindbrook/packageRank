#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @return An R data frame.
#' @export

packageLog <- function(packages = "cholera", date = NULL, all.filters = FALSE,
  ip.filter = FALSE, sequence.filter = FALSE, size.filter = FALSE, 
  small.filter = FALSE, memoization = TRUE, check.package = TRUE, 
  multi.core = FALSE, dev.mode = FALSE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  if (check.package) packages <- checkPackage(packages)
  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)

  ymd <- rev_fixDate_2012(file.url.date)
  cores <- multiCore(multi.core)

  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
    sequence.filter <- TRUE
    size.filter <- TRUE
  }

  if (ip.filter) {
    cran_log <- ipFilter(cran_log, multi.core = cores, dev.mode = dev.mode)
  }

  unobs.pkgs <- !packages %in% cran_log$package
  if (any(unobs.pkgs)) pkg.msg <- paste(packages[unobs.pkgs], collapse = ", ")

  if (all(unobs.pkgs)) {
    stop("No downloads for ", pkg.msg, " on ", ymd, ".", call. = FALSE)
  } else if (any(unobs.pkgs)) {
    message("No downloads for ", pkg.msg, " on ", ymd, ".")
    packages <- packages[!unobs.pkgs]
  }

  out <- lapply(packages, function(p) cran_log[cran_log$package == p, ])
  
  if (small.filter) {
    out <- smallFilter(out)
  }
  
  if (sequence.filter) {
    out <- sequenceFilter(out, packages, ymd, cores, dev.mode = dev.mode)
  }
  
  if (size.filter) {
    out <- sizeFilter(out, packages, cores, dev.mode = dev.mode)
  }

  out <- lapply(out, function(x) {
    if (!"t2" %in% names(x)) x$date.time <- dateTime(x$date, x$time)
    tmp <- x[order(x$date.time), ]
    tmp$date.time <- NULL
    tmp
  })
  
  pkgs.survived <- vapply(out, function(x) x[1, "package"], character(1L))
  pkg.not_survived <- setdiff(packages, pkgs.survived)

  if (length(pkg.not_survived) > 0) {
    message("No filtered downloads for ", paste(pkg.not_survived, 
      collapse = ", "), ".")
  }
  
  names(out) <- pkgs.survived
  
  if (length(packages) == 1) {
    out[[1]]
  } else {
    out
  }
}
