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
#' @param version.filter Logical. TRUE selects only most recent version.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @return An R data frame.
#' @export

packageLog <- function(packages = "cholera", date = NULL, all.filters = FALSE,
  ip.filter = FALSE, sequence.filter = FALSE, size.filter = FALSE, 
  small.filter = FALSE, version.filter = FALSE, memoization = TRUE, 
  check.package = TRUE, multi.core = FALSE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  if (check.package) packages <- checkPackage(packages)
  
  if (.Platform$OS.type == "windows") {
    cores <- 1L
  } else {
    cores <- multiCore(multi.core)
  }
  
  log.date <- logDate(date)
  cran_log <- fetchCranLog(date = log.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  ymd <- rev_fixDate_2012(log.date)

  unobs.pkgs <- !packages %in% cran_log$package
  if (any(unobs.pkgs)) pkg.msg <- paste(packages[unobs.pkgs], collapse = ", ")

  if (all(unobs.pkgs)) {
    stop("No downloads for ", pkg.msg, " on ", ymd, ".", call. = FALSE)
  } else if (any(unobs.pkgs)) {
    message("No downloads for ", pkg.msg, " on ", ymd, ".")
    packages <- packages[!unobs.pkgs]
  }

  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
    size.filter <- TRUE
    version.filter <- TRUE
  }
  
  if (ip.filter) cran_log <- ipFilter(cran_log, multi.core = FALSE)
  
  pkg.data <- lapply(packages, function(p) cran_log[cran_log$package == p, ])
  
  out <- parallel::mclapply(seq_along(pkg.data), function(i) { 
    p.dat <- pkg.data[[i]]
    p <- packages[i]
    
    if (nrow(p.dat) != 0) {
      if (small.filter) p.dat <- smallFilter(p.dat)
      
      p.dat$date.time <- dateTime(p.dat$date, p.dat$time)
      if (sequence.filter) p.dat <- sequenceFilter(p.dat, p, ymd)
      
      if (size.filter) p.dat <- sizeFilter(p.dat, p)
      if (version.filter) p.dat <- versionFilter(p.dat, p, ymd)
      p.dat <- p.dat[order(p.dat$date.time), ]
      p.dat$date.time <- NULL
    }
    p.dat
  }, mc.cores = cores)
  
  names(out) <- packages
  
  pkgs.survived <- vapply(out, function(x) x[1, "package"], character(1L))
  pkg.not_survived <- setdiff(packages, pkgs.survived)
  
  if (length(pkg.not_survived) > 0) {
    message("No filtered downloads for ", paste(pkg.not_survived, 
      collapse = ", "), ".")
  }
  
  if (length(packages) == 1) out[[1]]
  else out
}
