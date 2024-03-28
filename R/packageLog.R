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
#' @return An R data frame.
#' @export

packageLog <- function(packages = "cholera", date = NULL, all.filters = FALSE,
  ip.filter = FALSE, sequence.filter = FALSE, size.filter = FALSE, 
  small.filter = FALSE, memoization = TRUE, check.package = TRUE, 
  multi.core = FALSE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  
  cores <- multiCore(multi.core)
  if (.Platform$OS.type == "windows" & cores > 1) cores <- 1L

  if (check.package) packages <- checkPackage(packages)
  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  ymd <- rev_fixDate_2012(file.url.date)

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
    sequence.filter <- TRUE
    size.filter <- TRUE
  }
  
  if (ip.filter) cran_log <- ipFilter(cran_log, multi.core = cores)
  
  out <- parallel::mclapply(packages, function(p) {
    pkg.data <- cran_log[cran_log$package == p, ]
    if (small.filter) pkg.data <- smallFilter(pkg.data)
    if (sequence.filter) pkg.data <- sequenceFilter(pkg.data, p, ymd)
    if (size.filter) pkg.data <- sizeFilter(pkg.data, p)
    pkg.data$date.time <- dateTime(pkg.data$date, pkg.data$time)
    pkg.data <- pkg.data[order(pkg.data$date.time), ]
    pkg.data$date.time <- NULL
    pkg.data
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
