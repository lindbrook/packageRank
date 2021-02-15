#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param triplet.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param clean.output Logical. NULL row names.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

packageLog <- function(packages = "cholera", date = NULL, all.filters = FALSE,
  ip.filter = FALSE, triplet.filter = FALSE, small.filter = FALSE,
  sequence.filter = FALSE, size.filter = FALSE, memoization = TRUE,
  check.package = TRUE, clean.output = FALSE, multi.core = TRUE) {

  if (check.package) packages <- checkPackage(packages)
  pkg.order <- packages

  ymd <- logDate(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)

  cores <- multiCore(multi.core)

  if (all.filters) {
    ip.filter <- TRUE
    triplet.filter <- TRUE
    small.filter <- TRUE
    sequence.filter <- TRUE
    size.filter <- TRUE
  }

  pkg_specific_filters <- c(triplet.filter, sequence.filter, size.filter)

  if (ip.filter) {
    row.delete <- ipFilter(cran_log, multi.core = cores)
    cran_log <- cran_log[!row.names(cran_log) %in% row.delete, ]
  }

  if (any(pkg_specific_filters)) {
    out <- parallel::mclapply(packages, function(p) {
      cran_log[cran_log$package == p, ]
    }, mc.cores = cores)

    if (triplet.filter) {
      out <- parallel::mclapply(out, tripletFilter, mc.cores = cores)
    }

    if (small.filter) {
      out <- parallel::mclapply(out, smallFilter, mc.cores = cores)
    }

    if (sequence.filter) {
      arch.pkg.history <- parallel::mclapply(packages, function(x) {
        tmp <- packageHistory(x)
        tmp[tmp$Date <= ymd & tmp$Repository == "Archive", ]
      }, mc.cores = cores)

      out <- parallel::mclapply(seq_along(out), function(i) {
        sequenceFilter(out[[i]], arch.pkg.history[[i]])
      }, mc.cores = cores)
    }

    if (size.filter) out <- sizeFilter(out, packages, cores)
    names(out) <- packages

  } else {
    if (small.filter) cran_log <- smallFilter(cran_log)
     out <- lapply(packages, function(p) cran_log[cran_log$package == p, ])
     names(out) <- packages
  }

  out <- parallel::mclapply(out, function(x) {
    if (!"t2" %in% names(x)) x$date.time <- dateTime(x$date, x$time)
    tmp <- x[order(x$date.time), ]
    tmp$date.time <- NULL
    tmp
  }, mc.cores = cores)

  if (length(packages) == 1) {
    out[[1]]
  } else {
    out[pkg.order]  
  }
}
