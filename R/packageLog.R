#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param ip.filter Logical.
#' @param triplet.filter Logical.
#' @param small.filter Logical.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param clean.output Logical. NULL row names.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

packageLog <- function(packages = "cholera", date = NULL, ip.filter = TRUE,
  triplet.filter = TRUE, small.filter = TRUE, sequence.filter = TRUE,
  size.filter = TRUE, memoization = TRUE, check.package = TRUE,
  clean.output = FALSE, multi.core = TRUE) {

  cores <- multiCore(multi.core)

  if (check.package) packages <- checkPackage(packages)
  pkg.order <- packages

  ymd <- logDate(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)

  out <- lapply(packages, function(p) cran_log[cran_log$package == p, ])
  zero.downloads <- vapply(out, nrow, integer(1L))

  if (any(zero.downloads == 0)) {
    zero.sel <- zero.downloads == 0
    zero.packages <- packages[zero.sel]
    zero.out <- out[zero.sel]
    packages <- packages[!zero.sel]
    out <- out[!zero.sel]
  }

  if (ip.filter) {
    row.delete <- ipFilter(cran_log, multi.core = cores)
    out <- lapply(out, function(x) x[!row.names(x) %in% row.delete, ])
  }

  if (triplet.filter) {
    if (length(packages) == 1) {
      out <- lapply(out, tripletFilter)
    } else if (length(packages) > 1) {
      out <- parallel::mclapply(out, tripletFilter, mc.cores = cores)
    }
  }

  if (small.filter) lapply(out, smallFilter)

  if (sequence.filter) {
    arch.pkg.history <- parallel::mclapply(packages, function(x) {
      tmp <- packageHistory(x)
      tmp[tmp$Date <= ymd & tmp$Repository == "Archive", ]
    }, mc.cores = cores)

    out <- parallel::mclapply(seq_along(out), function(i) {
      sequenceFilter(out[[i]], arch.pkg.history[[i]])
    }, mc.cores = cores)
  }

  if (size.filter) out <- sizeFilter(out, packages)

  if (any(zero.downloads == 0)) {
    packages <- c(packages, zero.packages)
    out <- c(out, zero.out)
  }

  if (length(packages) == 1) {
    out <- out[[1]]
    if (nrow(out) != 0) {
      if (!"t2" %in% names(out)) out$t2 <- dateTime(out$date, out$time)
      out <- out[order(out$t2), ]
      out$t2 <- NULL
      if (clean.output) rownames(out) <- NULL
    }
  } else if (length(packages) > 1) {
    names(out) <- packages
    out <- parallel::mclapply(out, function(x) {
      if (!"t2" %in% names(x)) x$date.time <- dateTime(x$date, x$time)
      tmp <- x[order(x$date.time), ]
      tmp$date.time <- NULL
      tmp
    }, mc.cores = cores)

    out <- out[pkg.order]
  }

  out
}
