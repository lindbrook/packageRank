#' Tabulate an IP's package downloads.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param ip Numeric. ip_id.
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param triplet.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param sort Logical. Sort by download count.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @note ip = 10 is a tw top-level domain on 2020-07-09.
#' @export

ipPackage <- function(ip = 10, date = NULL, all.filters = FALSE,
  ip.filter = FALSE, triplet.filter = FALSE, small.filter = FALSE,
  sequence.filter = FALSE, size.filter = FALSE, sort = TRUE, memoization = TRUE,
  multi.core = TRUE) {

  ymd <- logDate(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)

  cores <- multiCore(multi.core)

  # N.B. using pkg_specific_filters not recommended!
  if (all.filters) {
    ip.filter <- TRUE
    # triplet.filter <- TRUE
    small.filter <- TRUE
    # sequence.filter <- TRUE
    # size.filter <- TRUE
  }

  pkg_specific_filters <- c(triplet.filter, sequence.filter, size.filter)

  if (ip.filter) {
    row.delete <- ipFilter(cran_log, multi.core = cores)
    cran_log <- cran_log[!row.names(cran_log) %in% row.delete, ]
  }

  if (any(pkg_specific_filters)) {
    pkgs <- unique(cran_log$package)

    out <- parallel::mclapply(pkgs, function(p) {
      cran_log[cran_log$package == p, ]
    }, mc.cores = cores)

    if (triplet.filter) {
      out <- parallel::mclapply(out, tripletFilter, mc.cores = cores)
    }

    if (small.filter) {
      out <- parallel::mclapply(out, smallFilter, mc.cores = cores)
    }

    if (sequence.filter) {
      arch.pkg.history <- parallel::mclapply(pkgs, function(x) {
        tmp <- packageHistory(x)
        tmp[tmp$Date <= ymd & tmp$Repository == "Archive", ]
      }, mc.cores = cores)

      out <- parallel::mclapply(seq_along(out), function(i) {
        sequenceFilter(out[[i]], arch.pkg.history[[i]])
      }, mc.cores = cores)
    }

    if (size.filter) out <- sizeFilter(out, pkgs, cores)
    cran_log <- do.call(rbind, out)

  } else {
    if (small.filter) cran_log <- smallFilter(cran_log)
  }

  cran_log <- cran_log[cran_log$ip_id == ip, ]
  freqtab <- table(cran_log$package)
  if (sort) sort(freqtab, decreasing = TRUE)
  else freqtab
}
