#' Tabulate an IP's package downloads (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param ip Numeric. ip_id.
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @param small.filter Logical.
#' @param triplet.filter Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @note ip = 10 is a tw top-level domain on 2020-07-09.
#' @export

ipPackage <- function(ip = 10, date = Sys.Date() - 1, memoization = TRUE,
  sort = TRUE, small.filter = FALSE, triplet.filter = FALSE,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)

  if (triplet.filter) {
    tri.filtered <- parallel::mclapply(unique(cran_log$package), function(p) {
      x <- cran_log[cran_log$package == p, ]
      do.call(rbind, tripletFilter(x))
    }, mc.cores = cores)

    cran_log <- do.call(rbind, tri.filtered)
  }

  if (small.filter) cran_log <- smallFilter0(cran_log)

  freqtab <- table(cran_log$package)

  if (sort) {
    out <- sort(freqtab, decreasing = TRUE)
  } else {
    out <- freqtab
  }
  out
}
