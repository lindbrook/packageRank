#' Tabulate an IP's package downloads.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param ip Numeric. ip_id.
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param triplet.filter Logical.
#' @param small.filter Logical.
#' @param sort Logical. Sort by download count.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @note ip = 10 is a tw top-level domain on 2020-07-09.
#' @export

ipPackage <- function(ip = 10, date = NULL, memoization = TRUE,
  sort = TRUE, small.filter = FALSE, triplet.filter = FALSE,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)

  ymd <- logDate(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  cran_log <- cran_log[cran_log$ip_id == ip, ]

  if (triplet.filter) {
    filtered <- parallel::mclapply(unique(cran_log$package), function(p) {
      tripletFilter(cran_log[cran_log$package == p, ], multi.core = FALSE)
    }, mc.cores = cores)

    cran_log <- do.call(rbind, filtered)
  }

  if (small.filter) cran_log <- smallFilter(cran_log)
  freqtab <- table(cran_log$package)
  if (sort) sort(freqtab, decreasing = TRUE)
  else freqtab
}
