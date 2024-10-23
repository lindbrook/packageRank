#' Filtered package downloads from the RStudio CRAN mirror (prototype).
#'
#' ip, small, sequence and size filters.
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param check.package Logical. Validate and "spell check" package.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

filteredDownloads <- function(packages = "HistData", date = NULL,
  all.filters = TRUE, ip.filter = FALSE, small.filter = FALSE, 
  sequence.filter = FALSE, size.filter = FALSE, check.package = TRUE, 
  memoization = TRUE, multi.core = FALSE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)

  cores <- multiCore(multi.core)
  if (.Platform$OS.type == "windows" & cores > 1) cores <- 1L

  if (check.package) packages <- checkPackage(packages)
  file.url.date <- logDate(date)
  ymd <- rev_fixDate_2012(file.url.date)

  cran_log <- packageLog(packages = packages, date = ymd,
    memoization = memoization, check.package = FALSE)

  if (is.data.frame(cran_log)) ct <- nrow(cran_log)
  else ct <- vapply(cran_log, nrow, integer(1L))
  
  individual.filter <- (ip.filter | small.filter | sequence.filter | 
                        size.filter) & all.filters
  
  if (individual.filter) all.filters <- FALSE

  f.cran_log <- packageLog(packages = packages, date = ymd,
    all.filters = all.filters, ip.filter = ip.filter,
    small.filter = small.filter, sequence.filter = sequence.filter, 
    size.filter = size.filter, memoization = memoization, check.package = FALSE)

  if (is.data.frame(f.cran_log)) f.ct <- nrow(f.cran_log)
  else f.ct <- vapply(f.cran_log, nrow, integer(1L))
  
  delta <- ct - f.ct
  inflation <- round(100 * delta / f.ct, 2)

  data.frame(date = ymd, package = packages, downloads = ct,
    filtered.downloads = f.ct, delta = delta, inflation = paste(inflation, "%"),
    row.names = NULL)
}
