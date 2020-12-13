#' Tabulate a country's package downloads.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param country Character. country abbreviation.
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @param ip.filter Logical.
#' @param ip.campaigns Logical.
#' @param triplet.filter Logical.
#' @param small.filter Logical.
#' @param sequence.filter Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @note "US" outlier 10 min with all filters!
#' @export

countryPackage <- function(country = "HK", date = Sys.Date() - 1,
  memoization = TRUE, sort = TRUE, triplet.filter = TRUE, ip.filter = TRUE,
  ip.campaigns = TRUE, small.filter = TRUE, sequence.filter = FALSE,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)

  if (ip.filter) {
    row.delete <- ipFilter(cran_log, campaigns = ip.campaigns,
      multi.core = cores)
    cran_log <- cran_log[!row.names(cran_log) %in% row.delete, ]
  }

  cran_log <- cran_log[!is.na(cran_log$country) & cran_log$country == country, ]

  if (triplet.filter) {
    out <- parallel::mclapply(unique(cran_log$package), function(p) {
      tripletFilter(cran_log[cran_log$package == p, ], multi.core = FALSE)
    }, mc.cores = cores)
  }

  if (small.filter) {
    out <- parallel::mclapply(out, smallFilter0, mc.cores = cores)
  }

  if (sequence.filter) {
    out <- parallel::mclapply(out, sequenceFilter, mc.cores = cores)
  }

  out <- do.call(rbind, out)
  tab <- table(out$package)

  if (sort) out <- sort(tab, decreasing = TRUE)
  else out <- tab
  out
}
