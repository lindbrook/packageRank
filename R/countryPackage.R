#' Tabulate a country's package downloads.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param country Character. country abbreviation.
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param ip.filter Logical.
#' @param triplet.filter Logical.
#' @param small.filter Logical.
#' @param sequence.filter Logical. Set to FALSE.
#' @param size.filter Logical. Set to FALSE.
#' @param sort Logical. Sort by download count.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @note "US" outlier 10 min with all filters!
#' @export

countryPackage <- function(country = "HK", date = NULL, ip.filter = TRUE,
  triplet.filter = TRUE, small.filter = TRUE, sequence.filter = FALSE,
  size.filter = FALSE, sort = TRUE, memoization = TRUE, multi.core = TRUE) {

  cores <- multiCore(multi.core)

  ymd <- logDate(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)

  if (ip.filter) {
    row.delete <- ipFilter(cran_log, multi.core = cores) # 93.221
    cran_log <- cran_log[!row.names(cran_log) %in% row.delete, ]
  }

  cran_log <- cran_log[!is.na(cran_log$country) & cran_log$country == country, ]
  pkgs <- unique(cran_log$package)

  if (triplet.filter) {
    out <- parallel::mclapply(pkgs, function(p) {
      tripletFilter(cran_log[cran_log$package == p, ], multi.core = FALSE)
    }, mc.cores = cores)
  }

  if (small.filter) out <- lapply(out, smallFilter)

  if (sequence.filter) {
    arch.pkg.history <- parallel::mclapply(pkgs, function(x) {
      tmp <- packageHistory(x)
      tmp[tmp$Date <= ymd & tmp$Repository == "Archive", ]
    }, mc.cores = cores)

    out <- parallel::mclapply(seq_along(out), function(i) {
      sequenceFilter(out[[i]], arch.pkg.history[[i]])
    }, mc.cores = cores)
  }

  if (size.filter) out <- sizeFilter(out, pkgs)

  out <- do.call(rbind, out)
  tab <- table(out$package)

  if (sort) out <- sort(tab, decreasing = TRUE)
  else out <- tab
  out
}
