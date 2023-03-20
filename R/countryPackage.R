#' Tabulate a country's package downloads.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param country Character. country abbreviation.
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param triplet.filter Logical.
#' @param small.filter Logical.
#' @param sequence.filter Logical. Set to FALSE.
#' @param size.filter Logical. Set to FALSE.
#' @param sort Logical. Sort by download count.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @note "US" outlier 10 min with all filters!
#' @export

countryPackage <- function(country = "HK", date = NULL, all.filters = FALSE,
  ip.filter = FALSE, triplet.filter = FALSE, small.filter = FALSE,
  sequence.filter = FALSE, size.filter = FALSE, sort = TRUE,
  memoization = TRUE, multi.core = TRUE, dev.mode = FALSE) {

  country <- toupper(country)
  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  cran_log <- cran_log[!is.na(cran_log$country) & cran_log$country == country, ]

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
    row.delete <- ipFilter(cran_log, multi.core = cores, dev.mode = dev.mode)
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
      out <- smallFilter(out, multi.core = cores, dev.mode = dev.mode)
    }

    if (sequence.filter) {
      ymd <- rev_fixDate_2012(file.url.date)
      
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
    if (small.filter) cran_log <- cran_log[cran_log$size >= 1000L, ]
  }

  freqtab <- table(cran_log$package)

  if (sort) sort(freqtab, decreasing = TRUE)
  else freqtab
}
