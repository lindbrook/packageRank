#' Tabulate package downloads by country.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param triplet.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @return An R data frame.
#' @export

countryDistribution <- function(date = NULL, all.filters = FALSE,
  ip.filter = FALSE, triplet.filter = FALSE, small.filter = FALSE,
  sequence.filter = FALSE, size.filter = FALSE, memoization = TRUE,
  multi.core = TRUE, dev.mode = FALSE) {

  cores <- multiCore(multi.core)
  ymd <- logDate(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  na.country <- is.na(cran_log$country)
  cran_log <- cran_log[!na.country, ]

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

  freqtab <- sort(table(cran_log$country), decreasing = TRUE)
  out <- list(date = ymd, na.country = na.country, data = freqtab)
  class(out) <- "countryDistribution"
  out
}

#' Plot top 10 package downloads by country domain.
#'
#' Plot method for packageDistribution().
#' @param x An object of class "countryDistribution" created by \code{countryDistribution()}.
#' @param ... Additional plotting parameters.
#' @export

plot.countryDistribution <- function(x, ...) {
  ct <- x$data / 10^6
  barplot(ct[1:10], ylab = "Downloads (Millions)")
  title(main = paste("Top Ten Country Domains @", x$date),
        sub = paste0("NAs = ", round(100 * mean(x$na.country), 1), "%"))
}
