#' Tabulate package downloads by country.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. yyyy-mm-dd.
#' @param ip.filter Logical.
#' @param small.filter Logical TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

countryDistribution <- function(date = Sys.Date() - 1, ip.filter = TRUE,
  small.filter = TRUE, memoization = TRUE, multi.core = TRUE) {

  cores <- multiCore(multi.core)
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  na.country <- is.na(cran_log$country)
  cran_log <- cran_log[!na.country, ]

  if (ip.filter) {
    row.delete <- campaigns2(cran_log, multi.core = cores)
    cran_log <- cran_log[!row.names(cran_log) %in% row.delete, ]
  }

  if (small.filter) cran_log <- smallFilter0(cran_log)

  freqtab <- sort(table(cran_log$country), decreasing = TRUE)
  out <- list(date = date, na.country = na.country, data = freqtab)
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
        sub = paste0("NAs = ", round(100 * mean(out$na.country), 1), "%"))
}
