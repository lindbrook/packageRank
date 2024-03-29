#' Tabulate package downloads by country.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

countryDistribution <- function(date = NULL, all.filters = FALSE,
  ip.filter = FALSE, small.filter = FALSE, sequence.filter = FALSE, 
  size.filter = FALSE, memoization = TRUE, multi.core = FALSE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)

  cores <- multiCore(multi.core)
  if (.Platform$OS.type == "windows" & cores > 1) cores <- 1L

  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)

  ymd <- rev_fixDate_2012(file.url.date)
  
  na.country <- is.na(cran_log$country)
  cran_log <- cran_log[!na.country, ]

  # N.B. sizeFilter() and sequence.filter() not implemented!
  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
    # sequence.filter <- TRUE
    # size.filter <- TRUE
  }

  if (small.filter) cran_log <- smallFilter(cran_log)
  if (ip.filter) cran_log <- ipFilter(cran_log, multi.core = cores)

  freqtab <- sort(table(cran_log$country), decreasing = TRUE)
  out <- list(date = ymd, na.country = na.country, data = freqtab)
  class(out) <- "countryDistribution"
  out
}

#' Plot top 10 package downloads by country domain.
#'
#' Plot method for countryDistribution().
#' @param x An object of class "countryDistribution" created by \code{countryDistribution()}.
#' @param N Integer. Top N countries.
#' @param ... Additional plotting parameters.
#' @export

plot.countryDistribution <- function(x, N = 10, ...) {
  ct <- x$data / 10^6
  barplot(ct[1:N], ylab = "Downloads (Millions)")
  title(main = paste("Top", N, "Country Domains @", x$date),
        sub = paste0("NAs = ", round(100 * mean(x$na.country), 1), "%"))
}

#' Print method for countryDistribution().
#' @param x object.
#' @param N Integer. Top N countries.
#' @param ... Additional parameters.
#' @importFrom utils head
#' @export

print.countryDistribution <- function(x, N = 10, ...) {
  print(utils::head(x$data, N))
}