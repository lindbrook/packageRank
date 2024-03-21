#' Tabulate a country's package downloads.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param country Character. country abbreviation.
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param sequence.filter Logical. Set to FALSE.
#' @param size.filter Logical. Set to FALSE.
#' @param sort.count Logical. Sort by download count.
#' @param memoization Logical. Use memoization when downloading logs.
#' @note "US" outlier 10 min with all filters!
#' @export

countryPackage <- function(country = "HK", date = NULL, all.filters = FALSE,
  ip.filter = FALSE, small.filter = FALSE, sequence.filter = FALSE, 
  size.filter = FALSE, sort.count = TRUE, memoization = TRUE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  
  # cores <- multiCore(multi.core)
  # if (.Platform$OS.type == "windows" & cores > 1) cores <- 1L

  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  
  country <- toupper(country)

  if (!country %in% cran_log$country) {
    stop("Country code filtered out or not found. Check spelling.")
  }

  cran_log <- cleanLog(cran_log)

  # additional filter to remove country NAs 
  cran_log <- cran_log[cran_log$country == country &!is.na(cran_log$country), ]
  
  if (!country %in% cran_log$country) stop("Country code filtered out.")

  # N.B. sizeFilter() and sequence.filter() not implemented!
  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
    # sequence.filter <- TRUE
    # size.filter <- TRUE
  }

  if (small.filter) cran_log <- smallFilter(cran_log)
  if (ip.filter) cran_log <- ipFilter(cran_log)
  
  if (!country %in% cran_log$country) {
    stop("Country code filtered out.")
  } else {
    freqtab <- table(cran_log$package)
    if (sort.count) sort(freqtab, decreasing = TRUE)
    else freqtab
  }
}
