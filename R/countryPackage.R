#' Tabulate a country's package downloads.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param country Character. country abbreviation.
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param version.filter Logical. TRUE selects only most recent version.
#' @param sort.count Logical. Sort by download count.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @note all.filters = TRUE only enables IP and small filters. "US" outlier 10 min with all filters!
#' @export

countryPackage <- function(country = "HK", date = NULL, all.filters = FALSE,
  ip.filter = FALSE, sequence.filter = FALSE, size.filter = FALSE, 
  small.filter = FALSE, version.filter = FALSE, sort.count = TRUE,
  memoization = TRUE, multi.core = FALSE) {

  if (.Platform$OS.type == "windows") {
    cores <- 1L
  } else {
    cores <- multiCore(multi.core)
  }

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  
  country <- toupper(country)

  if (!country %in% cran_log$country) {
    stop("Country code filtered out or not found. Check spelling.")
  }

  cran_log <- cleanLog(cran_log)
  ymd <- rev_fixDate_2012(file.url.date)

  # additional filter to remove country NAs 
  cran_log <- cran_log[cran_log$country == country &!is.na(cran_log$country), ]
  
  if (!country %in% cran_log$country) stop("Country code filtered out.")

  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
  }

  if (small.filter) cran_log <- smallFilter(cran_log)
  if (ip.filter) cran_log <- ipFilter(cran_log)

  if (sequence.filter | size.filter | version.filter) {
    out <- parallel::mclapply(cran_log$package, function(p) {
      p.dat <- cran_log[cran_log$package == p, ]
      p.dat$date.time <- dateTime(p.dat$date, p.dat$time)
      if (sequence.filter) p.dat <- sequenceFilter(p.dat, p, ymd)
      if (size.filter) p.dat <- sizeFilter(p.dat, p)
      if (version.filter) p.dat <- versionFilter(p.dat, p, ymd)
      p.dat <- p.dat[order(p.dat$date.time), ]
      p.dat$date.time <- NULL
    }, mc.cores = cores)
    cran_log <- do.call(rbind, out)  
  }

  if (!country %in% cran_log$country) {
    stop("Country code filtered out.")
  } else {
    freqtab <- table(cran_log$package)
    if (sort.count) sort(freqtab, decreasing = TRUE)
    else freqtab
  }
}
