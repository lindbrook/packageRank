#' Tabulate an IP's package downloads.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param ip Numeric. ip_id. Positive integer.
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param sort.count Logical. Sort by download count.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @note ip = 10 is a tw top-level domain on 2020-07-09.
#' @export

ipPackage <- function(ip = 10, date = NULL, all.filters = FALSE,
  ip.filter = FALSE, small.filter = FALSE, sequence.filter = FALSE, 
  size.filter = FALSE, sort.count = TRUE, memoization = TRUE, 
  multi.core = FALSE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  if (ip < 1) stop("Invalid IP. Use a positive integer.")

  cores <- multiCore(multi.core)
  if (.Platform$OS.type == "windows" & cores > 1) cores <- 1L

  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  
  if (!ip %in% cran_log$ip_id) {
    stop("Valid IPs: ", min(cran_log$ip_id), " to ", max(cran_log$ip_id))
  }

  cran_log <- cleanLog(cran_log)
  if (!ip %in% cran_log$ip_id) stop("IP filtered out or not found.")

  ymd <- rev_fixDate_2012(file.url.date)
  
  if (all.filters) {
    ip.filter = TRUE
    small.filter = TRUE
    sequence.filter = TRUE
    size.filter = TRUE
  }
  
  if (ip.filter) cran_log <- ipFilter(cran_log, multi.core = cores)
  
  if (!ip %in% cran_log$ip_id) {
    stop("IP filtered out.")
  } else {
    cran_log <- cran_log[cran_log$ip_id == ip, ]
    pkgs <- unique(cran_log$package)
    
    out <- parallel::mclapply(pkgs, function(p) {
      pkg.data <- cran_log[cran_log$package == p, ]
      if (small.filter) pkg.data <- smallFilter(pkg.data)
      
      pkg.data$date.time <- dateTime(pkg.data$date, pkg.data$time) 
      if (sequence.filter) pkg.data <- sequenceFilter(pkg.data, p, ymd)  
      
      if (size.filter) pkg.data <- sizeFilter(pkg.data, p)
      pkg.data <- pkg.data[order(pkg.data$date.time), ]
      pkg.data$date.time <- NULL
      pkg.data
    }, mc.cores = cores)
    
    cran_log <- do.call(rbind, out)
  }
  
 if (nrow(cran_log) > 0) {
    freqtab <- table(cran_log$package)
    if (sort.count) sort(freqtab, decreasing = TRUE)
    else freqtab
  } else stop("IP filtered out.")
}
