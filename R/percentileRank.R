#' Counts and rank percentiles (prototype).
#'
#' From Posit's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

percentileRank <- function(date = NULL, all.filters = FALSE, ip.filter = FALSE, 
  small.filter = FALSE, memoization = TRUE, multi.core = FALSE) {
  
  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)

  cores <- multiCore(multi.core)
  if (.Platform$OS.type == "windows" & cores > 1) cores <- 1L

  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  ymd <- rev_fixDate_2012(file.url.date)
  
  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
  }
  
  if (small.filter) cran_log <- smallFilter(cran_log)
  if (ip.filter) cran_log <- ipFilter(cran_log, multi.core = cores)
  
  freqtab <- sort(table(cran_log$package), decreasing = TRUE)
  
  pkg.data <- data.frame(pkg = names(freqtab), ct = c(freqtab), 
    row.names = NULL)
  pkg.data$n.rank <- seq_len(nrow(pkg.data))
  pkg.data$pct.rank <- vapply(pkg.data$ct, function(x) {
    100 * mean(pkg.data$ct < x)
  }, numeric(1L))
  
  rank.data <- sort(table(pkg.data$ct), decreasing = TRUE)
  rank.data <- data.frame(ct = as.numeric(names(rank.data)), 
                          freq = c(rank.data))
  rank.data <- rank.data[order(rank.data$ct, decreasing = TRUE), ]
  rank.data$freq <- NULL
  row.names(rank.data) <- NULL
  rank.data$t.rank <- seq_len(nrow(rank.data))
  
  pct.rank <- merge(pkg.data, rank.data, by = "ct", all.x = TRUE)
  pct.rank <- pct.rank[, c("pkg", "ct", "n.rank", "t.rank", "pct.rank")]
  pct.rank <- pct.rank[order(pct.rank$n.rank), ]
  row.names(pct.rank) <- NULL
  
  out <- list(date = file.url.date, data = pct.rank)
  class(out) <- "percentileRank"
  out
}

#' Plot method for percentileRank()..
#' @param x An object of class "percentileRank" created by \code{percentileRank()}.
#' @param type Character. "histogram" or "density".
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.percentileRank <- function(x, type = "histogram", ...) {
  ttl <- paste("Package Download Distribution @", x$date)
  xlab <-  "Log10 Count"
  if (type == "histogram") {
    graphics::hist(log10(x$data$ct), main = ttl, xlab = xlab)
  } else if (type == "density") {
    plot(stats::density(log10(x$data$ct)), main = ttl, xlab = xlab)
  } else stop('type must be "historgram" or "density"', call. = FALSE)
}

#' Count query.
#'
#' @param count Numeric or Integer.
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

queryCount <- function(count = 1, date = NULL, all.filters = FALSE, 
  ip.filter = FALSE, small.filter = FALSE, memoization = TRUE, 
  multi.core = FALSE) {

  x <- percentileRank(date = date, all.filters = all.filters, 
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)
  
  tmp <- x$data
  count.test <- any(tmp$ct == count)
  if (count.test) x$data[tmp$ct == count, ]
  else stop("Count not observed.", call. = FALSE)
}

#' Rank query.
#'
#' @param rank.num Numeric or Integer.
#' @param rank.tie Logical. TRUE uses ties. FALSE does not.
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

queryRank <- function(rank.num = 1, rank.tie = FALSE, date = NULL, 
  all.filters = FALSE, ip.filter = FALSE, small.filter = FALSE, 
  memoization = TRUE, multi.core = FALSE) {
  
  x <- percentileRank(date = date, all.filters = all.filters, 
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)
  
  tmp <- x$data
  tie <- ifelse(rank.tie, "t.rank",  "n.rank")
  rank.test <- any(tmp[, tie] == rank.num)
  if (rank.test) tmp[tmp[, tie] == rank.num, ]
  else stop("Rank not observed.", call. = FALSE)
}

#' Percentile-rank query.
#'
#' @param percentile Numeric.
#' @param lo Integer.
#' @param hi Integer
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

queryPercentile <- function(percentile = 50, lo = NULL, hi = NULL, 
  date = NULL, all.filters = FALSE, ip.filter = FALSE, small.filter = FALSE, 
  memoization = TRUE, multi.core = FALSE) {
  
  x <- percentileRank(date = date, all.filters = all.filters, 
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)

  tmp <- x$data
  
  if (!is.null(lo) & !is.null(hi)) {
    out <- tmp[round(tmp$pct.rank) >= lo & round(tmp$pct.rank) <= hi, ]
  } else if (is.null(lo) & !is.null(hi)) {
    out <- tmp[round(tmp$pct.rank) >= 0 & round(tmp$pct.rank) <= hi, ]
  } else if (!is.null(lo) & is.null(hi)) {
    out <- tmp[round(tmp$pct.rank) >= lo & round(tmp$pct.rank) <= 100, ]
  } else if (!is.null(percentile)) {
    if (percentile == 50) {
      out <- tmp[tmp$pct.rank == stats::median(tmp$pct.rank), ]
    } else {
      out <- tmp[round(tmp$pct.rank) == percentile, ]  
    }
  } 
  
  if (nrow(out) == 0) stop("Percentile{s} not observed.", call. = FALSE)
  else out
}
