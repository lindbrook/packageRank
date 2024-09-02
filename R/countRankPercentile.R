#' Counts, ranks and percentiles (prototype).
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

countRankPercentile <- function(date = NULL, all.filters = FALSE, 
  ip.filter = FALSE, small.filter = FALSE, memoization = TRUE, 
  multi.core = FALSE) {
  
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
  
  pkg.data <- data.frame(package = names(freqtab), count = c(freqtab), 
    row.names = NULL)
  pkg.data$n.rank <- seq_len(nrow(pkg.data))
  
  rnk <- rank(pkg.data$count, ties.method = "min")
  pkg.data$rank <- (max(rnk) + 1) - rnk
  
  pkg.data$percentile <- vapply(pkg.data$count, function(x) {
    100 * mean(pkg.data$count < x)
  }, numeric(1L))
  
  out <- list(date = file.url.date, data = pkg.data)
  class(out) <- "countRankPercentile"
  out
}

#' Plot method for countRankPercentile().
#' @param x An object of class "countRankPercentile" created by \code{countRankPercentile()}.
#' @param type Character. "histogram" or "density".
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.countRankPercentile <- function(x, type = "histogram", ...) {
  ttl <- paste("Package Download Distribution @", x$date)
  xlab <-  "Log10 Count"
  if (type == "histogram") {
    graphics::hist(log10(x$data$count), main = ttl, xlab = xlab)
  } else if (type == "density") {
    plot(stats::density(log10(x$data$count)), main = ttl, xlab = xlab)
  } else stop('type must be "historgram" or "density"', call. = FALSE)
}

#' Summary method for countRankPercentile().
#'
#' Five number summary of download count distribution
#' @param object An object of class "countRankPercentile" created by \code{countRankPercentile()}.
#' @param ... Additional plotting parameters.
#' @return A base R vector
#' @export

summary.countRankPercentile <- function(object, ...) {
  summary(object$data$count)
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

  x <- countRankPercentile(date = date, all.filters = all.filters, 
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)
  
  tmp <- x$data
  count.test <- any(tmp$count == count)
  if (count.test) x$data[tmp$count == count, ]
  else stop("Count not observed.", call. = FALSE)
}

#' Query package name.
#'
#' @param package Character..
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

queryPackage <- function(package = "packageRank", date = NULL, 
  all.filters = FALSE, ip.filter = FALSE, small.filter = FALSE, 
  memoization = TRUE, multi.core = FALSE) {
  
  x <- countRankPercentile(date = date, all.filters = all.filters, 
                           ip.filter = ip.filter, small.filter = small.filter, 
                           memoization = memoization, multi.core = multi.core)
  
  tmp <- x$data
  
  if (all(!package %in% tmp$package)) {
    stop("Package(s) not observed. Check spelling.", call. = FALSE)
  } else if (any(!package %in% tmp$package)) {
    message("No downloads for ", paste(package[!package %in% tmp$package], 
      collapse = ", "), ".")
    tmp[tmp$package %in% package, ]
  } else if (all(package %in% tmp$package)) {
    tmp[tmp$package %in% package, ]
  }
}

#' Rank query.
#'
#' @param num.rank Numeric or Integer.
#' @param rank.ties Logical. TRUE uses ties. FALSE does not.
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

queryRank <- function(num.rank = 1, rank.ties = FALSE, date = NULL, 
  all.filters = FALSE, ip.filter = FALSE, small.filter = FALSE, 
  memoization = TRUE, multi.core = FALSE) {
  
  x <- countRankPercentile(date = date, all.filters = all.filters, 
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)
  
  tmp <- x$data
  tie <- ifelse(rank.ties, "rank",  "n.rank")
  rank.test <- any(tmp[, tie] == num.rank)
  if (rank.test) tmp[tmp[, tie] == num.rank, ]
  else stop("Rank not observed.", call. = FALSE)
}

#' Percentile-rank query.
#'
#' @param percentile Numeric. 50 uses median().
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
  
  x <- countRankPercentile(date = date, all.filters = all.filters, 
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)

  tmp <- x$data
  
  if (!is.null(lo) & !is.null(hi)) {
    out <- tmp[round(tmp$percentile) >= lo & round(tmp$percentile) <= hi, ]
  } else if (is.null(lo) & !is.null(hi)) {
    out <- tmp[round(tmp$percentile) >= 0 & round(tmp$percentile) <= hi, ]
  } else if (!is.null(lo) & is.null(hi)) {
    out <- tmp[round(tmp$percentile) >= lo & round(tmp$percentile) <= 100, ]
  } else if (!is.null(percentile)) {
    if (percentile == 50) {
      out <- tmp[tmp$percentile == stats::median(tmp$percentile), ]
    } else {
      out <- tmp[round(tmp$percentile) == percentile, ]  
    }
  } 
  
  if (nrow(out) == 0) stop("Percentile(s) not observed.", call. = FALSE)
  else out
}
