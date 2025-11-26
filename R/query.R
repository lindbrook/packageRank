#' Query download count.
#'
#' @param count Numeric or Integer. whole number.
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

  x <- cranDistribution(date = date, all.filters = all.filters, 
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)

  count.test <- any(x$data$count %in% count)
  
  if (!count.test) {
    stop("Unobserved download count.", call. = FALSE)
  } else {
    out <- x$data[x$data$count %in% count, ]
  }
  rownames(out) <- NULL
  out
}

#' Query package name.
#'
#' @param packages Character..
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

queryPackage <- function(packages = "packageRank", date = NULL, 
  all.filters = FALSE, ip.filter = FALSE, small.filter = FALSE, 
  memoization = TRUE, check.package = TRUE, multi.core = FALSE) {
  
  if (check.package) packages <- checkPackage(packages)
  
  x <- cranDistribution(date = date, all.filters = all.filters,
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)

  unobs.pkgs <- !packages %in% x$data$package
  if (any(unobs.pkgs)) pkg.msg <- paste(packages[unobs.pkgs], collapse = ", ")

  if (all(unobs.pkgs)) {
    stop("No downloads for ", pkg.msg, " on ", x$date, ".", call. = FALSE)
  } else if (any(unobs.pkgs)) {
    message("No downloads for ", pkg.msg, " on ", x$date, ".")
    packages <- packages[!unobs.pkgs]
  }
  
  tmp <- x$data
  
  if (all(!packages %in% tmp$package)) {
    stop("Package(s) not observed. Check spelling.", call. = FALSE)
  } else if (any(!packages %in% tmp$package)) {
    message("No downloads for ", paste(packages[!packages %in% tmp$package], 
      collapse = ", "), ".")
    out <- tmp[tmp$package %in% packages, ]
  } else if (all(packages %in% tmp$package)) {
    out <- tmp[tmp$package %in% packages, ]
  }

  out <- out[match(packages, out$package), ]
  rownames(out) <- NULL
  out
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
  
  x <- cranDistribution(date = date, all.filters = all.filters, 
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)

  tmp <- x$data
  
  if (!is.null(lo) & !is.null(hi)) {
    if (lo > hi) stop("'lo' should be smaller than 'hi'", call. = FALSE)
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
  else {
    rownames(out) <- NULL
    out
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
  
  x <- cranDistribution(date = date, all.filters = all.filters, 
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)
  
  tmp <- x$data
  tie <- ifelse(rank.ties, "rank",  "nominal.rank")
  rank.test <- any(tmp[, tie] %in% num.rank)
  if (!rank.test) stop("Rank not observed.", call. = FALSE)
  else out <- tmp[tmp[, tie] %in% num.rank, ]
  rownames(out) <- NULL
  out
}
