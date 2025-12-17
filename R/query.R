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
#' @param package Character..
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

queryPackage <- function(package = "packageRank", date = NULL, 
  all.filters = FALSE, ip.filter = FALSE, small.filter = FALSE, 
  memoization = TRUE, check.package = TRUE, multi.core = FALSE) {
  
  if (check.package) package <- checkPackage(package)
  
  x <- cranDistribution(date = date, all.filters = all.filters,
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)

  unobs.pkgs <- !package %in% x$data$package
  if (any(unobs.pkgs)) pkg.msg <- paste(package[unobs.pkgs], collapse = ", ")

  if (all(unobs.pkgs)) {
    stop("No downloads for ", pkg.msg, " on ", x$date, ".", call. = FALSE)
  } else if (any(unobs.pkgs)) {
    message("No downloads for ", pkg.msg, " on ", x$date, ".")
    package <- package[!unobs.pkgs]
  }
  
  tmp <- x$data
  
  if (all(!package %in% tmp$package)) {
    stop("Package(s) not observed. Check spelling.", call. = FALSE)
  } else if (any(!package %in% tmp$package)) {
    message("No downloads for ", paste(package[!package %in% tmp$package], 
      collapse = ", "), ".")
    out <- tmp[tmp$package %in% package, ]
  } else if (all(package %in% tmp$package)) {
    out <- tmp[tmp$package %in% package, ]
  }

  out <- out[match(package, out$package), ]
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
  
  if (percentile < 0 | percentile > 100) {
    stop('percentile must be in [0, 100].', call. = FALSE)
  }

  if (!is.null(lo)) {
    if (lo < 0 | lo > 100) stop('lo must be in [0, 100].', call. = FALSE)
  }
  
  if (!is.null(hi)) {
    if (hi < 0 | hi > 100) stop('hi must be in [0, 100].', call. = FALSE)
  }

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
#' @param rank Numeric or Integer.
#' @param rank.ties Logical. TRUE uses ties. FALSE does not.
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

queryRank <- function(rank = 1, rank.ties = FALSE, date = NULL, 
  all.filters = FALSE, ip.filter = FALSE, small.filter = FALSE, 
  memoization = TRUE, multi.core = FALSE) {
  
  if (rank < 1) stop("rank must be > 0.", call. = FALSE)

  x <- cranDistribution(date = date, all.filters = all.filters, 
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)
  
  if (rank > nrow(x$data)) {
    stop("rank must be in [1, ", nrow(x$data), "].", call. = FALSE)
  }

  tmp <- x$data
  tie <- ifelse(rank.ties, "rank",  "nominal.rank")
  rank.test <- any(tmp[, tie] %in% rank)
  if (!rank.test) stop("Rank not observed.", call. = FALSE)
  else out <- tmp[tmp[, tie] %in% rank, ]
  rownames(out) <- NULL
  out
}
