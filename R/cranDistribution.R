#' CRAN distribution (prototype).
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

cranDistribution <- function(date = NULL, all.filters = FALSE, 
  ip.filter = FALSE, small.filter = FALSE, memoization = TRUE, 
  multi.core = FALSE) {
  
  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  ymd <- rev_fixDate_2012(file.url.date)
  
  cores <- multiCore(multi.core)
  if (.Platform$OS.type == "windows" & cores > 1) cores <- 1L
  
  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
  }
  
  if (small.filter) cran_log <- smallFilter(cran_log)
  if (ip.filter) cran_log <- ipFilter(cran_log, multi.core = cores)

  freqtab <- sort(table(cran_log$package), decreasing = TRUE)
  
  pkg.data <- data.frame(package = names(freqtab), count = c(freqtab), 
    row.names = NULL)

  rnk <- rank(pkg.data$count, ties.method = "min")
  pkg.data$rank <- (max(rnk) + 1) - rnk
  pkg.data$nominal.rank <- seq_len(nrow(pkg.data))

  pkg.data$percentile <- unlist(parallel::mclapply(pkg.data$count, function(x) {
    round(100 * mean(pkg.data$count < x), 1)
  }, mc.cores = cores))
  
  out <- list(date = ymd, unique.packages = length(freqtab), data = pkg.data)
  class(out) <- "cranDistribution"
  out
}

#' Plot method for cranDistribution().
#' @param x An object of class "cranDistribution" created by \code{cranDistribution()}.
#' @param type Character. "histogram" or "count".
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.cranDistribution <- function(x, type = "count", ...) {
  day <- weekdays(as.Date(x$date), abbreviate = TRUE)
  ttl <- paste0("CRAN/Posit @ ", x$date, " (", day, ")")
  xlab <-  "Log10 Download Count"
  if (type == "histogram") {
    graphics::hist(log10(x$data$count), main = ttl, xlab = xlab)
  } else if (type == "count") {
    cts <- sort(unique(x$data$count))
    freq <- vapply(cts, function(ct) sum(x$data$count == ct), integer(1L))
    freq.dist <- data.frame(count = cts, frequency = freq, row.names = NULL)
    freq.density <- 100 * freq.dist$frequency / sum(freq.dist$frequency)
    xlim <- range(log10(freq.dist$count))
    ylim <- range(freq.density)
    plot(log10(freq.dist$count), freq.density, type = "h", main = ttl,
      xlab = xlab, ylab = "Percent", xlim = xlim, ylim = ylim)
    avg <- mean(x$data$count)
    avg.lab <- paste("avg =", round(avg, 1))
    med <- stats::median(x$data$count)
    med.lab <- paste("med =", round(med, 1))
    max <- max(x$data$count)
    max.lab <- paste("max =", format(max, big.mark = ","))
    axis(3, at = log10(avg), cex.axis = 0.8, padj = 0.9, labels = avg.lab, 
      col.axis = "blue", col.ticks = "blue")
    axis(3, at = log10(med), cex.axis = 0.8, padj = 0.9, labels = med.lab, 
      col.axis = "red", col.ticks = "red")
    axis(3, at = log10(max), cex.axis = 0.8, padj = 0.9, labels = max.lab)
  } else stop('type must be "historgram" or "count"', call. = FALSE)
  title(sub = paste(
    format(sum(x$data$count), big.mark = ","), "total downloads;",
    format(x$unique.packages, big.mark = ","), "unique packages"), 
    cex.sub = 0.9)
}

#' Print method for cranDistribution().
#' @param x object.
#' @param top.n Numeric or Integer.
#' @param ... Additional parameters.
#' @export

print.cranDistribution <- function(x, top.n = 20, ...) {
  pkg.ct <- format(x$unique.packages, big.mark = ",")
  dwnld.ct <- format(sum(x$data$count), big.mark = ",")
  print(list(date = paste(x$date, weekdays(x$date)),
             unique.packages.downloaded = pkg.ct,
             total.downloads = dwnld.ct,
             top.n = head(x$data, top.n)))
}

#' Summary method for cranDistribution().
#'
#' Five number (+ mean) summary of download count distribution
#' @param object An object of class "cranDistribution" created by \code{cranDistribution()}.
#' @param ... Additional plotting parameters.
#' @return A base R vector
#' @export

summary.cranDistribution <- function(object, ...) {
  list(unique.packages.downloaded = object$unique.packages,
       total.downloads = sum(object$data$count),
       download.summary = summary(object$data$count))
}

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
