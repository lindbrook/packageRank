#' Package Download Distribution Plot.
#'
#' @param package Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd".
#' @param size.filter Logical or Numeric. If Logical, TRUE filters out downloads less than 1000 bytes. If Numeric, a postive value sets the minimum download size (in bytes) to consider; a negative value sets the maximum download size to consider.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Check if package exists.
#' @export

packageDistribution <- function(package = "HistData", date = Sys.Date() - 1,
  size.filter = FALSE, memoization = TRUE, check.package = TRUE) {

  if (check.package) {
    pkg.chk <- validatePackage2(package)
    if (is.list(pkg.chk)) {
      error <- paste(pkg.chk$invalid, collapse = ", ")
      if (length(pkg.chk$valid) == 0) {
        stop(error, ": misspelled or not on CRAN/Archive.")
      } else {
        warning(error, ": misspelled or not on CRAN/Archive.")
        package <- pkg.chk$valid
      }
    }
  }

  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cran_log[!is.na(cran_log$package), ]

  if (size.filter) {
    if (is.numeric(size.filter)) {
      if (size.filter >= 0) {
          cran_log <- cran_log[cran_log$size >= size.filter, ]
        } else if (size.filter < 0) {
          cran_log <- cran_log[cran_log$size < -size.filter, ]
        }
    } else if (is.logical(size.filter)) {
      cran_log <- cran_log[cran_log$size >= 1000, ]
    } else stop("'size.filter' must be Logical or Numeric.")
  }

  crosstab <- sort(table(cran_log$package), decreasing = TRUE)
  cts <- sort(unique(crosstab))
  freq <- vapply(cts, function(x) sum(crosstab == x), integer(1L))
  freq.dist <- data.frame(count = cts, frequency = freq, row.names = NULL)

  out <- list(package = package, freq.dist = freq.dist, crosstab = crosstab,
    date = ymd)
  class(out) <- "packageDistribution"
  out
}

#' Plot method for packageDistribution().
#' @param x An object of class "packageDistribution" created by \code{packageDistribution()}.
#' @param ... Additional plotting parameters.
#' @export

plot.packageDistribution <- function(x, ...) {
  freq.dist <- x$freq.dist
  crosstab <- x$crosstab

  plot(freq.dist$count, freq.dist$frequency, type = "h", log = "x",
    xlab = "Downloads", ylab = "Frequency")
  points(crosstab[1], 1, col = "dodgerblue")
  axis(3, at = crosstab[1], cex.axis = 0.8, padj = 0.9, col.axis = "dodgerblue",
    col.ticks = "dodgerblue", labels = paste(names(crosstab[1]), "=",
    format(crosstab[1], big.mark = ",")))
  abline(v = crosstab[1], col = "dodgerblue", lty = "dotted")

  if (!is.null(x$package)) {
    pkg.ct <- crosstab[names(crosstab) == x$package]
    pkg.bin <- crosstab[crosstab == pkg.ct]
    points(pkg.ct, length(pkg.bin) - which(names(pkg.bin) == x$package),
      col = "red", pch = 16)
    axis(3, at = pkg.ct, labels = format(pkg.ct, big.mark = ","),
      cex.axis = 0.8, padj = 0.9, col.axis = "red", col.ticks = "red")
    day <- weekdays(as.Date(x$date), abbreviate = TRUE)
    title(paste0(x$package, " @ ", x$date, " (", day, ")"))
  } else title(paste("Distribution of Package Download Counts:", x$date))
}

#' Print method for packageDistribution().
#' @param x An object of class "packageDistribution" created by \code{packageDistribution()}
#' @param ... Additional parameters.
#' @export

print.packageDistribution <- function(x, ...) {
  print(x$freq.dist)
}
