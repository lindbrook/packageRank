#' Package Download Distribution Plot.
#'
#' @param package Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd".
#' @param size.filter Logical or Numeric. If Logical, TRUE filters out downloads less than 1000 bytes. If Numeric, a postive value sets the minimum download size (in bytes) to consider; a negative value sets the maximum download size to consider.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.cran Logical. Check if package exists.
#' @param check.archive Logical. Include archive when validating package.
#' @export

packageDistribution <- function(package = "HistData", date = Sys.Date() - 1,
  size.filter = FALSE, memoization = TRUE, check.cran = FALSE,
  check.archive = FALSE) {

  if (check.cran) {
    pkg.chk <- validatePackage(package, check.archive = check.archive)
    if (is.list(pkg.chk)) {
      error <- paste(pkg.chk$invalid, collapse = ", ")
      if (length(pkg.chk$valid) == 0) {
        if (check.archive) {
          stop(error, ": misspelled or not on CRAN/Archive.")
        } else stop(error, ": misspelled or not on CRAN.")
      } else {
        if (check.archive) {
          warning(error, ": misspelled or not on CRAN/Archive.")
        } else warning(error, ": misspelled or not on CRAN.")
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
  points(crosstab[1], 1, col = "red")
  points(1, length(crosstab[crosstab == 1]), col = "red")
  axis(3, at = crosstab[1], cex.axis = 0.8, padj = 0.9, col.axis = "red",
    col.ticks = "red", labels = paste(names(crosstab[1]), "=",
    format(crosstab[1], big.mark = ",")))
  axis(2, at = length(crosstab[crosstab == 1]),
    labels = length(crosstab[crosstab == 1]), las = 2, cex.axis = 0.8,
    col.axis = "red", col.ticks = "red")
  abline(v = crosstab[1], col = "red", lty = "dotted")

  if (!is.null(x$package)) {
    pkg.ct <- crosstab[names(crosstab) == x$package]
    pkg.bin <- crosstab[crosstab == pkg.ct]
    points(pkg.ct, length(pkg.bin) - which(names(pkg.bin) == x$package),
      col = "red", pch = 16)
    abline(v = pkg.ct, col = "red", lwd = 0.5)
    axis(3, at = pkg.ct, labels = format(pkg.ct, big.mark = ","),
      cex.axis = 0.8, padj = 0.9, col.axis = "red", col.ticks = "red")
    title(paste(x$package, "@", x$date))
  } else title(paste("Distribution of Package Download Counts:", x$date))
}

#' Print method for packageDistribution().
#' @param x An object of class "packageDistribution" created by \code{packageDistribution()}
#' @param ... Additional parameters.
#' @export

print.packageDistribution <- function(x, ...) {
  print(x$freq.dist)
}
