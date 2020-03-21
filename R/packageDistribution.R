#' Package Download Distribution Plot.
#'
#' @param package Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd".
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.cran Logical. Check if package exists.
#' @param check.archive Logical. Include archive when validating package.
#' @export

packageDistribution <- function(package = "HistData", date = Sys.Date() - 1,
  memoization = TRUE, check.cran = FALSE, check.archive = FALSE) {

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

  crosstab <- sort(table(cran_log$package), decreasing = TRUE)
  cts <- sort(unique(crosstab))
  freq <- vapply(cts, function(x) sum(crosstab == x), integer(1L))
  freq.dist <- data.frame(count = cts, frequency = freq, row.names = NULL)

  out <- list(package = package, freq.dist = freq.dist, crosstab = crosstab)
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
  pkg.ct <- crosstab[names(crosstab) == x$package]
  pkg.bin <- crosstab[crosstab == pkg.ct]

  plot(freq.dist$count, freq.dist$frequency, type = "h", log = "x",
    xlab = "log10(Downloads)", ylab = "Frequency",
    main = paste(x$package, "+ Frequency Distribution of Package Downloads"))
  text(crosstab[1] * 0.75, mean(freq.dist$frequency) * 4,
    labels = names(crosstab[1]), col = "red")
  points(crosstab[1], 1, col = "red")
  points(1, length(crosstab[crosstab == 1]), col = "red")
  points(pkg.ct, length(pkg.bin) - which(names(pkg.bin) == x$package),
    col = "red", pch = 16)
  arrows(crosstab[1] * 0.75, mean(freq.dist$frequency) * 3,
    crosstab[1] * 0.9, mean(freq.dist$frequency) * 0.75, length = 0.05,
    col = "red")
  axis(3, at =  max(crosstab), labels = format( max(crosstab),
    big.mark = ","), cex.axis = 0.8, padj = 0.9, col.axis = "red",
    col.ticks = "red")
  axis(2, at = length(crosstab[crosstab == 1]),
    labels = length(crosstab[crosstab == 1]), las = 2, cex.axis = 0.8,
    col.axis = "red", col.ticks = "red")
  abline(v = pkg.ct, col = "red", lwd = 0.5)
  axis(3, at = pkg.ct, labels = format(pkg.ct, big.mark = ","),
    cex.axis = 0.8, padj = 0.9, col.axis = "red", col.ticks = "red")
}

#' Print method for packageDistribution().
#' @param x An object of class "packageDistribution" created by \code{packageDistribution()}
#' @param ... Additional parameters.
#' @export

print.packageDistribution <- function(x, ...) {
  print(x$freq.dist)
}
