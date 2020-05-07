#' Package Download Distribution.
#'
#' @param package Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd".
#' @param size.filter Logical or Numeric. If Logical, TRUE filters out downloads less than 1000 bytes. If Numeric, a positive value sets the minimum download size (in bytes) to consider; a negative value sets the maximum download size to consider.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @export

packageDistribution <- function(package = "HistData", date = Sys.Date() - 1,
  size.filter = FALSE, memoization = TRUE, check.package = TRUE,
  dev.mode = FALSE) {

  if (check.package) {
    if (dev.mode) {
      pkg.chk <- validatePackage0(package)
    } else {
      pkg.chk <- validatePackage(package)
    }
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

  if (length(date) > 1) {
    out <- lapply(date, function(x) {
      package_distribution(package, x, size.filter, memoization, check.package)
    })
  } else if (length(date) == 1) {
    dat <- package_distribution(package, date, size.filter, memoization,
      check.package)
    out <- list(dat)
  }

  class(out) <- "packageDistribution"
  out
}

package_distribution <- function(package, date, size.filter, memoization,
  check.package) {

  date <- check10CharDate(date)
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
}

#' Plot method for packageDistribution().
#' @param x An object of class "packageDistribution" created by \code{packageDistribution()}.
#' @param ... Additional plotting parameters.
#' @export

plot.packageDistribution <- function(x, ...) {
  xlim <- range(lapply(x, function(z) z$freq.dist$count))
  ylim <- range(lapply(x, function(z) z$freq.dist$frequency))
  invisible(lapply(x, function(dat) {
    plot_package_distribution(dat, xlim, ylim)
  }))
}

plot_package_distribution <- function(dat, xlim, ylim) {
  freq.dist <- dat$freq.dist
  crosstab <- dat$crosstab

  plot(freq.dist$count, freq.dist$frequency, type = "h", log = "x",
    xlab = "Downloads", ylab = "Frequency", xlim = xlim, ylim = ylim)
  if (!is.null(dat$package)) {
    pkg.ct <- crosstab[names(crosstab) == dat$package]
    if (pkg.ct > 10000) {
      axis(1, at = crosstab[1], cex.axis = 0.8, col.axis = "dodgerblue",
        col.ticks = "dodgerblue", labels = paste(names(crosstab[1]), "=",
        format(crosstab[1], big.mark = ",")))
    } else {
      axis(3, at = crosstab[1], cex.axis = 0.8, padj = 0.9,
        col.axis = "dodgerblue", col.ticks = "dodgerblue",
        labels = paste(names(crosstab[1]), "=",
        format(crosstab[1], big.mark = ",")))
    }
    abline(v = crosstab[1], col = "dodgerblue", lty = "dotted")
    axis(3, at = pkg.ct, labels = format(pkg.ct, big.mark = ","),
      cex.axis = 0.8, padj = 0.9, col.axis = "red", col.ticks = "red")
    abline(v = pkg.ct, col = grDevices::adjustcolor("red", alpha.f = 0.5))
    day <- weekdays(as.Date(dat$date), abbreviate = TRUE)
    title(paste0(dat$package, " @ ", dat$date, " (", day, ")"))
  } else {
    abline(v = crosstab[1], col = "dodgerblue", lty = "dotted")
    axis(3, at = crosstab[1], cex.axis = 0.8, padj = 0.9,
      col.axis = "dodgerblue", col.ticks = "dodgerblue",
      labels = paste(names(crosstab[1]), "=",
      format(crosstab[1], big.mark = ",")))
    day <- weekdays(as.Date(dat$date), abbreviate = TRUE)
    title(paste0("Package Download Counts", " @ ", dat$date, " (", day, ")"))
  }
}

#' Print method for packageDistribution().
#' @param x An object of class "packageDistribution" created by \code{packageDistribution()}
#' @param ... Additional parameters.
#' @export

print.packageDistribution <- function(x, ...) {
  invisible(lapply(x, function(dat) print(dat[c("package", "date")])))
}
