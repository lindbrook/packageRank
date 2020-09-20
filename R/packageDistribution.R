#' Package Download Distribution.
#'
#' @param package Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd".
#' @param size.filter Logical. If Logical, TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @export

packageDistribution <- function(package = "HistData", date = Sys.Date() - 1,
  size.filter = FALSE, memoization = TRUE, check.package = TRUE,
  dev.mode = FALSE) {

  if (check.package) packages <- checkPackage(package, dev.mode)
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  out <- package_distribution(package, ymd, size.filter, memoization,
    check.package, cran_log)
  class(out) <- "packageDistribution"
  out
}

package_distribution <- function(package, ymd, size.filter, memoization,
  check.package, cran_log) {

  if (size.filter) cran_log <- smallFilter0(cran_log)
  freqtab <- sort(table(cran_log$package), decreasing = TRUE)
  cts <- sort(unique(freqtab))
  freq <- vapply(cts, function(x) sum(freqtab == x), integer(1L))
  freq.dist <- data.frame(count = cts, frequency = freq, row.names = NULL)
  out <- list(package = package, freq.dist = freq.dist, freqtab = freqtab,
    date = ymd)
}

#' Plot method for packageDistribution().
#' @param x An object of class "packageDistribution" created by \code{packageDistribution()}.
#' @param ... Additional plotting parameters.
#' @export

plot.packageDistribution <- function(x, ...) {
  if (length(x$package) <= 1) {
    plot_package_distribution(x)
  } else if (length(x$package > 1)) {
    # ggplot doesn't like integers
    dat2 <- data.frame(x = as.numeric(x$freq.dist$count),
                       y = as.numeric(x$freq.dist$frequency),
                       package = rep(x$package, each = nrow(x$freq.dist)))

    pkg.ct <- data.frame(package = names(x$freqtab), x = c(x$freqtab),
      stringsAsFactors = FALSE, row.names = NULL)

    freqtab <- as.data.frame(x$freqtab, stringsAsFactors = FALSE)
    names(freqtab) <- c("package", "count")

    pkg.ct <- freqtab[freqtab$package %in% x$package, ]
    p.data <- pkg.ct[pkg.ct$package %in% x$package, ]
    l.data <- data.frame(package = p.data$package, x = p.data$count,
      y = max(dat2$y))

    ggplot(data = dat2, aes_string("x", "y")) +
      geom_segment(aes_string(x = "x", xend = "x", y = 0, yend = "y"),
                   size = 1/3) +
      geom_vline(data = p.data, aes_string(xintercept = "count"),
                 colour = grDevices::adjustcolor("red", alpha.f = 2/3),
                 size = 0.5) +
      geom_label(data = l.data, aes_string("x", "y"),
                 fill = grDevices::adjustcolor("red", alpha.f = 2/3),
                 colour = "white", size = 2.75, label = pkg.ct$count) +
      scale_x_log10() +
      facet_wrap(~ package, ncol = 2) +
      xlab("Downloads") +
      ylab("Frequency") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  }
}

plot_package_distribution <- function(dat) {
  freq.dist <- dat$freq.dist
  xlim <- range(dat$freq.dist$count)
  ylim <- range(dat$freq.dist$frequency)
  freqtab <- dat$freqtab

  plot(freq.dist$count, freq.dist$frequency, type = "h", log = "x",
    xlab = "Downloads", ylab = "Frequency", xlim = xlim, ylim = ylim)
  if (!is.null(dat$package)) {
    pkg.ct <- freqtab[names(freqtab) == dat$package]
    if (pkg.ct > 10000) {
      axis(1, at = freqtab[1], cex.axis = 0.8, col.axis = "dodgerblue",
        col.ticks = "dodgerblue", labels = paste(names(freqtab[1]), "=",
        format(freqtab[1], big.mark = ",")))
    } else {
      axis(3, at = freqtab[1], cex.axis = 0.8, padj = 0.9,
        col.axis = "dodgerblue", col.ticks = "dodgerblue",
        labels = paste(names(freqtab[1]), "=",
        format(freqtab[1], big.mark = ",")))
    }
    abline(v = freqtab[1], col = "dodgerblue", lty = "dotted")
    axis(3, at = pkg.ct, labels = format(pkg.ct, big.mark = ","),
      cex.axis = 0.8, padj = 0.9, col.axis = "red", col.ticks = "red")
    abline(v = pkg.ct, col = grDevices::adjustcolor("red", alpha.f = 0.5))
    day <- weekdays(as.Date(dat$date), abbreviate = TRUE)
    title(paste0(dat$package, " @ ", dat$date, " (", day, ")"))
  } else {
    abline(v = freqtab[1], col = "dodgerblue", lty = "dotted")
    axis(3, at = freqtab[1], cex.axis = 0.8, padj = 0.9,
      col.axis = "dodgerblue", col.ticks = "dodgerblue",
      labels = paste(names(freqtab[1]), "=",
      format(freqtab[1], big.mark = ",")))
    day <- weekdays(as.Date(dat$date), abbreviate = TRUE)
    title(paste0("Package Download Counts", " @ ", dat$date, " (", day, ")"))
  }
}

#' Print method for packageDistribution().
#' @param x An object of class "packageDistribution" created by \code{packageDistribution()}
#' @param ... Additional parameters.
#' @export

print.packageDistribution <- function(x, ...) {
  print(x[c("package", "date")])
}
