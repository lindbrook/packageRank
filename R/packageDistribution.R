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
  cran_log <- cran_log[!is.na(cran_log$package), ]

  out <- package_distribution(package, ymd, size.filter, memoization,
    check.package, cran_log)
  class(out) <- "packageDistribution"
  out
}

package_distribution <- function(package, ymd, size.filter, memoization,
  check.package, cran_log) {

  if (size.filter) cran_log <- smallFilter(cran_log)

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
  if (length(x$package) <= 1) {
    plot_package_distribution(x)
  } else if (length(x$package > 1)) {
    pkg.data <- rep(x$package, each = nrow(x$freq.dist))
    dat2 <- data.frame(x$freq.dist, package = pkg.data,
      stringsAsFactors = FALSE)
    names(dat2)[1:2] <- c("x", "y")

    pkg.ct <- data.frame(package = names(x$crosstab), x = c(x$crosstab),
      stringsAsFactors = FALSE, row.names = NULL)

    crosstab <- as.data.frame(x$crosstab, stringsAsFactors = FALSE)
    names(crosstab) <- c("package", "count")

    pkg.ct <- crosstab[crosstab$package %in% x$package, ]

    p <- ggplot(data = dat2, aes_string("x", "y")) +
      geom_point(size = 0.5) +
      scale_x_log10() +
      facet_wrap(~ package, ncol = 2) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))

    for (i in seq_along(x$package)) {
      sel <- pkg.ct$package == x$package[i]
      p <- p + geom_vline(data = pkg.ct[sel, ], aes_string(xintercept = "count"),
        colour = "red", size = 0.25)
    }
    
    p
  }
}

plot_package_distribution <- function(dat) {
  freq.dist <- dat$freq.dist
  xlim <- range(dat$freq.dist$count)
  ylim <- range(dat$freq.dist$frequency)
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
  print(x[c("package", "date")])
}
