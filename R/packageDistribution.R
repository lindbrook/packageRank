#' Package Download Distribution.
#'
#' @param package Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd".  NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

packageDistribution <- function(package = "HistData", date = NULL,
  all.filters = FALSE, ip.filter = FALSE, small.filter = FALSE,
  memoization = TRUE, check.package = TRUE, multi.core = FALSE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  if (check.package) packages <- checkPackage(package)
  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  ymd <- rev_fixDate_2012(file.url.date)

  if (all(!package %in% cran_log$package)) {
    stop('No downloads for ', package, ' on ', ymd, ".", call. = FALSE)
  } else {
    cran_log <- cleanLog(cran_log)
    cores <- multiCore(multi.core)
    ymd <- rev_fixDate_2012(file.url.date)
    out <- package_distribution(package, ymd, all.filters, ip.filter,
      small.filter, cran_log, cores)
    class(out) <- "packageDistribution"
    out
  }
}

package_distribution <- function(package, ymd, all.filters, ip.filter,
  small.filter, cran_log, cores) {

  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
  }

  if (ip.filter) cran_log <- ipFilter(cran_log, multi.core = cores)
  if (small.filter) cran_log <- smallFilter(cran_log)
  
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

    geom.col <- grDevices::adjustcolor("red", alpha.f = 2/3)

    ggplot2::ggplot(data = dat2, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_segment(linewidth = 1/3,
        ggplot2::aes(x = .data$x, xend = .data$x, y = 0, yend = .data$y)) +
      ggplot2::geom_vline(data = p.data, colour = "red", linetype = "dotted",
        size = 0.5, ggplot2::aes(xintercept = .data$count)) +
      ggplot2::geom_label(data = l.data, aes(x = .data$x, y = .data$y),
        fill = geom.col, colour = "white", size = 2.75, label = pkg.ct$count) +
      ggplot2::scale_x_log10() +
      ggplot2::facet_wrap(ggplot2::vars(.data$package), nrow = 2) +
      ggplot2::xlab("Downloads") +
      ggplot2::ylab("Frequency") +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank())
  }
}

plot_package_distribution <- function(dat) {
  freq.dist <- dat$freq.dist
  xlim <- range(log10(dat$freq.dist$count))
  ylim <- range(dat$freq.dist$frequency)
  freqtab <- dat$freqtab

  plot(log10(freq.dist$count), freq.dist$frequency, type = "h", 
    xlab = "Log10 Download Count", ylab = "Frequency", xlim = xlim, ylim = ylim)
  if (!is.null(dat$package)) {
    pkg.ct <- freqtab[names(freqtab) == dat$package]
    if (pkg.ct > 10000) {
      axis(1, at = log10(freqtab[1]), cex.axis = 0.8, col.axis = "dodgerblue",
        col.ticks = "dodgerblue", labels = paste(names(freqtab[1]), "=",
        format(freqtab[1], big.mark = ",")))
    } else {
      axis(3, at = log10(freqtab[1]), cex.axis = 0.8, padj = 0.9,
        col.axis = "dodgerblue", col.ticks = "dodgerblue",
        labels = paste(names(freqtab[1]), "=",
        format(freqtab[1], big.mark = ",")))
    }
    abline(v = log10(freqtab[1]), col = "dodgerblue", lty = "dotted")
    axis(3, at = log10(pkg.ct), labels = format(pkg.ct, big.mark = ","),
      cex.axis = 0.8, padj = 0.9, col.axis = "red", col.ticks = "red")
    abline(v = log10(pkg.ct), col = "red", lty = "dotted")
    day <- weekdays(as.Date(dat$date), abbreviate = TRUE)
    title(paste0(dat$package, " @ ", dat$date, " (", day, ")"))
  } else {
    abline(v = log10(freqtab[1]), col = "dodgerblue", lty = "dotted")
    axis(3, at = log10(freqtab[1]), cex.axis = 0.8, padj = 0.9,
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
