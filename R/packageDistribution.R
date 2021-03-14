#' Package Download Distribution.
#'
#' @param package Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd".  NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param triplet.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

packageDistribution <- function(package = "HistData", date = NULL,
  all.filters = FALSE, ip.filter = FALSE, triplet.filter = FALSE,
  small.filter = FALSE, sequence.filter = FALSE, size.filter = FALSE,
  memoization = TRUE, check.package = TRUE, multi.core = TRUE) {

  if (check.package) packages <- checkPackage(package)
  ymd <- logDate(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  cores <- multiCore(multi.core)

  out <- package_distribution(package, ymd, all.filters, ip.filter,
    triplet.filter, small.filter, sequence.filter, size.filter, cran_log, cores)

  class(out) <- "packageDistribution"
  out
}

package_distribution <- function(package, ymd, all.filters, ip.filter,
  triplet.filter, small.filter, sequence.filter, size.filter, cran_log, cores) {

  # N.B. using pkg_specific_filters not recommended!
  if (all.filters) {
    ip.filter <- TRUE
    # triplet.filter <- TRUE
    small.filter <- TRUE
    # sequence.filter <- TRUE
    # size.filter <- TRUE
  }

  pkg_specific_filters <- c(triplet.filter, sequence.filter, size.filter)

  if (ip.filter) {
    row.delete <- ipFilter(cran_log, multi.core = cores)
    cran_log <- cran_log[!row.names(cran_log) %in% row.delete, ]
  }

  if (any(pkg_specific_filters)) {
    pkgs <- unique(cran_log$package)

    out <- parallel::mclapply(pkgs, function(p) {
      cran_log[cran_log$package == p, ]
    }, mc.cores = cores)

    if (triplet.filter) {
      out <- parallel::mclapply(out, tripletFilter, mc.cores = cores)
    }

    if (small.filter) {
      out <- parallel::mclapply(out, smallFilter, mc.cores = cores)
    }

    if (sequence.filter) {
      arch.pkg.history <- parallel::mclapply(pkgs, function(x) {
        tmp <- packageHistory(x)
        tmp[tmp$Date <= ymd & tmp$Repository == "Archive", ]
      }, mc.cores = cores)

      out <- parallel::mclapply(seq_along(out), function(i) {
        sequenceFilter(out[[i]], arch.pkg.history[[i]])
      }, mc.cores = cores)
    }

    if (size.filter) out <- sizeFilter(out, pkgs, cores)
    cran_log <- do.call(rbind, out)

  } else {
    if (small.filter) cran_log <- smallFilter(cran_log)
  }

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
      facet_wrap(~ package, nrow = 2) +
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
