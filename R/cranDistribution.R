#' CRAN distribution (prototype).
#'
#' From Posit's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s). NULL for all downloads, all of CRAN
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

cranDistribution <- function(packages = NULL, date = NULL, all.filters = FALSE, 
  ip.filter = FALSE, small.filter = FALSE, memoization = TRUE, 
  check.package = TRUE, multi.core = FALSE) {
  
  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  
  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  ymd <- rev_fixDate_2012(file.url.date)

   if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
  }

  cores <- multiCore(multi.core)
  if (.Platform$OS.type == "windows" & cores > 1) cores <- 1L

  if (!is.null(packages)) {
    if (check.package) packages <- checkPackage(packages)

    if (all(!packages %in% cran_log$package)) {
      stop('No downloads for ', packages, ' on ', ymd, ".", call. = FALSE)
    } else {
      out <- package_distribution(packages, ymd, all.filters, ip.filter,
        small.filter, cran_log, cores)
      class(out) <- "packageDistribution"
    }
  } else {
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
  }
  out
}

package_distribution <- function(packages, ymd, all.filters, ip.filter,
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
  out <- list(package = packages, freq.dist = freq.dist, freqtab = freqtab,
    date = ymd)
}

#' Plot method for packageDistribution().
#' @param x An object of class "packageDistribution" created by \code{packageDistribution()}.
#' @param ... Additional plotting parameters.
#' @importFrom ggplot2 aes element_blank facet_wrap geom_label geom_line geom_point geom_segment geom_smooth geom_vline ggplot labs scale_x_log10 theme theme_bw
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
        linewidth = 0.5, ggplot2::aes(xintercept = .data$count)) +
      ggplot2::geom_label(data = l.data, aes(x = .data$x, y = .data$y),
        fill = geom.col, colour = "white", size = 2.5, label = pkg.ct$count) +
      ggplot2::scale_x_log10() +
      ggplot2::facet_wrap(ggplot2::vars(.data$package), nrow = 2) +
      ggplot2::labs(x = "Downloads") +
      ggplot2::labs(y = "Frequency") +
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
    title(sub = paste(cex.sub = 0.9,
      format(sum(x$data$count), big.mark = ","), "total downloads;",
      format(x$unique.packages, big.mark = ","), "unique packages"))
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
