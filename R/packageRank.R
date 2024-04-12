#' Package download counts and rank percentiles (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export
#' @examples
#' \dontrun{
#' packageRank(packages = "HistData", date = "2020-01-01")
#' packageRank(packages = c("h2o", "Rcpp", "rstan"), date = "2020-01-01")
#' }

packageRank <- function(packages = "HistData", date = NULL,
  all.filters = FALSE, ip.filter = FALSE, small.filter = FALSE,
  memoization = TRUE, check.package = TRUE, multi.core = FALSE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)

  cores <- multiCore(multi.core)
  if (.Platform$OS.type == "windows" & cores > 1) cores <- 1L

  if (check.package) packages <- checkPackage(packages)
  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  ymd <- rev_fixDate_2012(file.url.date)
  
  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
  }

  if (small.filter) cran_log <- smallFilter(cran_log)
  if (ip.filter) cran_log <- ipFilter(cran_log, multi.core = cores)
  
  freqtab <- sort(table(cran_log$package), decreasing = TRUE)

  unobs.pkgs <- !packages %in% cran_log$package
  if (any(unobs.pkgs)) pkg.msg <- paste(packages[unobs.pkgs], collapse = ", ")

  if (all(unobs.pkgs)) {
    stop("No downloads for ", pkg.msg, " on ", ymd, ".", call. = FALSE)
  } else if (any(unobs.pkgs)) {
    message("No downloads for ", pkg.msg, " on ", ymd, ".")
    packages <- packages[!unobs.pkgs]
  }

  # packages in bin
  pkg.bin <- lapply(packages, function(nm) freqtab[freqtab %in% freqtab[nm]])

  # offset: ties arbitrarily broken by alphabetical order
  pkg.bin.delta <- vapply(seq_along(pkg.bin), function(i) {
    which(names(pkg.bin[[i]]) %in% packages[i])
  }, numeric(1L))

  nominal.rank <- lapply(seq_along(packages), function(i) {
    sum(freqtab > freqtab[packages[i]]) + pkg.bin.delta[i]
  })

  tot.pkgs <- length(freqtab)

  pkg.percentile <- vapply(packages, function(x) {
    100 * mean(freqtab < freqtab[x])
  }, numeric(1L))

  dat <- data.frame(date = ymd,
                    packages = packages,
                    downloads = c(freqtab[packages]),
                    rank = unlist(nominal.rank),
                    percentile = pkg.percentile,
                    total.downloads = sum(freqtab),
                    total.packages = tot.pkgs,
                    stringsAsFactors = FALSE,
                    row.names = NULL)

  out <- list(packages = packages, date = ymd, package.data = dat,
    freqtab = freqtab)

  class(out) <- "packageRank"
  out
}

#' Plot method for packageRank() and packageRank0().
#' @param x An object of class "packageRank" created by \code{packageRank()}.
#' @param graphics Character. "base" or "ggplot2".
#' @param log.y Logical. Logarithm of package downloads.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @export
#' @examples
#' \dontrun{
#' plot(packageRank(packages = "HistData", date = "2020-01-01"))
#' plot(packageRank(packages = c("h2o", "Rcpp", "rstan"), date = "2020-01-01"))
#' }

plot.packageRank <- function(x, graphics = NULL, log.y = TRUE, ...) {
  if (is.logical(log.y) == FALSE) stop("log.y must be TRUE or FALSE.")

  freqtab <- x$freqtab
  package.data <- x$package.data
  packages <- x$packages
  date <- x$date
  y.max <- freqtab[1]
  q <- stats::quantile(freqtab)[2:4]

  iqr <- vapply(c("75%", "50%", "25%"), function(id) {
    dat <- which(freqtab > q[[id]])
    dat[length(dat)]
  }, numeric(1L))

  if (is.null(graphics)) {
    if (length(packages) == 1) {
      basePlot(packages, log.y, freqtab, iqr, package.data, y.max, date)
    } else if (length(packages) > 1) {
      ggPlot(x, log.y, freqtab, iqr, package.data, y.max, date)
    } else stop("Error.")
  } else if (graphics == "base") {
    if (length(packages) > 1) {
      invisible(lapply(packages, function(pkg) {
        basePlot(pkg, log.y, freqtab, iqr, package.data, y.max, date)
      }))
    } else {
      basePlot(packages, log.y, freqtab, iqr, package.data, y.max, date)
    }
  } else if (graphics == "ggplot2") {
    ggPlot(x, log.y, freqtab, iqr, package.data, y.max, date)
  } else stop('graphics must be "base" or "ggplot2"')
}

#' Base R Graphics Plot.
#' @param pkg Object.
#' @param log.y Logical. Logarithm of package downloads.
#' @param freqtab Object.
#' @param iqr Object.
#' @param package.data Object.
#' @param y.max Numeric.
#' @param date Character.
#' @noRd

basePlot <- function(pkg, log.y, freqtab, iqr, package.data, y.max, date) {
  if (log.y) {
    plot(c(freqtab), type = "l", xlab = "Rank", ylab = "log10(Count)",
      log = "y")
  } else {
    plot(c(freqtab), type = "l", xlab = "Rank", ylab = "Count")
  }

  abline(v = iqr, col = "gray", lty = "dotted")
  iqr.labels <- c("75th", "50th", "25th")

  if (log.y) {
    invisible(lapply(seq_along(iqr), function(i) {
      text(iqr[[i]], y.max / 2, labels = iqr.labels[i], cex = 0.75)
    }))
  } else {
    invisible(lapply(seq_along(iqr), function(i) {
      text(iqr[[i]], 3 * y.max / 4, labels = iqr.labels[i], cex = 0.75)
    }))
  }

  abline(v = which(names(freqtab) == pkg), col = "red")
  abline(h = freqtab[pkg], col = "red")

  pct <- package.data[package.data$package == pkg, "percentile"]
  pct.label <- paste0(round(pct, 2), "%")
  axis(3, at = which(names(freqtab) == pkg), padj = 0.9, col.axis = "red",
    col.ticks = "red", labels = pct.label, cex.axis = 0.8)

  axis(4, at = freqtab[pkg], col.axis = "red", col.ticks = "red",
    cex.axis = 0.8, labels = format(freqtab[pkg], big.mark = ","))
  points(which(names(freqtab) == pkg), freqtab[pkg], col = "red")
  points(which(names(freqtab) == names(freqtab[1])), y.max,
    col = "dodgerblue")
  text(which(names(freqtab) == names(freqtab[1])), y.max, pos = 4,
    labels = paste(names(freqtab[1]), "=", format(freqtab[1],
    big.mark = ",")), cex = 0.8, col = "dodgerblue")
  text(which(names(freqtab) == names(freqtab[length(freqtab)])), y.max,
    labels = paste("Tot = ", format(sum(freqtab), big.mark = ",")), cex = 0.8,
    col = "dodgerblue", pos = 2)

  if (inherits(date, "Date")) {
    day <- weekdays(as.Date(date), abbreviate = TRUE)
    title(main = paste0(pkg, " @ ", date, " (", day, ")"))
  } else {
    title(main = paste0(pkg, " @ ", date))
  }
}

#' ggplot2 Graphics Plot.
#' @param x Object.
#' @param log.y Logical. Logarithm of package downloads.
#' @param freqtab Object.
#' @param iqr Object.
#' @param package.data Object.
#' @param y.max Numeric.
#' @param date Character.
#' @importFrom ggplot2 geom_label geom_vline
#' @noRd

ggPlot <- function(x, log.y, freqtab, iqr, package.data, y.max, date) {
  package.data <- x$package.data
  packages <- x$packages

  if (inherits(date, "Date")) {
    day <- weekdays(as.Date(date), abbreviate = TRUE)
    id <- paste0(package.data$packages, " @ ", date, " (", day, ")")
  } else {
    id <- paste0(package.data$packages, " @ ", date)
  }

  download.data <- data.frame(x = seq_along(freqtab),
                              y = c(freqtab),
                              packages = names(freqtab),
                              row.names = NULL,
                              stringsAsFactors = FALSE)

  download.lst <- rep(list(download.data), length(x$packages))

  for (i in seq_along(download.lst)) download.lst[[i]]$id <- id[i]
  
  download.data <- do.call(rbind, download.lst)
  first <- cumsum(vapply(download.lst, nrow, numeric(1L))) - length(freqtab) + 1
  last <- sum(vapply(download.lst, nrow, numeric(1L)))
  iqr.labels <- c("75th", "50th", "25th")
  iqr.data <- data.frame(x = rep(iqr, length(x$packages)),
                         y = stats::quantile(freqtab, 0.995),
                         label = rep(iqr.labels, length(x$packages)),
                         id = rep(id, each = length(iqr)),
                         row.names = NULL)

  point.data <- lapply(seq_along(packages), function(i) {
    download.lst[[i]][download.lst[[i]]$packages %in% packages[i], ]
  })

  point.data <- do.call(rbind, point.data)

  top.pkg <- paste(download.data[first, "packages"], "=",
    format(download.data[first, "y"], big.mark = ","))
  tot.dwnld <- paste("Total =", format(sum(freqtab), big.mark = ","))

  xlabel <- paste0(round(package.data$percentile, 2), "%")
  ylabel <- format(point.data$y, big.mark = ",")

  if (length(packages) > 1) {
    label.size <- 2.5
    ylabel.nudge <- 0.75
  } else {
    label.size <- 3.5
    ylabel.nudge <- 0.5
  }

  alpha.col <- grDevices::adjustcolor("red", alpha.f = 2/3)

  geom.text.data <- data.frame(x = download.data[last, "x"], y = y.max)

  p <- ggplot2::ggplot(data = download.data, 
         ggplot2::aes(x = .data$x, y = .data$y)) +
       ggplot2::geom_line() +
       ggplot2::geom_vline(xintercept = iqr, colour = "gray", 
        linetype = "dotted") +
       ggplot2::geom_point(data = download.data[first, ],
                           shape = 1,
                           colour = "dodgerblue") +
       ggplot2::geom_text(data = download.data[first, ],
                          colour = "dodgerblue",
                          label = top.pkg,
                          hjust = -0.1,
                          size = 3) +
       ggplot2::geom_text(data = geom.text.data,
                          colour = "dodgerblue",
                          label = tot.dwnld,
                          hjust = 1,
                          size = 3) +
       ggplot2::geom_text(data = iqr.data, label = iqr.data$label) +
       ggplot2::geom_vline(data = point.data,
         ggplot2::aes(xintercept = .data$x), colour = alpha.col) +
       ggplot2::geom_hline(data = point.data, 
         ggplot2::aes(yintercept = .data$y), colour = alpha.col) +
       ggplot2::geom_point(data = point.data, 
         ggplot2::aes(x = .data$x, y = .data$y), shape = 1, colour = "red", 
           size = 2) +
       ggplot2::geom_label(data = point.data, 
         ggplot2::aes(x = .data$x, y = .data$y), fill = alpha.col, 
           colour = "white", size = label.size, label = ylabel, nudge_x = 2000) +
       ggplot2::geom_label(data = point.data, 
          ggplot2::aes(x = .data$x, y = .data$y), fill = alpha.col, 
            colour = "white", size = label.size, label = xlabel, 
            nudge_y = ylabel.nudge) +
       ggplot2::xlab("Rank") +
       ggplot2::ylab("Count") +
       ggplot2::facet_wrap(ggplot2::vars(.data$id), nrow = 2) +
       ggplot2::theme_bw() +
       ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank())

  if (log.y) p + ggplot2::scale_y_log10() else p
}

#' Print method for packageRank().
#' @param x An object of class "packageRank" created by \code{packageRank()}
#' @param ... Additional parameters.
#' @export

print.packageRank <- function(x, ...) {
  dat <- x$package.data
  dat$downloads <- format(dat$downloads, big.mark = ",")
  rank <- paste(format(dat$rank, big.mark = ","), "of",
                format(dat$total.packages, big.mark = ","))
  out <- data.frame(dat[, c("date", "packages", "downloads")], rank,
    percentile = round(dat[, "percentile"], 1), stringsAsFactors = FALSE,
    row.names = NULL)
  print(out)
}

#' Summary method for packageRank().
#' @param object Object. An object of class "packageRank" created by \code{packageRank()}
#' @param ... Additional parameters.
#' @export
#' @note This is useful for directly accessing the data frame.

summary.packageRank <- function(object, ...) {
  object$package.data
}
