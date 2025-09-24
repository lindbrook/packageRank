#' Package download counts and rank percentiles.
#'
#' From Posit/RStudio's CRAN Mirror (CDN) http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param rank.ties Logical. TRUE uses competition ranking ("1224") for ties. FALSE uses nominal rank (no ties).
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export
#' @examples
#' \dontrun{
#' packageRank(packages = "cholera", date = "2020-01-01")
#' packageRank(packages = c("h2o", "Rcpp", "rstan"), date = "2020-01-01")
#' }

packageRank <- function(packages = "packageRank", date = NULL,
  all.filters = FALSE, ip.filter = FALSE, small.filter = FALSE,
  memoization = TRUE, check.package = TRUE, rank.ties = TRUE, 
  multi.core = FALSE) {

  if (check.package) packages <- checkPackage(packages)

  x <- cranDistribution(date = date, all.filters = all.filters, 
    ip.filter = ip.filter, small.filter = small.filter, 
    memoization = memoization, multi.core = multi.core)

  unobs.pkgs <- !packages %in% x$data$package
  if (any(unobs.pkgs)) pkg.msg <- paste(packages[unobs.pkgs], collapse = ", ")

  if (all(unobs.pkgs)) {
    stop("No downloads for ", pkg.msg, " on ", x$date, ".", call. = FALSE)
  } else if (any(unobs.pkgs)) {
    message("No downloads for ", pkg.msg, " on ", x$date, ".")
    packages <- packages[!unobs.pkgs]
  }
  
  tmp <- x$data[x$data$package %in% packages, ]
  tmp <- tmp[match(packages, tmp$package), ]
  
  if (rank.ties) {
    rnk <- paste(format(tmp$rank, big.mark = ","), "of", 
                 format(x$unique.packages, big.mark = ","))
  } else {
    rnk <- paste(format(tmp$nominal.rank, big.mark = ","), "of", 
                 format(x$unique.packages, big.mark = ","))
  }
  
  pkg.data <- data.frame(date = x$date, tmp[, c("package", "count")], 
    rank = rnk, percentile = tmp$percentile, row.names = NULL)
  
  out <- list(packages = packages, date = x$date, package.data = pkg.data, 
    cran.data = x$data)
  
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

  freqtab <- x$cran.data$count
  names(freqtab) <- x$cran.data$package
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
        grDevices::devAskNewPage(ask = TRUE)
        basePlot(pkg, log.y, freqtab, iqr, package.data, y.max, date)
        grDevices::devAskNewPage(ask = FALSE)
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
    id <- paste0(package.data$package, " @ ", date, " (", day, ")")
  } else {
    id <- paste0(package.data$packags, " @ ", date)
  }

  download.data <- data.frame(x = seq_along(freqtab),
                              y = freqtab,
                              packages = names(freqtab),
                              row.names = NULL)

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
       ggplot2::geom_text(data = iqr.data, label = iqr.data$label, size = 3.5) +
       ggplot2::geom_vline(data = point.data,
         ggplot2::aes(xintercept = .data$x), colour = alpha.col) +
       ggplot2::geom_hline(data = point.data, 
         ggplot2::aes(yintercept = .data$y), colour = alpha.col) +
       ggplot2::geom_point(data = point.data, 
         ggplot2::aes(x = .data$x, y = .data$y), shape = 1, colour = "red", 
           size = 2) +
       ggplot2::geom_label(data = point.data, 
         ggplot2::aes(x = .data$x, y = .data$y), fill = alpha.col, 
           colour = "white", size = label.size, label = ylabel,
           nudge_x = 2000) +
       ggplot2::geom_label(data = point.data, 
          ggplot2::aes(x = .data$x, y = .data$y), fill = alpha.col, 
            colour = "white", size = label.size, label = xlabel, 
            nudge_y = ylabel.nudge) +
       ggplot2::labs(x = "Rank") +
       ggplot2::labs(y = "Count") +
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
  print(x$package.data)
}

#' Summary method for packageRank().
#' @param object Object. An object of class "packageRank" created by \code{packageRank()}
#' @param ... Additional parameters.
#' @export
#' @note This is useful for directly accessing the data frame.

summary.packageRank <- function(object, ...) {
  object$package.data
}
