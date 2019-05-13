#' Package download counts and rank percentiles (cross-sectional).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param memoization Logical. Use memoisation when downloading logs.
#' @return An R data frame.
#' @import data.table RCurl
#' @export
#' @examples
#' \donttest{
#'
#' packageRank(packages = "HistData", date = "2019-01-01")
#' packageRank(packages = c("h2o", "Rcpp", "rstan"), date = "2019-01-01")
#' }

packageRank <- function(packages = "HistData", date = Sys.Date() - 1,
  memoization = TRUE) {

  ymd <- as.Date(date)
  if (ymd > Sys.Date()) stop("Can't see into the future!")
  year <- as.POSIXlt(ymd)$year + 1900
  rstudio.url <- "http://cran-logs.rstudio.com/"
  url <- paste0(rstudio.url, year, '/', ymd, ".csv.gz")

  if (RCurl::url.exists(url)) {
    if (memoization) cran_log <- mfetchLog(url)
    else cran_log <- fetchLog(url)
  } else {
    msg <- "Check your internet connection or try the previous day."
    stop("Log for ", date, " not (yet) available. ", msg)
  }

  # tools::CRAN_package_db() not always current/up-to-date?
  if (any(packages %in% unique(cran_log$package) == FALSE)) {
    stop("Package not found in log.")
  }

  crosstab <- sort(table(cran_log$package), decreasing = TRUE)

  # packages in bin
  pkg.bin <- lapply(packages, function(nm) {
    crosstab[crosstab %in% crosstab[nm]]
  })

  # offset: ties arbitrarily broken by alphabetical order
  pkg.bin.delta <- vapply(seq_along(pkg.bin), function(i) {
    which(names(pkg.bin[[i]]) %in% packages[i])
  }, numeric(1L))

  nominal.rank <- lapply(seq_along(packages), function(i) {
    sum(crosstab > crosstab[packages[i]]) + pkg.bin.delta[i]
  })

  tot.pkgs <- length(crosstab)

  pkg.percentile <- vapply(packages, function(x) {
    round(100 * mean(crosstab < crosstab[x]), 1)
  }, numeric(1L))

  dat <- data.frame(date = ymd,
                    packages = packages,
                    downloads = c(crosstab[packages]),
                    rank = unlist(nominal.rank),
                    percentile = pkg.percentile,
                    total.downloads = sum(crosstab),
                    total.packages = tot.pkgs,
                    stringsAsFactors = FALSE,
                    row.names = NULL)

  out <- list(packages = packages, date = ymd, package.data = dat,
    crosstab = crosstab)

  class(out) <- "package_rank"
  out
}

#' Plot method for packageRank().
#' @param x An object of class "package_rank" created by \code{packageRank()}.
#' @param graphics_pkg Character. "base" or "ggplot2".
#' @param log_count Logical. Logarithm of package downloads.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @import graphics ggplot2
#' @importFrom ggplot2 ggplot aes_string scale_y_log10 geom_point geom_line facet_wrap theme
#' @export
#' @examples
#' \donttest{
#'
#' plot(packageRank(packages = "HistData", date = "2019-01-01"))
#' plot(packageRank(packages = c("h2o", "Rcpp", "rstan"), date = "2019-01-01"))
#' }

plot.package_rank <- function(x, graphics_pkg = "ggplot2", log_count = TRUE,
  ...) {

  if (is.logical(log_count) == FALSE) stop("log_count must be TRUE or FALSE.")

  crosstab <- x$crosstab
  package.data <- x$package.data
  packages  <- x$packages
  date <- x$date
  y.max <- crosstab[1]
  q <- stats::quantile(crosstab)[2:4]

  iqr <- vapply(c("75%", "50%", "25%"), function(id) {
    dat <- which(crosstab > q[[id]])
    dat[length(dat)]
  }, numeric(1L))

  if (graphics_pkg == "base") {
    if (length(packages) > 1) {
      invisible(lapply(packages, function(x) {
        basePlot(x, log_count, crosstab, iqr, package.data, y.max, date)
      }))
    } else {
      basePlot(packages, log_count, crosstab, iqr, package.data, y.max, date)
    }
  } else if (graphics_pkg == "ggplot2") {
    package.data <- x$package.data
    id <- paste(package.data$packages, "@", date)

    download.data <- data.frame(x = 1:length(crosstab),
                                y = c(crosstab),
                                packages = row.names(crosstab),
                                row.names = NULL,
                                stringsAsFactors = FALSE)

    download.lst <- rep(list(download.data), length(x$packages))

    for (i in seq_along(download.lst)) {
      download.lst[[i]]$id <- id[i]
    }

    download.data <- do.call(rbind, download.lst)
    first <- cumsum(vapply(download.lst, nrow, numeric(1L))) -
      length(x$crosstab) + 1
    last <- sum(vapply(download.lst, nrow, numeric(1L)))
    iqr.labels <- c("75th", "50th", "25th")
    iqr.data <- data.frame(x = rep(iqr, length(x$packages)),
                           y = stats::quantile(crosstab, 0.995),
                           label = rep(iqr.labels, length(x$packages)),
                           id = rep(id, each = length(iqr)),
                           row.names = NULL)

    point.data <- lapply(seq_along(packages), function(i) {
      download.lst[[i]][download.lst[[i]]$packages %in% packages[i], ]
    })

    point.data <- do.call(rbind, point.data)

    top.pkg <- paste(download.data[first, "packages"], "=",
      format(download.data[first, "y"], big.mark = ","))
    tot.dwnld <- paste("Total =", format(sum(crosstab), big.mark = ","))

    xlabel <- paste0(round(package.data$percentile, 2), "%")
    ylabel <- format(point.data$y, big.mark = ",")

    if (length(packages) > 1) {
      label.size <- 2.5
      ylabel.nudge <- 0.75
    } else {
      label.size <- 3.5
      ylabel.nudge <- 0.5
    }

    p <- ggplot(data = download.data, aes_string("x", "y")) +
         geom_line() +
         geom_vline(xintercept = iqr, colour = "gray", linetype = "dotted") +
         xlab("Rank") +
         ylab("Count") +
         facet_wrap(~ id, ncol = 2) +
         theme_bw() +
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               plot.title = element_text(hjust = 0.5)) +
         geom_point(data = download.data[first, ],
                    shape = 1,
                    colour = "dodgerblue") +
         geom_text(data = download.data[first, ],
                   colour = "dodgerblue",
                   label = top.pkg,
                   hjust = -0.1,
                   size = 3) +
         geom_text(data = data.frame(x = download.data[last, "x"], y = y.max),
                   colour = "dodgerblue",
                   label = tot.dwnld,
                   hjust = 1,
                   size = 3) +
         geom_text(data = iqr.data, label = iqr.data$label)

    for (i in seq_along(packages)) {
      sel <- point.data$package == packages[i] & point.data$id == id[i]
      p <- p + geom_vline(data = point.data[sel, ],
                          aes_string(xintercept = "x"), colour = "red") +
               geom_hline(data = point.data[sel, ],
                          aes_string(yintercept = "y"), colour = "red")
    }

    p <- p + geom_point(data = point.data, aes_string("x", "y"), shape = 1,
                        colour = "red", size = 2) +
             geom_label(data = point.data, aes_string("x", "y"), fill = "red",
                        colour = "white", size = label.size, label = ylabel,
                        nudge_x = 2000) +
             geom_label(data = point.data, aes_string("x", "y"), fill = "red",
                        colour = "white", size = label.size, label = xlabel,
                        nudge_y = ylabel.nudge)

    if (log_count) p + scale_y_log10() else p
  } else stop('graphics_pkg must be "base" or "ggplot2"')
}

#' Base R Graphics Plot (Cross-sectional).
#' @param pkg Object.
#' @param log_count Logical. Logarithm of package downloads.
#' @param crosstab Object.
#' @param iqr Object.
#' @param package.data Object.
#' @param y.max Numeric.
#' @param date Character.
#' @noRd

basePlot <- function(pkg, log_count, crosstab, iqr, package.data, y.max, date) {
  if (log_count) {
    plot(c(crosstab), type = "l", xlab = "Rank", ylab = "log10(Count)",
      log = "y")
  } else {
    plot(c(crosstab), type = "l", xlab = "Rank", ylab = "Count")
  }

  abline(v = iqr, col = "gray", lty = "dotted")
  iqr.labels <- c("75th", "50th", "25th")

  if (log_count) {
    invisible(lapply(seq_along(iqr), function(i) {
      text(iqr[[i]], y.max / 2, labels = iqr.labels[i], cex = 0.75)
    }))
  } else {
    invisible(lapply(seq_along(iqr), function(i) {
      text(iqr[[i]], 3 * y.max / 4, labels = iqr.labels[i], cex = 0.75)
    }))
  }

  abline(v = which(names(crosstab) == pkg), col = "red")
  abline(h = crosstab[pkg], col = "red")

  pct <- package.data[package.data$package == pkg, "percentile"]
  pct.label <- paste0(round(pct, 2), "%")
  axis(3, at = which(names(crosstab) == pkg), padj = 0.9, col.axis = "red",
    col.ticks = "red", labels = pct.label, cex.axis = 0.8)

  axis(4, at = crosstab[pkg], col.axis = "red", col.ticks = "red",
    cex.axis = 0.8, labels = format(crosstab[pkg], big.mark = ","))
  points(which(names(crosstab) == pkg), crosstab[pkg], col = "red")
  points(which(names(crosstab) == names(crosstab[1])), y.max,
    col = "dodgerblue")
  text(which(names(crosstab) == names(crosstab[1])), y.max, pos = 4,
    labels = paste(names(crosstab[1]), "=", format(crosstab[1],
    big.mark = ",")), cex = 0.8, col = "dodgerblue")
  text(which(names(crosstab) == names(crosstab[length(crosstab)])), y.max,
    labels = paste("Tot = ", format(sum(crosstab), big.mark = ",")), cex = 0.8,
    col = "dodgerblue", pos = 2)
  title(main = paste(pkg, "@", date))
}

#' Print method for packageRank().
#' @param x An object of class "package_rank" created by \code{packageRank()}
#' @param ... Additional parameters.
#' @export

print.package_rank <- function(x, ...) {
  dat <- x$package.data
  rank <- paste(format(dat$rank, big.mark = ","), "of",
                format(dat$total.packages, big.mark = ","))
  out <- data.frame(dat[, c("date", "packages", "downloads", "percentile")],
    rank, stringsAsFactors = FALSE, row.names = NULL)
  print(out)
}

#' Summary method for packageRank().
#' @param object Object. An object of class "package_rank" created by \code{packageRank()}
#' @param ... Additional parameters.
#' @export
#' @note This is useful for directly accessing the data frame.

summary.package_rank <- function(object, ...) {
  object$package.data
}

#' Fetch Package Logs.
#'
#' @param x Character. URL
#' @import data.table memoise
#' @export
#' @note mFetchLog() is memoized version.

fetchLog <- function(x) data.table::fread(x)
mfetchLog <- memoise::memoise(fetchLog)
