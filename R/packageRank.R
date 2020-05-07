#' Package download counts and rank percentiles.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd".
#' @param size.filter Logical or Numeric. If Logical, TRUE filters out downloads less than 1000 bytes. If Numeric, a positive value sets the minimum download size (in bytes) to consider; a negative value sets the maximum download size to consider.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @return An R data frame.
#' @export
#' @examples
#' \donttest{
#' packageRank(packages = "HistData", date = "2020-01-01")
#' packageRank(packages = c("h2o", "Rcpp", "rstan"), date = "2020-01-01")
#' }

packageRank <- function(packages = "HistData", date = Sys.Date() - 1,
  size.filter = TRUE, memoization = TRUE, check.package = TRUE,
  dev.mode = FALSE) {

  if (check.package) {
    if (dev.mode) {
      pkg.chk <- validatePackage0(packages)
    } else {
      pkg.chk <- validatePackage(packages)
    }
    if (is.list(pkg.chk)) {
      error <- paste(pkg.chk$invalid, collapse = ", ")
      if (length(pkg.chk$valid) == 0) {
        stop(error, ": misspelled or not on CRAN/Archive.")
      } else {
        warning(error, ": misspelled or not on CRAN/Archive.")
        packages <- pkg.chk$valid
      }
    }
  }

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

  if (all(packages %in% unique(cran_log$package) == FALSE)) {
    stop(packages, ": not in log (not downloaded).")
  } else if (any(packages %in% unique(cran_log$package) == FALSE)) {
    err <- packages[packages %in% unique(cran_log$package) == FALSE]
    warning(err, ": not in log (not downloaded).")
    packages <- packages[packages %in% unique(cran_log$package)]
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
    100 * mean(crosstab < crosstab[x])
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

  class(out) <- "packageRank"
  out
}

#' Plot method for packageRank().
#' @param x An object of class "packageRank" created by \code{packageRank()}.
#' @param graphics Character. "base" or "ggplot2".
#' @param log_count Logical. Logarithm of package downloads.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @export
#' @examples
#' \donttest{
#' plot(packageRank(packages = "HistData", date = "2020-01-01"))
#' plot(packageRank(packages = c("h2o", "Rcpp", "rstan"), date = "2020-01-01"))
#' }

plot.packageRank <- function(x, graphics = NULL, log_count = TRUE, ...) {
  if (is.logical(log_count) == FALSE) stop("log_count must be TRUE or FALSE.")

  crosstab <- x$crosstab
  package.data <- x$package.data
  packages <- x$packages
  date <- x$date
  y.max <- crosstab[1]
  q <- stats::quantile(crosstab)[2:4]

  iqr <- vapply(c("75%", "50%", "25%"), function(id) {
    dat <- which(crosstab > q[[id]])
    dat[length(dat)]
  }, numeric(1L))

  if (is.null(graphics)) {
    if (length(packages) == 1) {
      basePlot(packages, log_count, crosstab, iqr, package.data, y.max, date)
    } else if (length(packages) > 1) {
      ggPlot(x, log_count, crosstab, iqr, package.data, y.max, date)
    } else stop("Error.")
  } else if (graphics == "base") {
    if (length(packages) > 1) {
      invisible(lapply(packages, function(pkg) {
        basePlot(pkg, log_count, crosstab, iqr, package.data, y.max, date)
      }))
    } else {
      basePlot(packages, log_count, crosstab, iqr, package.data, y.max, date)
    }
  } else if (graphics == "ggplot2") {
    ggPlot(x, log_count, crosstab, iqr, package.data, y.max, date)
  } else stop('graphics must be "base" or "ggplot2"')
}

#' Base R Graphics Plot.
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

  if (class(date) == "Date" ) {
    day <- weekdays(as.Date(date), abbreviate = TRUE)
    title(main = paste0(pkg, " @ ", date, " (", day, ")"))
  } else {
    title(main = paste0(pkg, " @ ", date))
  }
}

#' ggplot2 Graphics Plot.
#' @param x Object.
#' @param log_count Logical. Logarithm of package downloads.
#' @param crosstab Object.
#' @param iqr Object.
#' @param package.data Object.
#' @param y.max Numeric.
#' @param date Character.
#' @noRd

ggPlot <- function(x, log_count, crosstab, iqr, package.data, y.max, date) {
  package.data <- x$package.data
  packages <- x$packages

  if (class(date) == "Date") {
    day <- weekdays(as.Date(date), abbreviate = TRUE)
    id <- paste0(package.data$packages, " @ ", date, " (", day, ")")
  } else {
    id <- paste0(package.data$packages, " @ ", date)
  }
  
  download.data <- data.frame(x = seq_along(crosstab),
                              y = c(crosstab),
                              packages = names(crosstab),
                              row.names = NULL,
                              stringsAsFactors = FALSE)

  download.lst <- rep(list(download.data), length(x$packages))

  for (i in seq_along(download.lst)) {
    download.lst[[i]]$id <- id[i]
  }

  download.data <- do.call(rbind, download.lst)
  first <- cumsum(vapply(download.lst, nrow, numeric(1L))) -
    length(crosstab) + 1
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
