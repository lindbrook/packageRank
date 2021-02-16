#' Daily package downloads from the RStudio CRAN mirror.
#'
#' Enhanced implementation of cranlogs::cran_downloads().
#' @param packages A character vector, the packages to query,
#'   or \code{NULL} for a sum of downloads for all packages.
#'   Alternatively, it can also be \code{"R"}, to query downloads
#'   of R itself. \code{"R"} cannot be mixed with packages.
#' @param when \code{last-day}, \code{last-week} or \code{last-month}.
#'   If this is given, then \code{from} and \code{to} are ignored.
#' @param from Start date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
#' @param to End date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @export
#' @examples
#' \dontrun{
#' cranDownloads(packages = "HistData")
#' cranDownloads(packages = "HistData", when = "last-week")
#' cranDownloads(packages = "HistData", when = "last-month")
#'
#' # January 7 - 31, 2019
#' cranDownloads(packages = "HistData", from = "2019-01-07", to = "2019-01-31")
#'
#' # February through March 2019
#' cranDownloads(packages = "HistData", from = "2019-02", to = "2019-03")
#'
#' # 2020 year-to-date
#' cranDownloads(packages = "HistData", from = 2020)
#' }

cranDownloads <- function(packages = NULL, when = NULL, from = NULL,
  to = NULL, check.package = TRUE, dev.mode = FALSE) {

  if (length(packages) > 1) {
    if ("R" %in% packages) {
      stop("R downloads cannot be mixed with package downloads.", call. = FALSE)
    }
  }

  if (!is.null(packages)) {
    if (!"R" %in% packages) {
      if (check.package) {
        packages <- checkPackage(packages, dev.mode)
      }
    }
  }

  cal.date <- logDate(warning.msg = FALSE)

  if (is.null(when) & is.null(from) & is.null(to)) {
    args <- list(packages = packages, from = cal.date, to = cal.date)

  } else if (!is.null(when) & is.null(from) & is.null(to)) {
    if (when %in% c("last-day", "last-week", "last-month")) {
      args <- list(packages = packages, when = when)
    } else stop('"when" must be "last-day", "last-week" or "last-month".',
      call. = FALSE)

  } else if (is.null(when) & !is.null(from)) {
    start.date <- resolveDate(from, type = "from")

    if (!is.null(to)) {
      end.date <- resolveDate(to, type = "to")
    } else end.date <- cal.date

    if (start.date > end.date) stop('"from" must be <= "to".', call. = FALSE)
    args <- list(packages = packages, from = start.date, to = end.date)

  } else if (is.null(when) & !is.null(to)) {
    end.date <- resolveDate(to, type = "to")

    first.published <- lapply(packages, function(pkg) {
      packageHistory(pkg)[1, "Date"]
    })

    to.data <- lapply(seq_along(packages), function(i) {
      cranlogs::cran_downloads(packages[i], from = first.published[[i]],
        to = end.date)
    })
  }

  if ("args" %in% ls()) {
    cranlogs.data <- do.call(cranlogs::cran_downloads, args)

    if (is.null(args$packages)) {
      cranlogs.data$cumulative <- cumsum(cranlogs.data$count)
    } else if ("R" %in% args$packages) {
      cranlogs.data <- cranlogs.data[cranlogs.data$os != "NA", ]
      count <- tapply(cranlogs.data$count, list(cranlogs.data$date,
        cranlogs.data$os), sum)
      cumulative <- apply(count, 2, cumsum)
      dts <- rep(as.Date(row.names(count)), ncol(count))
      plt <- rep(colnames(count), each = nrow(count))
      cranlogs.data <- data.frame(date = dts, count = c(count),
        cumulative = c(cumulative), platform = plt, row.names = NULL)
    } else {
      cumulative <- lapply(unique(cranlogs.data$package), function(pkg) {
        cumsum(cranlogs.data[cranlogs.data$package == pkg, "count"])
      })
      cranlogs.data <- cbind(cranlogs.data[, c("date", "count")],
        unlist(cumulative), cranlogs.data$package)
      sel <- (ncol(cranlogs.data) - 1):ncol(cranlogs.data)
      names(cranlogs.data)[sel] <- c("cumulative", "package")
    }
    out <- list(packages = packages, cranlogs.data = cranlogs.data,
      when = args$when, from = args$from, to = args$to)
  } else {
    cranlogs.data <- do.call(rbind, to.data)

    cumulative <- unlist(lapply(unique(cranlogs.data$package), function(pkg) {
      cumsum(cranlogs.data[cranlogs.data$package == pkg, "count"])
    }))

    cranlogs.data <- cbind(cranlogs.data[, c("date", "count")], cumulative,
      cranlogs.data$package)
    names(cranlogs.data)[ncol(cranlogs.data)] <- "package"

    id <- which.min(unlist(first.published))
    out <- list(packages = packages, cranlogs.data = cranlogs.data,
      when = NULL, from = first.published[[id]], to = end.date)
  }

  class(out) <- "cranDownloads"
  out
}

#' Plot method for cranDownloads().
#'
#' @param x object.
#' @param statistic Character. "count" or "cumulative".
#' @param graphics Character. "auto", "base" or "ggplot2".
#' @param points Character of Logical. Plot points. "auto", TRUE, FALSE.
#' @param log.count Logical. Logarithm of package downloads.
#' @param smooth Logical. Add smoother.
#' @param se Logical. Works only with graphics = "ggplot2".
#' @param f Numeric. stats::lowess() smoother window. For use with graphics = "base" only.
#' @param package.version Logical. Add latest package release dates.
#' @param r.version Logical. Add R release dates.
#' @param population.plot Logical. Plot population plot.
#' @param population.seed Numeric. Seed for sample in population plot.
#' @param multi.plot Logical.
#' @param same.xy Logical. Use same scale for multiple packages when graphics = "base".
#' @param legend.loc Character.
#' @param r.total Logical.
#' @param dev.mode Logical. Use packageHistory0() to scrape CRAN.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @export
#' @examples
#' \dontrun{
#' plot(cranDownloads(packages = c("Rcpp", "rlang", "data.table")))
#' plot(cranDownloads(packages = c("Rcpp", "rlang", "data.table"), when = "last-month"))
#' plot(cranDownloads(packages = "R", from = "2020-01-01", to = "2020-01-01"))
#' plot(cranDownloads(packages = "R", from = 2020))
#' }

plot.cranDownloads <- function(x, statistic = "count", graphics = "auto",
  points = "auto", log.count = FALSE, smooth = FALSE, se = FALSE, f = 1/3,
  package.version = FALSE, r.version = FALSE, population.plot = FALSE,
  population.seed = as.numeric(Sys.Date()), multi.plot = FALSE, same.xy = TRUE,
  legend.loc = "topleft", r.total = FALSE, dev.mode = FALSE, ...) {

  if (graphics == "auto") {
    if (is.null(x$packages)) {
      graphics <- "base"
    } else if (length(x$packages) == 1) {
      graphics <- "base"
    } else if (length(x$package) > 1) {
      graphics <- "ggplot2"
    }
  }

  if (is.logical(log.count) == FALSE) {
    stop("log.count must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.logical(smooth) == FALSE) {
    stop("smooth must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.logical(se) == FALSE) {
    stop("se must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.numeric(f) == FALSE) {
    stop("f must be numeric.", call. = FALSE)
  }
  if (statistic %in% c("count", "cumulative") == FALSE) {
    stop('"statistic" must be "count" or "cumulative".', call. = FALSE)
  }

  dat <- x$cranlogs.data
  days.observed <- unique(dat$date)

  if (points == "auto") {
    if (length(days.observed) <= 45) points <- TRUE else points <- FALSE
  } else if (is.logical(points) == FALSE) {
    stop('points must be "auto", TRUE, or FALSE.', call. = FALSE)
  }

  if (population.plot) {
     populationPlot(x, graphics = graphics, f = f,
       population.seed = population.seed)
  } else if ("R" %in% x$packages) {
    if (r.total) {
      rTotPlot(x, statistic, graphics, legend.loc, points, smooth, se,
        r.version, f)
    } else {
      rPlot(x, statistic, graphics, legend.loc, points, smooth, se, r.version,
        f)
    }
  } else {
    if (multi.plot) {
      multiPlot(x, statistic, graphics, days.observed, log.count, legend.loc,
        points, smooth, se)
    } else {
      singlePlot(x, statistic, graphics, days.observed, points, smooth, se, f,
        log.count, package.version, dev.mode, r.version, same.xy)
    }
  }
}

#' Print method for cranDownloads().
#' @param x object.
#' @param ... Additional parameters.
#' @export

print.cranDownloads <- function(x, ...) {
  print(x$cranlogs.data)
}

#' Summary method for cranDownloads().
#' @param object Object.
#' @param ... Additional parameters.
#' @export
#' @note This is useful for directly accessing the data frame.

summary.cranDownloads <- function(object, ...) {
  object$cranlogs.data
}

rPlot <- function(x, statistic, graphics, legend.loc, points, smooth, se,
  r.version, f) {

  dat <- x$cranlogs.data

  if (statistic == "count") {
    ylab <- "Count"
  } else if (statistic == "cumulative") {
    ylab <- "Cumulative"
  }

  if (graphics == "base") {
    if (points) {
      plot(dat[dat$platform == "win", "date"],
           dat[dat$platform == "win", statistic],
           type = "o", ylim = range(dat[, statistic]),
           xlab = "Date", ylab = ylab)
      lines(dat[dat$platform == "osx", "date"],
            dat[dat$platform == "osx", statistic],
            type = "o", pch = 0, col = "red")
      lines(dat[dat$platform == "src", "date"],
            dat[dat$platform == "src", statistic],
            type = "o", pch = 2, col = "blue")
    } else {
      plot(dat[dat$platform == "win", "date"],
           dat[dat$platform == "win", statistic],
           type = "l", ylim = range(dat[, statistic]),
           xlab = "Date", ylab = ylab)
      lines(dat[dat$platform == "osx", "date"],
            dat[dat$platform == "osx", statistic],
            col = "red")
      lines(dat[dat$platform == "src", "date"],
            dat[dat$platform == "src", statistic],
            col = "blue")
    }

    legend(x = legend.loc,
           legend = c("win", "mac", "src"),
           col = c("black", "red", "blue"),
           pch = c(1, 0, 2),
           bg = "white",
           cex = 2/3,
           title = "Platform",
           lwd = 1)

    if (smooth) {
      lines(stats::lowess(dat[dat$platform == "win", "date"],
                          dat[dat$platform == "win", statistic], f = f),
                          lty = "dotted")
      lines(stats::lowess(dat[dat$platform == "osx", "date"],
                          dat[dat$platform == "osx", statistic], f = f),
                          lty = "dotted", col = "red")
      lines(stats::lowess(dat[dat$platform == "src", "date"],
                          dat[dat$platform == "src", statistic], f = f),
                          lty = "dotted", col = "blue")
    }

    if (r.version) {
      r_v <- rversions::r_versions()
      axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
        cex.axis = 2/3, padj = 0.9)
      abline(v = as.Date(r_v$date), lty = "dotted")
    }

    title(main = "R Downloads")

  } else if (graphics == "ggplot2") {
    if (statistic == "count") {
      dat2 <- dat[, c("date", "count", "platform")]
      p <- ggplot(data = dat2, aes_string("date", "count"))
    } else {
      dat2 <- dat[, c("date", "cumulative", "platform")]
      p <- ggplot(data = dat2, aes_string("date", "cumulative"))
    }

    p <- p + geom_line(size = 0.5) +
      facet_wrap(~ platform, nrow = 2) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      ggtitle("R Downloads")

    if (points & smooth) {
      p + geom_point() +
        geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (points & !smooth) {
      p + geom_point()
    } else if (!points & smooth) {
      p + geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else p
  } else stop('graphics must be "base" or "ggplot2"', call. = FALSE)
}

rTotPlot <- function(x, statistic, graphics, legend.loc, points, smooth, se,
  r.version, f) {

  dat <- x$cranlogs.data
  if (statistic == "count") ylab <- "Count"
  if (statistic == "cumulative") ylab <- "Cumulative"
  ct <- tapply(dat$count, dat$date, sum)
  cs <- cumsum(ct)
  dat2 <- data.frame(date = as.Date(names(ct)), count = ct, cumulative = cs,
    row.names = NULL)

  if (graphics == "base") {
    if (points) {
      plot(dat2$date, dat2[, statistic], type = "o", xlab = "Date", ylab = ylab)
    } else {
      plot(dat2$date, dat2[, statistic], type = "l", xlab = "Date", ylab = ylab)
    }

    if (smooth) {
      lines(stats::lowess(dat2$date, dat2[, statistic], f), col = "blue",
        lwd = 1.25)
    }

    if (r.version) {
      r_v <- rversions::r_versions()
      axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
        cex.axis = 2/3, padj = 0.9)
      abline(v = as.Date(r_v$date), lty = "dotted")
    }

    title(main = "Total R Downloads")

  } else if (graphics == "ggplot2") {
    if (statistic == "count") {
      p <- ggplot(data = dat2, aes_string("date", "count"))
    } else if (statistic == "cumulative") {
      p <- ggplot(data = dat2, aes_string("date", "cumulative"))
    }

    p <- p + geom_line(size = 0.5) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      ggtitle("Total R Downloads")

    if (points & smooth) {
      p + geom_point() +
        geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (points & !smooth) {
      p + geom_point()
    } else if (!points & smooth) {
      p + geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else p
  } else stop('graphics must be "base" or "ggplot2"', call. = FALSE)
}

multiPlot <- function(x, statistic, graphics, days.observed, log.count,
  legend.loc, points, smooth, se) {

  dat <- x$cranlogs.data

  if (statistic == "count") {
    ttl <- "Package Download Counts"
  } else if (statistic == "cumulative") {
    ttl <- "Cumulative Package Downloads"
  }

  if (graphics == "base") {
    if (length(days.observed) == 1) {
      if (log.count) {
        dotchart(log10(dat$count), labels = dat$package,
          xlab = "log10(Count)", main = days.observed)
      } else {
        dotchart(dat$count, labels = dat$package, xlab = "Count",
          main = days.observed)
      }
    } else if (length(days.observed) > 1) {
      if (length(x$packages) > 8) {
        stop('Use <= 8 packages when graphics = "base".', call. = FALSE)
      } else {
        if (log.count) {
          if (points) {
            plot(dat[dat$package == x$packages[1], c("date", statistic)],
              ylim = range(dat[, statistic]), type = "o", log = "y", main = ttl)
          } else {
            plot(dat[dat$package == x$packages[1], c("date", statistic)],
              ylim = range(dat[, statistic]), type = "l", log = "y", main = ttl)
          }
        } else {
          if (points) {
            plot(dat[dat$package == x$packages[1], c("date", statistic)],
              ylim = range(dat[, statistic]), type = "o", main = ttl)
          } else {
            plot(dat[dat$package == x$packages[1], c("date", statistic)],
              ylim = range(dat[, statistic]), type = "l", main = ttl)
          }
        }

        # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
        # http://jfly.iam.u-tokyo.ac.jp/color/
        # The palette with grey:
        # cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
        #   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        token <- c(0, 2:7)
        invisible(lapply(seq_along(x$packages)[-1], function(i) {
          lines(dat[dat$package == x$packages[i], c("date", statistic)],
            type = "o", col = cbPalette[i], pch = token[i])
        }))

        id <- seq_along(x$packages)
        legend(x = legend.loc,
               legend = x$packages,
               col = cbPalette[id],
               pch = c(1, token[id]),
               bg = "white",
               cex = 2/3,
               title = NULL,
               lwd = 1)
      }
    }

  } else if (graphics == "ggplot2") {
    if (length(days.observed) == 1) {
      p <- ggplot(data = dat, aes_string("count", y = "package",
                  colour = "package"))
      if (log.count) {
        # p + scale_x_log10() + xlab("log10(count)") doesn't work!
        dat2 <- dat
        dat2$count <- log10(dat2$count)
        p <- ggplot(data = dat2, aes_string("count", "package",
                    colour = "package")) +
             xlab("log10(count)")
      }

      p <- p + geom_hline(yintercept = c(1, 2), linetype = "dotted") +
        theme(panel.grid.minor = element_blank())

    } else if (length(days.observed) > 1) {
      if (statistic == "count") {
        p <- ggplot(data = dat,
                    aes_string("date", "count",
                    colour = "package")) +
             ggtitle("Package Download Counts")
      } else if (statistic == "cumulative") {
        p <- ggplot(data = dat,
                    aes_string("date", "cumulative",
                    colour = "package")) +
             ggtitle("Cumulative Package Downloads")
      }

      p <- p + geom_line() +
        theme(panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5))
    }

    if (points & log.count & smooth) {
      p + geom_point() + scale_y_log10() +
        geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (points & log.count & !smooth) {
      p + geom_point() + scale_y_log10()
    } else if (points & !log.count & smooth) {
      p + geom_point() +
        geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (!points & log.count & smooth) {
      p + scale_y_log10() +
        geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (!points & !log.count & smooth) {
      p + geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (points & !log.count & !smooth) {
      p + geom_point()
    } else if (!points & log.count & !smooth) {
      p + scale_y_log10()
    } else p
  }
}

cranDownloadsPlot <- function(x, statistic, graphics, points, log.count,
  smooth, se, f, r.version) {

  dat <- x$cranlogs.data

  if (statistic == "count") {
    y.nm.case <- "Count"
    y.nm <- tolower(y.nm.case)
  } else if (statistic == "cumulative") {
    y.nm.case <- "Cumulative"
    y.nm <- tolower(y.nm.case)
  }

  if (graphics == "base") {
    if (log.count) {
      if (points) {
        plot(dat$date, dat[, y.nm], type = "o", xlab = "Date",
          ylab = paste0("log10(", y.nm.case, ")"), log = "y")
      } else {
        plot(dat$date, dat[, y.nm], type = "l", xlab = "Date",
          ylab = paste0("log10(", y.nm.case, ")"), log = "y")
      }
    } else {
      if (points) {
        plot(dat$date, dat[, y.nm], type = "o", xlab = "Date", ylab = y.nm.case)
      } else {
        plot(dat$date, dat[, y.nm], type = "l", xlab = "Date", ylab = y.nm.case)
      }
    }

    if (r.version) {
      r_v <- rversions::r_versions()
      axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
        cex.axis = 2/3, padj = 0.9)
      abline(v = as.Date(r_v$date), lty = "dotted")
    }

    if (smooth) {
      lines(stats::lowess(dat$date, dat[, y.nm], f = f),
        col = "blue")
    }

    title(main = "Total Package Downloads")

  } else if (graphics == "ggplot2") {
    if (statistic == "count") {
      p <- ggplot(data = dat, aes_string("date", "count"))
    } else if (statistic == "cumulative") {
      p <- ggplot(data = dat, aes_string("date", "cumulative"))
    }

    p <- p + geom_line(size = 0.5) +
      theme_bw() +
      ggtitle("Total Package Downloads") +
      theme(plot.title = element_text(hjust = 0.5))

    if (points & log.count & smooth) {
      p + geom_point() +
          scale_y_log10() +
          geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (points & log.count & !smooth) {
      p + geom_point() + scale_y_log10()
    } else if (points & !log.count & smooth) {
      p + geom_point() +
       geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (!points & log.count & smooth) {
      p + scale_y_log10() +
        geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (!points & !log.count & smooth) {
      p + geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (points & !log.count & !smooth) {
      p + geom_point()
    } else if (!points & log.count & !smooth) {
      p + scale_y_log10()
    } else p

  } else stop('graphics must be "base" or "ggplot2".', call. = FALSE)
}

singlePlot <- function(x, statistic, graphics, days.observed, points, smooth,
  se, f, log.count, package.version, dev.mode, r.version, same.xy) {

  dat <- x$cranlogs.data

  if (statistic == "count") {
    y.var <- dat$count
    y.nm.case <- "Count"
    y.nm <- tolower(y.nm.case)
  } else if (statistic == "cumulative") {
    y.var <- dat$cumulative
    y.nm.case <- "Cumulative"
    y.nm <- tolower(y.nm.case)
  }

  if (graphics == "base") {
    if (is.null(x$packages)) {
      cranDownloadsPlot(x, statistic, graphics, points, log.count, smooth, se,
        f, r.version)

    } else if (length(x$packages) > 1) {
      if (length(days.observed) == 1) {
        if (log.count) {
          dotchart(log10(dat$count), labels = dat$package,
            xlab = "log10(Count)", main = days.observed)
        } else {
          dotchart(dat$count, labels = dat$package, xlab = "Count",
            main = days.observed)
        }
      } else if (length(days.observed) > 1) {

        if (same.xy) {
          xlim <- range(dat$date)
          ylim <- range(y.var)
          grDevices::devAskNewPage(ask = TRUE)

          invisible(lapply(x$package, function(pkg) {
            pkg.dat <- dat[dat$package == pkg, ]
            if (log.count) {
              if (points) {
                plot(pkg.dat$date, pkg.dat[, y.nm], type = "o", xlab = "Date",
                  ylab = paste0("log10(", y.nm.case, ")"), log = "y",
                  xlim = xlim, ylim = ylim)
              } else {
                plot(pkg.dat$date, pkg.dat[, y.nm], type = "l", xlab = "Date",
                  ylab = paste0("log10(", y.nm.case, ")"), log = "y",
                  xlim = xlim, ylim = ylim)
              }
            } else {
              if (points) {
                plot(pkg.dat$date, pkg.dat[, y.nm], type = "o", xlab = "Date",
                  ylab = y.nm.case, xlim = xlim, ylim = ylim)
              } else {
                plot(pkg.dat$date, pkg.dat[, y.nm], type = "l", xlab = "Date",
                  ylab = y.nm.case, xlim = xlim, ylim = ylim)
              }
            }

            if (package.version) {
              if (dev.mode) p_v <- lapply(x$packages, packageHistory0)
              else p_v <- lapply(x$packages, packageHistory)

              invisible(lapply(p_v, function(dat) {
                axis(3, at = dat$Date, labels = dat$Version, cex.axis = 2/3,
                  padj = 0.9, col.axis = "red", col.ticks = "red")
                abline(v = dat$Date, lty = "dotted", col = "red")
              }))
            }

            if (r.version) {
              r_v <- rversions::r_versions()
              axis(3, at = as.Date(r_v$date),
                labels = paste("R", r_v$version), cex.axis = 2/3, padj = 0.9)
              abline(v = as.Date(r_v$date), lty = "dotted")
            }

            if (smooth) {
              lines(stats::lowess(pkg.dat$date, pkg.dat[, y.nm], f = f),
                col = "blue")
            }
            title(main = pkg)
          }))
          grDevices::devAskNewPage(ask = FALSE)

        } else {
          grDevices::devAskNewPage(ask = TRUE)
          invisible(lapply(x$package, function(pkg) {
            pkg.dat <- dat[dat$package == pkg, ]
            if (log.count) {
              if (points) {
                plot(pkg.dat$date, pkg.dat[, y.nm], type = "o", xlab = "Date",
                  ylab = paste0("log10(", y.nm.case, ")"), log = "y")
              } else {
                plot(pkg.dat$date, pkg.dat[, y.nm], type = "l", xlab = "Date",
                  ylab = paste0("log10(", y.nm.case, ")"), log = "y")
              }
            } else {
              if (points) {
                plot(pkg.dat$date, pkg.dat[, y.nm], type = "o", xlab = "Date",
                  ylab = y.nm.case)
              } else {
                plot(pkg.dat$date, pkg.dat[, y.nm], type = "l", xlab = "Date",
                  ylab = y.nm.case)
              }
            }

            if (package.version) {
              if (dev.mode) p_v <- lapply(x$packages, packageHistory0)
              else p_v <- lapply(x$packages, packageHistory)

              invisible(lapply(p_v, function(dat) {
                axis(3, at = dat$Date, labels = dat$Version, cex.axis = 2/3,
                  padj = 0.9, col.axis = "red", col.ticks = "red")
                abline(v = dat$Date, lty = "dotted", col = "red")
              }))
            }

            if (r.version) {
              r_v <- rversions::r_versions()
              axis(3, at = as.Date(r_v$date),
                labels = paste("R", r_v$version), cex.axis = 2/3, padj = 0.9)
              abline(v = as.Date(r_v$date), lty = "dotted")
            }

            if (smooth) {
              lines(stats::lowess(pkg.dat$date, pkg.dat[, y.nm], f = f),
                col = "blue")
            }
            title(main = pkg)
          }))
          grDevices::devAskNewPage(ask = FALSE)
        }
      }

    } else if (length(x$packages) == 1) {
      if (log.count) {
        if (points) {
          plot(dat$date, dat[, y.nm], type = "o", xlab = "Date",
            ylab = paste0("log10(", y.nm.case, ")"), log = "y")
        } else {
          plot(dat$date, dat[, y.nm], type = "l", xlab = "Date",
            ylab = paste0("log10(", y.nm.case, ")"), log = "y")
        }
      } else {
        if (points) {
          plot(dat$date, dat[, y.nm], type = "o", xlab = "Date",
            ylab = y.nm.case)
        } else {
          plot(dat$date, dat[, y.nm], type = "l", xlab = "Date",
            ylab = y.nm.case)
        }
      }

      if (package.version) {
        if (dev.mode) p_v <- lapply(x$packages, packageHistory0)
        else p_v <- lapply(x$packages, packageHistory)

        invisible(lapply(p_v, function(dat) {
          axis(3, at = dat$Date, labels = dat$Version, cex.axis = 2/3,
            padj = 0.9, col.axis = "red", col.ticks = "red")
          abline(v = dat$Date, lty = "dotted", col = "red")
        }))
      }

      if (r.version) {
        r_v <- rversions::r_versions()
        axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
          cex.axis = 2/3, padj = 0.9)
        abline(v = as.Date(r_v$date), lty = "dotted")
      }

      if (smooth) {
        lines(stats::lowess(dat$date, dat[, y.nm], f = f), col = "blue")
      }

      title(main = x$packages)
    }

  } else if (graphics == "ggplot2") {
    if (is.null(x$packages)) {
      p <- cranDownloadsPlot(x, statistic, graphics, points, log.count, smooth,
        se, f)
    } else {
      if (length(days.observed) == 1) {
        p <- ggplot(data = dat) +
             theme_bw() +
             theme(panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank()) +
             facet_wrap(~ date, nrow = 2)

        if (statistic == "count") {
          p <- p + geom_point(aes_string("count", "package"), size = 1.5)
        } else if (statistic == "cumulative") {
          p <- p + geom_point(aes_string("cumulative", "package"), size = 1.5)
        }

        if (log.count) p <- p + scale_x_log10() + xlab("log10(count)")

      } else if (length(days.observed) > 1) {
        if (statistic == "count") {
          p <- ggplot(data = dat, aes_string("date", "count"))
        } else if (statistic == "cumulative") {
          p <- ggplot(data = dat, aes_string("date", "cumulative"))
        }

        p <- p + geom_line(size = 0.5) +
          facet_wrap(~ package, nrow = 2) +
          theme_bw() +
          theme(panel.grid.minor = element_blank())

        if (points & log.count & smooth) {
          p <- p + geom_point() +
            scale_y_log10() +
            geom_smooth(method = "loess", formula = "y ~ x", se = se)
        } else if (points & log.count & !smooth) {
          p <- p + geom_point() +
            scale_y_log10()
        } else if (points & !log.count & smooth) {
          p <- p + geom_point() +
            geom_smooth(method = "loess", formula = "y ~ x", se = se)
        } else if (!points & log.count & smooth) {
          p <- p + scale_y_log10() +
            geom_smooth(method = "loess", formula = "y ~ x", se = se)
        } else if (!points & !log.count & smooth) {
          p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se)
        } else if (points & !log.count & !smooth) {
          p <- p + geom_point()
        } else if (!points & log.count & !smooth) {
          p <- p + scale_y_log10()
        }
      }
    }
    p
  }
}
