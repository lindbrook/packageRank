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
#' \donttest{
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
      stop("R downloads cannot be mixed with package downloads.")
    }
  }

  if (!is.null(packages)) {
    if (!"R" %in% packages) {
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
    }
  }

  # first.log <- as.Date("2012-10-01") # resolveDate() checks this.
  cal.date <- Sys.Date() - 1

  if (is.null(when) & is.null(from) & is.null(to)) {
    args <- list(packages = packages, from = cal.date, to = cal.date)

  } else if (!is.null(when) & is.null(from) & is.null(to)) {
    if (when %in% c("last-day", "last-week", "last-month")) {
      args <- list(packages = packages, when = when)
    } else stop('"when" must be "last-day", "last-week" or "last-month".')

  } else if (is.null(when) & !is.null(from)) {
    start.date <- resolveDate(from, type = "from")

    if (!is.null(to)) {
      end.date <- resolveDate(to, type = "to")
    } else end.date <- cal.date

    if (start.date > end.date) stop('"from" must be <= "to".')
    args <- list(packages, from = start.date, to = end.date)

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
    out <- list(packages = packages, cranlogs.data = cranlogs.data,
      when = args$when, from = args$from, to = args$to)
  } else {
    cranlogs.data <- do.call(rbind, to.data)
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
#' @param graphics Character. "auto", "base" or "ggplot2".
#' @param points Character of Logical. Plot points. "auto", TRUE, FALSE.
#' @param log.count Logical. Logarithm of package downloads.
#' @param smooth Logical. Add smoother.
#' @param se Logical. Works only with graphics = "ggplot2".
#' @param f Numeric. stats::lowess() smoother window. For use with graphics = "base" only.
#' @param package.version Logical. Add latest package release dates.
#' @param r.version Logical. Add R release dates.
#' @param population.plot Logical. Plot population plot.
#' @param multi.plot Logical.
#' @param same.xy Logical. Use same scale for multiple packages when graphics = "base".
#' @param legend.loc Character.
#' @param dev.mode Logical. Use packageHistory0() to scrape CRAN.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @export
#' @examples
#' \donttest{
#' plot(cranDownloads(packages = c("Rcpp", "rlang", "data.table")))
#' plot(cranDownloads(packages = c("Rcpp", "rlang", "data.table"), when = "last-month"))
#' plot(cranDownloads(packages = "R", from = "2020-01-01", to = "2020-01-01"))
#' plot(cranDownloads(packages = "R", from = 2020))
#' }

plot.cranDownloads <- function(x, graphics = "auto", points = "auto",
  log.count = FALSE, smooth = FALSE, se = FALSE, f = 1/3,
  package.version = FALSE, r.version = FALSE, population.plot = FALSE,
  multi.plot = FALSE, same.xy = TRUE, legend.loc = "topleft",
  dev.mode = FALSE, ...) {

  if (graphics == "auto") {
    if (is.null(x$packages)) {
      graphics <- "base"
    } else if (length(x$packages) == 1) {
      graphics <- "base"
    } else if (length(x$package) > 1) {
      graphics <- "ggplot2"
    }
  }

  if (is.logical(log.count) == FALSE) stop("log.count must be TRUE or FALSE.")
  if (is.logical(smooth) == FALSE) stop("smooth must be TRUE or FALSE.")
  if (is.logical(se) == FALSE) stop("se must be TRUE or FALSE.")
  if (is.numeric(f) == FALSE) stop("f must be numeric.")
  if (package.version) {
    if (dev.mode) {
      p_v <- lapply(x$packages, packageHistory0)
    } else {
      p_v <- lapply(x$packages, packageHistory)
    }
  }
  if (r.version) r_v <- rversions::r_versions()

  dat <- x$cranlogs.data
  days.observed <- unique(dat$date)

  if (points == "auto") {
    if (length(days.observed) <= 45) points <- TRUE else points <- FALSE
  } else if (is.logical(points) == FALSE) {
    stop('points must be "auto", TRUE, or FALSE.')
  }

  if (population.plot) {
     populationPlot(x, graphics = graphics, f = f)
  } else if ("R" %in% x$packages) {
    rPlot(dat, graphics, days.observed, log.count, legend.loc, points, smooth,
      r.version, r_v)
  } else {
    if (multi.plot) {
      multiPlot(dat, x, graphics, days.observed, log.count, legend.loc)
    } else {
      singlePlot(dat, x, graphics, days.observed, points, smooth, se, f,
        log.count, package.version, p_v, r.version, r_v, same.xy)
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

rPlot <- function(dat, graphics, days.observed, log.count, legend.loc, points,
  smooth, r.version, r_v) {

  if (graphics == "base") {
    daily <- lapply(days.observed, function(day) {
      os <- unique(dat[dat$date == day, "os"])
      day.data <- dat[dat$date == day, ]
      tot.downloads <- sum(day.data$count)
      count <- vapply(os, function(x) {
        sum(day.data[day.data$os == x, "count"])
      }, numeric(1L))
      if ("NA" %in% names(count) == FALSE) {
        count <- c(count, 0)
        names(count)[length(count)] <- "NA"
      }
      count[order(names(count))]
    })

    daily <- as.data.frame(do.call(rbind, daily))

    if (points) {
      plot(unique(dat$date), daily$win, type = "o", ylim = range(daily),
        xlab = "Date", ylab = "Count")
      lines(unique(dat$date), daily$osx, type = "o", pch = 0, col = "red")
      lines(unique(dat$date), daily$src, type = "o", pch = 2, col = "blue")
      lines(unique(dat$date), daily$`NA`, type = "o", pch = 3, col = "green")
      legend(x = legend.loc,
             legend = c("win", "mac", "src", "NA"),
             col = c("black", "red", "blue", "green"),
             pch = c(1, 0, 2, 3),
             bg = "white",
             cex = 2/3,
             title = "Platform",
             lwd = 1)
    } else {
      plot(unique(dat$date), daily$win, type = "l", ylim = range(daily),
        xlab = "Date", ylab = "Count")
      lines(unique(dat$date), daily$osx, col = "red")
      lines(unique(dat$date), daily$src, col = "blue")
      lines(unique(dat$date), daily$`NA`, col = "green")
      legend(x = legend.loc,
             legend = c("win", "mac", "src", "NA"),
             col = c("black", "red", "blue", "green"),
             bg = "white",
             cex = 2/3,
             title = "Platform",
             lwd = 1)
    }

    if (smooth) {
      lines(stats::lowess(unique(dat$date), daily$win))
      lines(stats::lowess(unique(dat$date), daily$osx), col = "red")
      lines(stats::lowess(unique(dat$date), daily$src), col = "blue")
      lines(stats::lowess(unique(dat$date), daily$`NA`), col = "green")
    }

    if (r.version) {
      axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
        cex.axis = 2/3, padj = 0.9)
      abline(v = as.Date(r_v$date), lty = "dotted")
    }

    title(main = "R Downloads")

  } else if (graphics == "ggplot2") {
    dat2 <- as.data.frame(stats::xtabs(count ~ date + os, data = dat),
      stringsAsFactors = FALSE)
    names(dat2)[3] <- "count"
    dat2$date <- as.Date(dat2$date)
    ggplot(data = dat2, aes_string("date", "count")) +
      geom_line(size = 0.5) +
      facet_wrap(~ os, ncol = 2) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      ggtitle("R Downloads")
  }
}

multiPlot <- function(dat, x, graphics, days.observed, log.count, legend.loc) {
  if (graphics == "base") {
    if (length(days.observed) == 1) {
      if (log.count) {
        dotchart(log10(dat$count), labels = dat$package,
          xlab = "log10(Count)", main = days.observed)
      } else {
        dotchart(dat$count, labels = dat$package, xlab = "count",
          main = days.observed)
      }
    } else if (length(days.observed) > 1) {
      if (length(x$packages) > 8) {
        stop('Currently, use <= 8 packages when graphics = "base".')
      } else {
        # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
        # http://jfly.iam.u-tokyo.ac.jp/color/
        # The palette with grey:
        # cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
        #   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        token <- c(0, 2:7)
        plot(dat[dat$package == x$packages[1], c("date", "count")],
          ylim = range(dat$count), type = "o", main = "Package Downloads")
        invisible(lapply(seq_along(x$packages)[-1], function(i) {
          lines(dat[dat$package == x$packages[i], c("date", "count")],
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
          colour = "package")) + xlab("log10(count)")
      }
      p <- p + geom_point(size = 2) +
        geom_hline(yintercept = c(1, 2), linetype = "dotted") +
        theme(panel.grid.minor = element_blank())
    } else if (length(days.observed) > 1) {
      p <- ggplot(data = dat,
        aes_string("date", "count", colour = "package")) +
        geom_line() +
        geom_point() +
        theme(panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        ggtitle("Package Downloads")
    }
    p
  }
}

cranDownloadsPlot <- function(x, graphics, points, log.count, smooth, se, f,
  r.version, r_v) {

  dat <- x$cranlogs.data

  if (graphics == "base") {
    if (log.count) {
      if (points) {
        plot(dat$date, dat$count, type = "o", xlab = "Date",
          ylab = "log10(Count)", log = "y")
      } else {
        plot(dat$date, dat$count, type = "l", xlab = "Date",
          ylab = "log10(Count)", log = "y")
      }
    } else {
      if (points) {
        plot(dat$date, dat$count, type = "o", xlab = "Date",
          ylab = "Count")
      } else {
        plot(dat$date, dat$count, type = "l", xlab = "Date",
          ylab = "Count")
      }
    }

    if (r.version) {
      axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
        cex.axis = 2/3, padj = 0.9)
      abline(v = as.Date(r_v$date), lty = "dotted")
    }

    if (smooth) {
      lines(stats::lowess(dat$date, dat$count, f = f),
        col = "blue")
    }

    title(main = "Total Package Downloads")

  } else if (graphics == "ggplot2") {
    p <- ggplot(data = dat, aes_string("date", "count")) +
      geom_line(size = 0.5) +
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

  } else stop('graphics must be "base" or "ggplot2"')
}

singlePlot <- function(dat, x, graphics, days.observed, points, smooth, se, f,
  log.count, package.version, p_v, r.version, r_v, same.xy) {

  if (graphics == "base") {
    if (is.null(x$packages)) {
      cranDownloadsPlot(x, graphics, points, log.count, smooth, se, f,
        r.version, r_v)

    } else if (length(x$packages) > 1) {
      if (length(days.observed) == 1) {
        if (log.count) {
          dotchart(log10(dat$count), labels = dat$package,
            xlab = "log10(Count)", main = days.observed)
        } else {
          dotchart(dat$count, labels = dat$package, xlab = "count",
            main = days.observed)
        }
      } else if (length(days.observed) > 1) {

        if (same.xy) {
          xlim <- range(x$cranlogs.data$date)
          ylim <- range(x$cranlogs.data$count)
          grDevices::devAskNewPage(ask = TRUE)

          invisible(lapply(x$package, function(pkg) {
            pkg.dat <- dat[dat$package == pkg, ]
            if (log.count) {
              if (points) {
                plot(pkg.dat$date, pkg.dat$count, type = "o", xlab = "Date",
                  ylab = "log10(Count)", log = "y", xlim = xlim, ylim = ylim)
              } else {
                plot(pkg.dat$date, pkg.dat$count, type = "l", xlab = "Date",
                  ylab = "log10(Count)", log = "y", xlim = xlim, ylim = ylim)
              }
            } else {
              if (points) {
                plot(pkg.dat$date, pkg.dat$count, type = "o", xlab = "Date",
                  ylab = "Count", xlim = xlim, ylim = ylim)
              } else {
                plot(pkg.dat$date, pkg.dat$count, type = "l", xlab = "Date",
                  ylab = "Count", xlim = xlim, ylim = ylim)
              }
            }

            if (package.version) {
              invisible(lapply(p_v, function(dat) {
                axis(3, at = dat$Date, labels = dat$Version, cex.axis = 2/3,
                  padj = 0.9, col.axis = "red", col.ticks = "red")
                abline(v = dat$Date, lty = "dotted", col = "red")
              }))
            }

            if (r.version) {
              axis(3, at = as.Date(r_v$date),
                labels = paste("R", r_v$version), cex.axis = 2/3, padj = 0.9)
              abline(v = as.Date(r_v$date), lty = "dotted")
            }

            if (smooth) {
              lines(stats::lowess(pkg.dat$date, pkg.dat$count, f = f),
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
                plot(pkg.dat$date, pkg.dat$count, type = "o", xlab = "Date",
                  ylab = "log10(Count)", log = "y")
              } else {
                plot(pkg.dat$date, pkg.dat$count, type = "l", xlab = "Date",
                  ylab = "log10(Count)", log = "y")
              }
            } else {
              if (points) {
                plot(pkg.dat$date, pkg.dat$count, type = "o", xlab = "Date",
                  ylab = "Count")
              } else {
                plot(pkg.dat$date, pkg.dat$count, type = "l", xlab = "Date",
                  ylab = "Count")
              }
            }

            if (package.version) {
              invisible(lapply(p_v, function(dat) {
                axis(3, at = dat$Date, labels = dat$Version, cex.axis = 2/3,
                  padj = 0.9, col.axis = "red", col.ticks = "red")
                abline(v = dat$Date, lty = "dotted", col = "red")
              }))
            }

            if (r.version) {
              axis(3, at = as.Date(r_v$date),
                labels = paste("R", r_v$version), cex.axis = 2/3, padj = 0.9)
              abline(v = as.Date(r_v$date), lty = "dotted")
            }

            if (smooth) {
              lines(stats::lowess(pkg.dat$date, pkg.dat$count, f = f),
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
          plot(dat$date, dat$count, type = "o", xlab = "Date",
            ylab = "log10(Count)", log = "y")
        } else {
          plot(dat$date, dat$count, type = "l", xlab = "Date",
            ylab = "log10(Count)", log = "y")
        }
      } else {
        if (points) {
          plot(dat$date, dat$count, type = "o", xlab = "Date",
            ylab = "Count")
        } else {
          plot(dat$date, dat$count, type = "l", xlab = "Date",
            ylab = "Count")
        }
      }

      if (package.version) {
        invisible(lapply(p_v, function(dat) {
          axis(3, at = dat$Date, labels = dat$Version, cex.axis = 2/3,
            padj = 0.9, col.axis = "red", col.ticks = "red")
          abline(v = dat$Date, lty = "dotted", col = "red")
        }))
      }

      if (r.version) {
        axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
          cex.axis = 2/3, padj = 0.9)
        abline(v = as.Date(r_v$date), lty = "dotted")
      }

      if (smooth) {
        lines(stats::lowess(dat$date, dat$count, f = f), col = "blue")
      }

      title(main = x$packages)
    }

  } else if (graphics == "ggplot2") {
    if (is.null(x$packages)) {
      p <- cranDownloadsPlot(x, graphics, points, log.count, smooth, se, f)
    } else {
      if (length(days.observed) == 1) {
        p <- ggplot(data = dat) +
             geom_point(aes_string("count", "package")) +
             theme_bw() +
             theme(panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank()) +
             facet_wrap(~ date, ncol = 2)

        if (log.count) p <- p + scale_x_log10() + xlab("log10(count)")

      } else if (length(days.observed) > 1) {
        p <- ggplot(data = dat, aes_string("date", "count")) +
          geom_line(size = 0.5) +
          facet_wrap(~ package, ncol = 2) +
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
