#' Daily package downloads from the RStudio CRAN mirror.
#'
#' Enhanced S3 implementation of cranlogs::cran_downloads().
#' @param packages A character vector, the packages to query,
#'   or \code{NULL} for a sum of downloads for all packages.
#'   Alternatively, it can also be \code{"R"}, to query downloads
#'   of R itself. \code{"R"} cannot be mixed with packages.
#' @param when \code{last-day}, \code{last-week} or \code{last-month}.
#'   If this is given, then \code{from} and \code{to} are ignored.
#' @param from Start date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
#' @param to End date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
#' @param check.cran Logical. Check if package exists.
#' @param check.archive Logical. Validate archived packages.
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
  to = NULL, check.cran = TRUE, check.archive = FALSE) {

  if (check.cran) {
    pkg.chk <- validatePackage(packages, check.archive = check.archive)
    if (is.list(pkg.chk)) {
      error <- paste(pkg.chk$invalid, collapse = ", ")
      if (length(pkg.chk$valid) == 0) {
        if (check.archive) {
          stop(error, ": misspelled or not on CRAN/Archive.")
        } else stop(error, ": misspelled or not on CRAN.")
      } else {
        if (check.archive) {
          warning(error, ": misspelled or not on CRAN/Archive.")
        } else warning(error, ": misspelled or not on CRAN.")
        packages <- pkg.chk$valid
      }
    }
  }

  # first.log <- as.Date("2012-10-01") # resolveDate() checks this.
  cal.date <- Sys.Date() - 1

  if (is.null(when) & is.null(from) & is.null(to)) {
    args <- list(packages = packages, from = cal.date, to = cal.date)

  } else if (!is.null(when)) {
    if (when %in% c("last-day", "last-week", "last-month")) {
      args <- list(packages = packages, when = when)
    } else stop('"when" must be "last-day", "last-week" or "last-month".')

  } else if (!is.null(from)) {
    start.date <- resolveDate(from, type = "from")

    if (!is.null(to)) {
      end.date <- resolveDate(to, type = "to")
    } else end.date <- cal.date

    if (start.date > end.date) stop('"from" must be <= "to".')
    args <- list(packages, from = start.date, to = end.date)
  }

  cranlogs.data <- do.call(cranlogs::cran_downloads, args)
  out <- list(packages = packages, cranlogs.data = cranlogs.data,
    when = args$when, from = args$from, to = args$to)
  class(out) <- "cranDownloads"
  out
}

#' Plot method for cranDownloads().
#'
#' @param x object.
#' @param graphics Character. NULL, "base" or "ggplot2".
#' @param points Character of Logical. Plot points. "auto", TRUE, FALSE.
#' @param log.count Logical. Logarithm of package downloads.
#' @param smooth Logical. Add smoother.
#' @param se Logical. Works only with graphics = "ggplot2".
#' @param f Numeric. stats::lowess() smoother window. For use with graphics = "base" only.
#' @param package.version Logical. Add latest package release dates.
#' @param r.version Logical. Add R release dates.
#' @param population.plot Logical. Plot population plot.
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

plot.cranDownloads <- function(x, graphics = NULL, points = "auto",
  log.count = FALSE, smooth = FALSE, se = FALSE, f = 1/3,
  package.version = FALSE, r.version = FALSE, population.plot = FALSE, ...) {

  if (is.logical(log.count) == FALSE) stop("log.count must be TRUE or FALSE.")
  if (is.logical(smooth) == FALSE) stop("smooth must be TRUE or FALSE.")
  if (is.logical(se) == FALSE) stop("se must be TRUE or FALSE.")
  if (is.numeric(f) == FALSE) stop("f must be numeric.")

  if (population.plot) {
     populationPlot(x, graphics = graphics, f = f)
  } else {
    dat <- x$cranlogs.data

    if (package.version) p_v <- packageCRAN(x$packages)
    if (r.version) r_v <- rversions::r_versions()

    days.observed <- unique(dat$date)

    if (points == "auto") {
    if (length(days.observed) <= 45) points <- TRUE else points <- FALSE
    } else if (is.logical(points) == FALSE) {
      stop('points must be "auto", TRUE, or FALSE.')
    }

    if (is.null(graphics)) {
      if (is.null(x$packages)) {
        graphics <- "base"
      } else if (length(x$packages) == 1 | length(unique(dat$date)) == 1) {
        graphics <- "base"
      } else graphics <- "ggplot2"
    } else {
      if (all(graphics %in% c("base", "ggplot2") == FALSE)) {
        stop('graphics must be "base" or "ggplot2"')
      }
    }

    if (graphics == "base") {
      if (is.null(x$packages)) {
        rDownloadsPlot(x, graphics, points, log.count, smooth, se, f)

      } else if (length(x$packages) > 1) {
        if (length(days.observed) == 1) {
          if (log.count) {
            dotchart(log10(dat$count), labels = dat$package,
              xlab = "log10(Count)", main = days.observed)
          } else {
            dotchart(dat$count, labels = dat$package, xlab = "count",
              main = days.observed)
          }

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
              axis(3, at = as.Date(p_v$date), labels = paste(p_v$version),
                cex.axis = 2/3, tick = FALSE, line = -2/3, col.axis = "red")
              abline(v = as.Date(p_v$date), lty = "dotted", col = "red")
            }

            if (r.version) {
              axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
                cex.axis = 2/3, tick = FALSE, line = -2/3)
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

      } else {
        if (length(days.observed) == 1) {
          if (x$packages == "R") {
            os <- unique(dat$os)

            count <- vapply(os, function(x) {
              sum(dat[dat$os == x, "count"])
            }, numeric(1L))

            if ("NA" %in% names(count) == FALSE) {
              count <- c(count, 0)
              names(count)[length(count)] <- "NA"
            }

            count <- count[order(names(count))]
            dotchart(sort(count), xlab = "count", main = days.observed)
          } else {
            if (log.count) {
              dotchart(log10(dat$count), labels = dat$package,
                xlab = "log10(Count)", main = days.observed)
            } else {
              dotchart(dat$count, labels = dat$package, xlab = "count",
                main = days.observed)
            }
          }
        } else {
          if (x$packages == "R") {
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
              lines(unique(dat$date), daily$osx, type = "o", pch = 0,
                col = "red")
              lines(unique(dat$date), daily$src, type = "o", pch = 2,
                col = "blue")
              lines(unique(dat$date), daily$`NA`, type = "o", pch = 3,
                col = "green")
              legend(x = "topleft",
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
              legend(x = "topleft",
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

            if (package.version) {
              axis(3, at = as.Date(p_v$date), labels = paste(p_v$version),
                cex.axis = 2/3, tick = FALSE, line = -2/3, col.axis = "red")
              abline(v = as.Date(p_v$date), lty = "dotted", col = "red")
            }

            if (r.version) {
              axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
                cex.axis = 2/3, tick = FALSE, line = -2/3)
              abline(v = as.Date(r_v$date), lty = "dotted")
            }

            title(main = "R Downloads")

          } else {
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

            if (smooth) {
              lines(stats::lowess(dat$date, dat$count, f = f), col = "blue")
            }

            if (package.version) {
              axis(3, at = as.Date(p_v$date), labels = paste(p_v$version),
                cex.axis = 2/3, tick = FALSE, line = -2/3, col.axis = "red")
              abline(v = as.Date(p_v$date), lty = "dotted", col = "red")
            }

            if (r.version) {
              axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
                cex.axis = 2/3, tick = FALSE, line = -2/3)
              abline(v = as.Date(r_v$date), lty = "dotted")
            }

            title(main = x$package)
          }
        }
      }
    } else if (graphics == "ggplot2") {

      if (is.null(x$packages)) {
        rDownloadsPlot(x, graphics, points, log.count, smooth, se, f)

      } else if (length(days.observed) == 1) {
        if ("R" %in% x$packages == FALSE) {
          p <- ggplot(data = dat) +
               geom_point(aes_string("count", "package")) +
               theme_bw() +
               theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor = element_blank()) +
               facet_wrap(~ date, ncol = 2)
        } else {
          dat2 <- as.data.frame(stats::xtabs(count ~ date + os, data = dat),
            stringsAsFactors = FALSE)
          names(dat2)[3] <- "count"
          dat2$date <- as.Date(dat2$date)
          p <- ggplot(data = dat2) +
               geom_point(aes_string("count", "os")) +
               theme_bw() +
               theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor = element_blank()) +
               facet_wrap(~ date, ncol = 2)
        }
        if (log.count) p + scale_x_log10() + xlab("log10(count)") else p

      } else {
        if ("R" %in% x$packages == FALSE) {
          p <- ggplot(data = dat, aes_string("date", "count")) +
            geom_line(size = 0.5) +
            facet_wrap(~ package, ncol = 2) +
            theme_bw() +
            theme(panel.grid.minor = element_blank())
        } else {
          dat2 <- as.data.frame(stats::xtabs(count ~ date + os, data = dat),
            stringsAsFactors = FALSE)
          names(dat2)[3] <- "count"
          dat2$date <- as.Date(dat2$date)
          p <- ggplot(data = dat2, aes_string("date", "count")) +
            geom_line(size = 0.5) +
            facet_wrap(~ os, ncol = 2) +
            theme_bw() +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(hjust = 0.5)) +
            ggtitle("R Downloads")
        }

        if (points & log.count & smooth) {
          p + geom_point() +
              scale_y_log10() +
              geom_smooth(method = "loess", formula = "y ~ x", se = se)
        } else if (points & log.count & !smooth) {
          p + geom_point() + scale_y_log10()
        } else if (points & !log.count & smooth) {
          p +  geom_point() + geom_smooth(method = "loess", formula = "y ~ x",
            se = se)
        } else if (!points & log.count & smooth) {
          p + scale_y_log10() + geom_smooth(method = "loess",
            formula = "y ~ x", se = se)
        } else if (!points & !log.count & smooth) {
          p + geom_smooth(method = "loess", formula = "y ~ x", se = se)
        } else if (points & !log.count & !smooth) {
          p + geom_point()
        } else if (!points & log.count & !smooth) {
          p + scale_y_log10()
        } else p
      }
    } else stop('graphics must be "base" or "ggplot2"')
  }
}

#' Print method for packageRank().
#' @param x object.
#' @param ... Additional parameters.
#' @export

print.cranDownloads <- function(x, ...) {
  print(x$cranlogs.data)
}

#' Summary method for packageRank().
#' @param object Object.
#' @param ... Additional parameters.
#' @export
#' @note This is useful for directly accessing the data frame.

summary.cranDownloads <- function(object, ...) {
  object$cranlogs.data
}

rDownloadsPlot <- function(x, graphics, points, log.count, smooth, se, f) {
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
      p +  geom_point() +
       geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (!points & log.count & smooth) {
      p + scale_y_log10() + geom_smooth(method = "loess", formula = "y ~ x",
        se = se)
    } else if (!points & !log.count & smooth) {
      p + geom_smooth(method = "loess", formula = "y ~ x", se = se)
    } else if (points & !log.count & !smooth) {
      p + geom_point()
    } else if (!points & log.count & !smooth) {
      p + scale_y_log10()
    } else p

  } else stop('graphics must be "base" or "ggplot2"')
}
