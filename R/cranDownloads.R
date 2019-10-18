#' Daily package downloads from the RStudio CRAN mirror.
#'
#' S3 implementation of cranlogs::cran_downloads().
#' @param packages A character vector, the packages to query,
#'   or \code{NULL} for a sum of downloads for all packages.
#'   Alternatively, it can also be \code{"R"}, to query downloads
#'   of R itself. \code{"R"} cannot be mixed with packages.
#' @param when \code{last-day}, \code{last-week} or \code{last-month}.
#'   If this is given, then \code{from} and \code{to} are ignored.
#' @param from Start date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
#' @param to End date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
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
#' # February to March 2019
#' cranDownloads(packages = "HistData", from = "2019-02", to = "2019-03")
#'
#' # 2019 year-to-date
#' cranDownloads(packages = "HistData", from = 2019)
#' }

cranDownloads <- function(packages = NULL, when = NULL, from = NULL,
  to = NULL) {

  first.log <- as.Date("2012-10-01") # first log on RStudio CRAN mirror.
  cal.date <- Sys.Date() - 1

  if (is.null(when) & is.null(from) & is.null(to)) {
    args <- list(packages = packages, from = cal.date, to = cal.date)

  } else if (!is.null(when)) {
    if (when %in% c("last-day", "last-week", "last-month")) {
      args <- list(packages = packages, when = when)
    } else stop('"when" must be "last-day", "last-week" or "last-month".')

  } else if (!is.null(from)) {
    date.err.msg <- 'Invalid date or format: "yyyy-mm-dd", "yyyy-mm" or "yyyy".'

    if (!is.null(from)) {
      if (nchar(from) == 10L & grepl("-", from)) {
        start.date <- as.Date(from, optional = TRUE)
      } else if (nchar(from) == 7L & grepl("-", from)) {
        start.date <- dayOfMonth(from, first.log)
      } else if (nchar(from) == 4L) {
        start.date <- as.Date(paste0(from, "-01-01"), optional = TRUE)
      } else stop(date.err.msg)
      if (is.na(start.date)) stop(date.err.msg)
    }

    if (!is.null(to)) {
      if (nchar(to) == 10L & grepl("-", to)) {
        end.date <- as.Date(to, optional = TRUE)
      } else if (nchar(to) == 7L & grepl("-", to)) {
        end.date <- dayOfMonth(to, first.log, end.date = TRUE)
      } else if (nchar(to) == 4L) {
        end.date <- as.Date(paste0(to, "-12-31"), optional = TRUE)
        if (end.date > cal.date) end.date <- cal.date
      } else stop(date.err.msg)
      if (is.na(end.date)) stop(date.err.msg)
    } else end.date <- cal.date

    if (start.date > end.date) stop ('"from" must be <= "to".')

    if (start.date < as.Date(first.log)) {
      warning(paste0('RStudio CRAN logs begin on ', first.log, "."))
      start.date <- first.log
    }

    if (end.date > cal.date) {
      warning("Future dates truncated.")
      end.date <- cal.date
    }

    args <- list(packages, from = start.date, to = end.date)
  }

  cranlogs.data <- do.call(cranlogs::cran_downloads, args)
  out <- list(packages = packages, cranlogs.data = cranlogs.data)
  class(out) <- "cran_downloads"
  out
}

#' Plot method for cranDownloads().
#'
#' @param x object.
#' @param graphics Character. NULL, "base" or "ggplot2".
#' @param points Character of Logical. Plot points. "auto", TRUE, FALSE.
#' @param log_count Logical. Logarithm of package downloads.
#' @param smooth Logical. Add smoother.
#' @param se Logical. Works only with graphics = "ggplot2".
#' @param f Numeric. stats::lowess() smoother window. For use with graphics = "base" only.
#' @param r.version Logical. Add R release dates.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @export
#' @examples
#' \donttest{
#' plot(cranDownloads(packages = c("Rcpp", "rlang", "data.table")))
#' plot(cranDownloads(packages = c("Rcpp", "rlang", "data.table"), when = "last-month"))
#' plot(cranDownloads(packages = "R", from = "2019-05-01", to = "2019-05-01"))
#' plot(cranDownloads(packages = "R", from = 2019))
#' }

plot.cran_downloads <- function(x, graphics = NULL, points = "auto",
  log_count = FALSE, smooth = FALSE, se = FALSE, f = 1/3, r.version = FALSE,
  ...) {

  if (is.logical(log_count) == FALSE) stop("log_count must be TRUE or FALSE.")
  if (is.logical(smooth) == FALSE) stop("smooth must be TRUE or FALSE.")
  if (is.logical(se) == FALSE) stop("se must be TRUE or FALSE.")
  if (is.numeric(f) == FALSE) stop("f must be numeric.")

  dat <- x$cranlogs.data
  r_v <- rversions::r_versions()
  days.observed <- unique(dat$date)

  if (points == "auto") {
    if (length(days.observed) <= 31) points <- TRUE else points <- FALSE
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
    if (all(graphics %in% c("base", "ggplot2") == FALSE))
    stop('graphics must be "base" or "ggplot2"')
  }

  if (graphics == "base") {
    if (is.null(x$packages)) {
      rDownloadsPlot(x, graphics, points, log_count, smooth, se, f)
    } else if (length(x$packages) > 1) {
      if (length(days.observed) == 1) {
        if (log_count) {
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
          if (log_count) {
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
          if (log_count) {
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
            lines(unique(dat$date), daily$osx, type = "o", pch = 0, col = "red")
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

          if (r.version) {
            axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
              cex.axis = 2/3, tick = FALSE, line = -2/3)
            abline(v = as.Date(r_v$date), lty = "dotted")
          }

          title(main = "R Downloads")

        } else {
          if (log_count) {
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
      rDownloadsPlot(x, graphics, points, log_count, smooth, se, f)

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

      if (log_count) p + scale_x_log10() + xlab("log10(count)") else p

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

      if (points & log_count & smooth) {
        p + geom_point() +
            scale_y_log10() +
            geom_smooth(method = "loess", se = se)
      } else if (points & log_count & !smooth) {
        p + geom_point() + scale_y_log10()
      } else if (points & !log_count & smooth) {
        p +  geom_point() + geom_smooth(method = "loess", se = se)
      } else if (!points & log_count & smooth) {
        p + scale_y_log10() + geom_smooth(method = "loess", se = se)
      } else if (!points & !log_count & smooth) {
        p + geom_smooth(method = "loess", se = se)
      } else if (points & !log_count & !smooth) {
        p + geom_point()
      } else if (!points & log_count & !smooth) {
        p + scale_y_log10()
      } else p
    }
  } else stop('graphics must be "base" or "ggplot2"')
}

#' Print method for packageRank().
#' @param x object.
#' @param ... Additional parameters.
#' @export

print.cran_downloads <- function(x, ...) {
  print(x$cranlogs.data)
}

#' Summary method for packageRank().
#' @param object Object.
#' @param ... Additional parameters.
#' @export
#' @note This is useful for directly accessing the data frame.

summary.cran_downloads <- function(object, ...) {
  object$cranlogs.data
}

dayOfMonth <- function(string, first.log, end.date = FALSE) {
  if (is.character(string) == FALSE) stop("string must a text string.")

  if (nchar(string) != 7 | (grepl("-", string) == FALSE)) {
    stop('Format must be "yyyy-mm".')
  } else {
    date.parts <- unlist(strsplit(string, "-"))
    if (date.parts[2] %in% c(paste0(0, 1:9), paste(10:12)) == FALSE) {
      stop("Month must be between 01 and 12.")
    }
    if (date.parts[1] < data.table::year(first.log)) {
      warning(paste0('RStudio CRAN logs begin on ', first.log, "."))
    }
  }

  if (end.date) {
    end.candidates <- lapply(28:31, function(day) {
      as.Date(paste0(string , "-", day), optional = TRUE)
    })
    end.candidates <- do.call(c, end.candidates)
    end.selelct <- rev(end.candidates[!is.na(end.candidates)])[1]
    out <- as.Date(end.selelct, optional = TRUE)
  } else {
    out <- as.Date(paste0(string , "-01"), optional = TRUE)
  }

  out
}

rDownloadsPlot <- function(x, graphics, points, log_count, smooth, se, f) {
  dat <- x$cranlogs.data

  if (graphics == "base") {
    if (log_count) {
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

    if (points & log_count & smooth) {
      p + geom_point() +
          scale_y_log10() +
          geom_smooth(method = "loess", se = se)
    } else if (points & log_count & !smooth) {
      p + geom_point() + scale_y_log10()
    } else if (points & !log_count & smooth) {
      p +  geom_point() + geom_smooth(method = "loess", se = se)
    } else if (!points & log_count & smooth) {
      p + scale_y_log10() + geom_smooth(method = "loess", se = se)
    } else if (!points & !log_count & smooth) {
      p + geom_smooth(method = "loess", se = se)
    } else if (points & !log_count & !smooth) {
      p + geom_point()
    } else if (!points & log_count & !smooth) {
      p + scale_y_log10()
    } else p

  } else stop('graphics must be "base" or "ggplot2"')
}
