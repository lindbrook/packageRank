#' Daily package downloads from the RStudio CRAN mirror.
#'
#' S3 implementation of cranlogs::cran_downloads().
#' @param packages A character vector, the packages to query,
#'   or \code{NULL} for a sum of downloads for all packages.
#'   Alternatively, it can also be \code{"R"}, to query downloads
#'   of R itself. \code{"R"} cannot be mixed with packages.
#' @param when \code{last-day}, \code{last-week} or \code{last-month}.
#'   If this is given, then \code{from} and \code{to} are ignored.
#' @param start.year Numeric.
#' @param start.month Numeric.
#' @param start.day Numeric.
#' @param end.year Numeric.
#' @param end.month Numeric.
#' @param end.day Numeric.
#' @param from Start date, in \code{yyyy-mm-dd} format, or
#'   \code{last-day}.
#' @param to End date, in \code{yyyy-mm-dd} format, or
#'   \code{last-day}.
#' @export
#' @examples
#' \donttest{
#' cranDownloads(packages = "HistData")
#' cranDownloads(packages = "HistData", "last-week")
#' cranDownloads(packages = "HistData", "last-month")
#'
#' # year-to-date
#' cranDownloads(packages = "HistData", start.year = 2019)
#'
#' # first two weeks of January 2019
#' cranDownloads(packages = "HistData", from = "2019-01-07", to = "2019-01-14")
#' }

cranDownloads <- function(packages = NULL,
  when = c("last-day", "last-week", "last-month"),
  start.year = NULL, start.month = NULL, start.day = NULL,
  end.year = NULL, end.month = NULL, end.day = NULL, from = NULL, to = NULL) {

  cal.date <- Sys.Date() - 1

  if (!missing(when)) {
    args <- list(packages = packages, when = when)
  } else if (!missing(from) & !missing(to)) {
    from <- as.Date(from, optional = TRUE)
    to <- as.Date(to, optional = TRUE)
    if (!is.na(from) & !is.na(to)) {
      args <- list(packages, from = from, to = to)
    } else stop("Check dates!")
  } else {

    ## start date ##

    if (is.null(start.year) & is.null(start.month) & is.null(start.day)) {
      start.date <- cal.date

    } else if (!is.null(start.year) & is.null(start.month) &
      is.null(start.day)) {
        start.date <- as.Date(paste0(start.year, "-01-01"), optional = TRUE)

    } else if (!is.null(start.year) & !is.null(start.month) &
      is.null(start.day)) {

      if (start.month < 10) start.month <- paste0("0", start.month)
      else if (start.month > 12 | start.month < 1) stop("1 <= Month <= 12.")
        date.string <- paste0(start.year, "-", start.month, "-01")
        start.date <- as.Date(date.string, optional = TRUE)

     } else if (!is.null(start.year) & !is.null(start.month) &
         !is.null(start.day)) {

       if (start.month < 10) start.month <- paste0("0", start.month)
       else if (start.month > 12 | start.month < 10) stop("1 <= Month <= 12.")
       if (start.day < 10) start.day <- paste0("0", start.day)
       else if (start.day > 31 | start.day < 0) stop("1 <= Day <= 31.")

       date.sting <- paste0(start.year, "-", start.month ,"-", start.day)
       start.date <- as.Date(date.sting, optional = TRUE)
       if (is.na(start.date)) stop('Not a valid date! Check "start.day"')
     }

    ## end date ##

    if (is.null(end.year) & is.null(end.month) & is.null(end.day)) {
      end.date <- cal.date

    } else if (!is.null(end.year) & is.null(end.month) & is.null(end.day)) {
      end.date <- as.Date(paste0(end.year, "-12-31"), optional = TRUE)

    } else if (!is.null(end.year) & !is.null(end.month) & is.null(end.day)) {
      if (end.month < 10) end.month <- paste0("0", end.month)
      else if (end.month > 12 | end.month < 10) stop("1 <= Month <= 12.")

       end.candidates <- lapply(28:31, function(day) {
         paste0(end.year, "-", end.month , "-", day)
       })

      end.candidates <- do.call(c, end.candidates)
      end.selelct <- rev(end.candidates[!is.na(end.candidates)])[1]
      end.date <- as.Date(end.selelct, optional = TRUE)

    } else if (!is.null(end.year) & !is.null(end.month) & !is.null(end.day)) {
      if (end.month < 10) end.month <- paste0("0", end.month)
      else if (end.month > 12 | end.month < 10) stop("1 <= Month <= 12.")

      if (end.day < 10) end.day <- paste0("0", end.day)
      else if (end.day > 31 | end.day < 0) stop("1 <= Day <= 31.")

      date.sting <- paste0(end.year, "-", end.month ,"-", end.day)
      end.date <- as.Date(date.sting, optional = TRUE)
    }

    if (is.na(end.date)) {
      stop("Not a valid date!")
    } else if (start.date > end.date) {
      stop ('"start.date" <= "end.date".')
    } else args <- list(packages, from = start.date, to = end.date)
  }

  out <- list(packages = packages,
              cranlogs.data = do.call(cranlogs::cran_downloads, args))

  class(out) <- "cran_downloads"
  out
}

#' Plot method for cranDownloads().
#'
#' @param x object.
#' @param graphics Character. NULL, "base" or "ggplot2".
#' @param points Logical. Plot points.
#' @param log_count Logical. Logarithm of package downloads.
#' @param smooth Logical. Add smoother.
#' @param se Logical. Works only with graphics = "ggplot2".
#' @param f Numeric. stats::lowess() smoother window. For use with graphics = "base" only.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @export
#' @examples
#' \donttest{
#' plot(cranDownloads(packages = c("Rcpp", "rlang", "data.table")))
#' plot(cranDownloads(packages = c("Rcpp", "rlang", "data.table"), when = "last-month"))
#' plot(cranDownloads(packages = "R", from = "2019-05-01", to = "2019-05-01"))
#' plot(cranDownloads(packages = "R", when = "last-month"))
#' }

plot.cran_downloads <- function(x, graphics = NULL, points = TRUE,
  log_count = FALSE, smooth = FALSE, se = FALSE, f = 1/3, ...) {

  if (is.logical(log_count) == FALSE) stop("log_count must be TRUE or FALSE.")
  if (is.logical(smooth) == FALSE) stop("smooth must be TRUE or FALSE.")
  if (is.logical(se) == FALSE) stop("se must be TRUE or FALSE.")
  if (is.numeric(f) == FALSE) stop("f must be numeric.")

  dat <- x$cranlogs.data
  days.observed <- unique(dat$date)

  if (is.null(graphics)) {
    if (length(x$packages) == 1 | length(unique(dat$date)) == 1) {
      graphics <- "base"
    } else graphics <- "ggplot2"
  } else {
    if (all(graphics %in% c("base", "ggplot2") == FALSE))
    stop('graphics must be "base" or "ggplot2"')
  }

  if (graphics == "base") {
    if (length(x$packages) > 1) {
      if (length(days.observed) == 1) {
        if (log_count) {
          dotchart(log10(dat$count), labels = dat$package,
            xlab = "log10(Count)", main = days.observed)
        } else {
          dotchart(dat$count, labels = dat$package, xlab = "count",
            main = days.observed)
        }
      } else {
        invisible(lapply(x$package, function(pkg) {
          pkg.dat <- dat[dat$package == pkg, ]
          if (log_count) {
            if (points) {
              plot(pkg.dat$date, pkg.dat$count, type = "o", xlab = "Rank",
                ylab = "log10(Count)", log = "y")
            } else {
              plot(pkg.dat$date, pkg.dat$count, type = "l", xlab = "Rank",
                ylab = "log10(Count)", log = "y")
            }
          } else {
            if (points) {
              plot(pkg.dat$date, pkg.dat$count, type = "o", xlab = "Rank",
                ylab = "Count")
            } else {
              plot(pkg.dat$date, pkg.dat$count, type = "l", xlab = "Rank",
                ylab = "Count")
            }
          }
          if (smooth) {
            lines(stats::lowess(pkg.dat$date, pkg.dat$count, f = f),
              col = "blue")
          }
          title(main = pkg)
        }))
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
          plot(unique(dat$date), daily$win, type = "o", ylim = range(daily))
          lines(unique(dat$date), daily$osx, type = "o", pch = 0, col = "red")
          lines(unique(dat$date), daily$src, type = "o", pch = 2, col = "blue")
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
          if (log_count) {
            if (points) {
              plot(dat$date, dat$count, type = "o", xlab = "Rank",
                ylab = "log10(Count)", log = "y")
            } else {
              plot(dat$date, dat$count, type = "l", xlab = "Rank",
                ylab = "log10(Count)", log = "y")
            }
          } else {
            if (points) {
              plot(dat$date, dat$count, type = "o", xlab = "Rank",
                ylab = "Count")
            } else {
              plot(dat$date, dat$count, type = "l", xlab = "Rank",
                ylab = "Count")
            }
          }
          if (smooth) lines(stats::lowess(dat$date, dat$count, f = f),
            col = "blue")
          title(main = x$package)
        }
      }
    }
  } else if (graphics == "ggplot2") {
    if (length(days.observed) == 1) {
      p <- ggplot(dat) +
           geom_point(aes_string("count", "package")) +
           theme_bw() +
           theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor = element_blank()) +
           facet_wrap(~ date, ncol = 2)

      if (log_count) p + scale_x_log10() + xlab("log10(count)") else p

    } else {
      p <- ggplot(data = dat, aes_string("date", "count")) +
        geom_line(size = 0.5) +
        facet_wrap(~ package, ncol = 2) +
        theme_bw() +
        theme(panel.grid.minor = element_blank())

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

checkDate <- function(string, end.date = FALSE) {
  if (is.character(string) == FALSE) stop("string must a text string.")

  if ((nchar(string) == 10 | nchar(string) == 7) == FALSE) {
    stop('Format must be "yyyy-mm-dd" or "yyyy-mm".')
  }

  date.parts <- unlist(strsplit(string, "-"))

  if (date.parts[2] %in% c(paste0(0, 1:9), paste(10:12)) == FALSE) {
    stop("The month must be between 01 and 12.")
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
