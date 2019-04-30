#' Daily package downloads from the RStudio CRAN mirror.
#'
#' S3 implementation of cranlogs::cran_downloads().
#' @param packages A character vector, the packages to query,
#'   or \code{NULL} for a sum of downloads for all packages.
#'   Alternatively, it can also be \code{"R"}, to query downloads
#'   of R itself. \code{"R"} cannot be mixed with packages.
#' @param when \code{last-day}, \code{last-week} or \code{last-month}.
#'   If this is given, then \code{from} and \code{to} are ignored.
#' @param from Start date, in \code{yyyy-mm-dd} format, or
#'   \code{last-day}. It is ignored if \code{when} is given.
#' @param to End date, in \code{yyyy-mm-dd} format, or
#'   \code{last-day}. It is ignored if \code{when} is given.
#' @export

cran_downloads2 <- function(packages = NULL,
  when = c("last-day", "last-week", "last-month"), from = "last-day",
  to = "last-day") {

  if (!missing(when)) {
    args <- list(packages = packages, when = when)
  } else {
    args <- list(packages, from = from, to = to)
  }

  out <- list(packages = packages,
              cranlogs.data = do.call(cranlogs::cran_downloads, args))

  class(out) <- "cranlogs"
  out
}

#' Plot method for cran_downloads2().
#'
#' @param x object.
#' @param graphics_pkg Character. "base" or "ggplot2".
#' @param points Logical
#' @param log_y Logical
#' @param smooth Logical.
#' @param se Logical.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @import graphics ggplot2
#' @importFrom ggplot2 ggplot aes_string scale_y_log10 geom_point geom_line facet_wrap theme
#' @export

plot.cranlogs <- function(x, graphics_pkg = "ggplot2", points = TRUE,
  log_y = FALSE, smooth = FALSE, se = FALSE, ...) {

  dat <- x$cranlogs.data
  days.observed <- unique(dat$date)

  if (graphics_pkg == "base") {
    if (length(x$packages) > 1) {
      if (length(days.observed) == 1) {
        dotchart(dat$count, labels = dat$package, xlab = "count",
          main = days.observed)
      } else {
        invisible(lapply(x$package, function(pkg) {
          pkg.dat <- dat[dat$package == pkg, ]
          if (log_y) {
            plot(pkg.dat$date, pkg.dat$count, type = "o", xlab = "Rank",
              ylab = "log10(Count)", log = "y")
          } else {
            plot(pkg.dat$date, pkg.dat$count, type = "o", xlab = "Rank",
              ylab = "Count")
          }
          if (smooth) {
            lines(stats::lowess(pkg.dat$date, pkg.dat$count), col = "blue")
          }
          title(main = pkg)
        }))
      }
    } else {
      if (length(days.observed) == 1) {
        dotchart(dat$count, labels = dat$package, xlab = "count",
          main = days.observed)
      } else {
        if (log_y) {
          plot(dat$date, dat$count, type = "o", xlab = "Rank",
            ylab = "log10(Count)", log = "y")
        } else {
          plot(dat$date, dat$count, type = "o", xlab = "Rank", ylab = "Count")
        }
        if (smooth) lines(stats::lowess(dat$date, dat$count), col = "blue")
        title(main = x$package)
      }
    }

  } else if (graphics_pkg == "ggplot2") {
    if (length(days.observed) == 1) {
      ggplot(dat) +
        geom_point(aes_string("count", "package")) +
        theme_bw() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank()) +
        facet_wrap(~ date, ncol = 2)
    } else {
      p <- ggplot(data = dat, aes_string("date", "count")) +
        geom_line(size = 0.5) +
        facet_wrap(~ package, ncol = 2) +
        theme_bw() +
        theme(panel.grid.minor = element_blank())

      if (points & log_y & smooth) {
        p + geom_point() +
            scale_y_log10() +
            geom_smooth(method = "loess", se = se)
      } else if (points & log_y & !smooth) {
        p + geom_point() + scale_y_log10()
      } else if (points & !log_y & smooth) {
        p +  geom_point() + geom_smooth(method = "loess", se = se)
      } else if (!points & log_y & smooth) {
        p + scale_y_log10() + geom_smooth(method = "loess", se = se)
      } else if (!points & !log_y & smooth) {
        p + geom_smooth(method = "loess", se = se)
      } else if (points & !log_y & !smooth) {
        p + geom_point()
      } else if (!points & log_y & !smooth) {
        p + scale_y_log10()
      } else p
    }
  } else stop('graphics_pkg must be "base" or "ggplot2"')
}

#' Print method for packageRank().
#' @param x object.
#' @param ... Additional parameters.
#' @export

print.cranlogs <- function(x, ...) {
  print(x$cranlogs.data)
}

#' Summary method for packageRank().
#' @param object Object.
#' @param ... Additional parameters.
#' @export

summary.cranlogs <- function(object, ...) {
  object$cranlogs.data
}
