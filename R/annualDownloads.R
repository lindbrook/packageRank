#' Count Total CRAN Download.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param start.yr Numeric or Integer.
#' @param end.yr Numeric or Integer.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

annualDownloads <- function(start.yr = 2013, end.yr = 2020, multi.core = TRUE) {
  cores <- multiCore(multi.core)

  dwnlds <- parallel::mclapply(start.yr:end.yr, function(x) {
    x <- cranDownloads(from = x, to = x)$cranlogs.data
    x$year <- as.numeric(format(x$date, "%Y"))
    x$day.mo <- format(x$date, "%d %b")
    x$percent <- 100 * x$count / sum(x$count)
    x
  }, mc.cores = cores)

  out <- do.call(rbind, dwnlds)
  day.month <- dayMonth()
  out <- merge(out, day.month, by.x = "day.mo", by.y = "date.nm")
  out <- out[order(out$year, out$date.id), ]
  row.names(out) <- NULL
  class(out) <- c("annualDownloads", class(out))
  out
}

#' Plot method for annualDownloads().
#'
#' @param x object.
#' @param statistic Character. "count" or "percent".
#' @param pool.obs Logical.
#' @param log.y Logical. Base 10 logarithm of y-axis.
#' @param nrow Numeric. Number of rows for ggplot2 facets.
#' @param smooth Logical. Add smoother. 2/3 is built-in default.
#' @param span Numeric. Smoothing parameter for geom_smooth(); c.f. stats::loess(span).
#' @param ... Additional plotting parameters.
#' @export

plot.annualDownloads <- function(x, statistic = "count", pool.obs = FALSE,
  log.y = TRUE, nrow = 3, smooth = TRUE, span = 3/4, ...) {

  day.month <- dayMonth()
  outliers <- outlierDays(x, c("2014-11-17", "2018-10-21", "2020-02-29"))

  if (pool.obs) {
    if (statistic == "count") {
      p <- ggplot(data = x, aes_string(x = "date", y = "count"))
      y.var <- "Downloads"
    } else if (statistic == "percent") {
      p <- ggplot(data = x, aes_string(x = "date", y = "percent"))
      y.var <- "Percent"
    } else stop('statistic must be "count" or "percent".')


  } else {
    if (statistic == "count") {
      p <- ggplot(data = x, aes_string(x = "date.id", y = "count"))
      y.var <- "Downloads"
    } else if (statistic == "percent") {
      p <- ggplot(data = x, aes_string(x = "date.id", y = "percent"))
      y.var <- "Percent"
    } else stop('statistic must be "count" or "percent".')

    p <- p + facet_wrap(~ year, nrow = nrow, scales = "free_y") +
      scale_x_continuous(breaks = c(1, 122, 245),
                         labels = day.month$date.nm[c(1, 122, 245)]) +
      geom_vline(xintercept = c(122, 245),
                 colour = grDevices::adjustcolor("red", alpha.f = 0.5),
                 linetype = "dashed")
  }

  p <- p + geom_line(colour = "gray") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    xlab("Date") +
    geom_point(data = outliers, shape = 15, colour = "red") +
    ggtitle("Total CRAN Package Downloads")

  if (log.y) {
    p <- p + scale_y_continuous(trans = "log10") + ylab(paste("Log", y.var))
  } else {
    p <- p + ylab(y.var)
  }

  if (smooth) {
    p <- p +
      geom_smooth(method = "loess", formula = "y ~ x", se = FALSE, span = span)
  }

  p
}

dayMonth <- function() {
  tmp <- seq.Date(as.Date("2016-1-1"), as.Date("2016-12-31"), by = "days")
  date.nm <- format(tmp, "%d %b")
  data.frame(date.id = seq_along(date.nm), date.nm = date.nm)
}

outlierDays <- function(x, dates) {
  dts <- as.Date(dates)
  vars <- c("date", "date.id", "count", "percent")
  out <- x[x$date %in% dts, vars]
  out$year <- as.numeric(format(out$date, "%Y"))
  out
}
