#' Count Total CRAN Download.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param start.yr Numeric or Integer.
#' @param end.yr Numeric or Integer.
#' @note A way around Gateway Timeout (HTTP 504). This takes a while since it computes each year separately.s
#' @noRd

annualDownloads <- function(start.yr = 2013, end.yr = 2023) {
  dwnlds <- lapply(start.yr:end.yr, function(yr) {
    x <- cranDownloads(from = yr, to = yr)$cranlogs.data
    names(x)[names(x) == "cumulative"] <- "cumulative.ct"
    x$percent <- 100 * x$count / sum(x$count)
    x$cumulative.pct <- cumsum(x$percent)
    x
  })
  
  out <- do.call(rbind, dwnlds)
  out$year <- as.integer(format(out$date, "%Y"))
  out$month <- as.integer(format(out$date, "%m"))
  class(out) <- c("annualDownloads", class(out))
  out
}

#' Plot method for annualDownloads().
#'
#' @param x object.
#' @param statistic Character. "count" or "percent".
#' @param pool Logical. Pool annual data into single time series.
#' @param log.y Logical. Base 10 logarithm of y-axis.
#' @param sep.y Logical. Separate, independent y-scales for each panel.
#' @param nrow Numeric. Number of rows for ggplot2 facets.
#' @param smooth Logical. Add smoother (loess). 
#' @param f Numeric. Parameter for lowess.
#' @param span Numeric. Smoothing parameter for geom_smooth(), which uses loess.
#' @param points Logical.
#' @param line.col Character. Color of line
#' @param ... Additional plotting parameters.
#' @noRd

plot.annualDownloads <- function(x, statistic = "count", pool = TRUE, 
  log.y = FALSE, sep.y = FALSE, nrow = 3, smooth = TRUE, f = 1/4, span = 3/4, 
  points = FALSE, line.col = "gray", ...) {

  if (!statistic %in% c("count", "percent")) {
    stop('statistic must be "count" or "percent".', call. = FALSE)
  }
 
  x <- x[x$count != 0,]
  outliers <- as.Date(c("2014-11-17", "2018-10-21", "2020-02-29"))
  y.nm.case <- tools::toTitleCase(statistic)
  
  if (pool) {
    if (log.y) {
      plot(x$date, x[, statistic], type = "l", col = line.col, xlab = "Date", 
           ylab = paste("log10", y.nm.case), log = "y")
    } else {
      plot(x$date, x[, statistic], type = "l", col = line.col, xlab = "Date", 
           ylab = y.nm.case)
    }
    
    if (points) points(x$date, x[, statistic])
    
    if (any(outliers %in% x$date)) {
      obs.outlier <- outliers[outliers %in% x$date]
      points(obs.outlier, x[x$date %in% obs.outlier, statistic], col = "red")
    }
  
    if (smooth) {
      lines(stats::lowess(x$date, x[, statistic], f = f), col = "blue", 
        lwd = 1.5)
    }

    title(main = "Total CRAN Package Downloads")
  } else {
    annual <- x
    annual$obs <- seq_along(annual$date)
   
    run.lengths <- rle(quarters(annual$date))
    last.obs <- length(run.lengths$lengths)

    quarter.data <- data.frame(quarter = run.lengths$value, 
      id = c(1, cumsum(run.lengths$lengths[-last.obs]) + 1))
    
    quarter.data <- quarter.data[quarter.data$quarter != "Q1", ]

    day.id <- unlist(lapply(unique(annual$year), function(yr) {
      seq_along(annual$year[annual$year == yr])
    }))
    
    annual$day.id <- day.id
    
    quarter.labels <- format(annual$date[quarter.data$id], "%b")
    quarter.labels <- quarter.labels[quarter.labels != "Q1"]
    
    if (statistic == "count") {
      p <- ggplot2::ggplot(data = annual, ggplot2::aes(x = day.id, 
             y = .data$count))

    } else if (statistic == "percent") {
      p <- ggplot2::ggplot(data = annual, ggplot2::aes(x = day.id, 
             y = .data$percent))
    }
    
    p <- p + ggplot2::geom_line(colour = "gray") +
      ggplot2::scale_x_continuous(breaks = annual$obs[quarter.data$id], 
                                  labels = quarter.labels) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::xlab("Date") +
      ggplot2::ggtitle("Total CRAN Package Downloads")

    if (points) p <- p + ggplot2::geom_point()
    
    if (log.y) {
      p <- p + ggplot2::scale_y_continuous(transform = "log10") + 
               ggplot2::ylab(paste("log10", y.nm.case))
    } else {
      p <- p + ggplot2::ylab(y.nm.case)
    }

    if (smooth) {
      p <- p + ggplot2::geom_smooth(method = "loess", formula = "y ~ x", 
        se = FALSE, span = span)
    }
    
    if (sep.y) {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$year), nrow = nrow,
                                   scales = "free_y")
    } else {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$year), nrow = nrow)
    }
    
    outlier.data <- annual[annual$date %in% outliers, ]
    p + ggplot2::geom_point(data = outlier.data, 
          ggplot2::aes(x = .data$day.id, y = .data$count), shape = 1, 
            col = "red")
  }
}

