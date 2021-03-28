#' Sample Weekly CRAN Downloads Data.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param start.yr Numeric or Integer.
#' @param n Numeric or Integer. Number of weeks (samples).
#' @export

weeklyDownloads <- function(start.yr = 2013, n = 50) {
  first.day <- as.Date(paste0(start.yr, "-01-01"))
  last.wk <- seq.Date(logDate() - 7, logDate(), by = "days")
  last.wk.day <- weekdays(last.wk)
  last.day <- last.wk[last.wk.day == "Sunday"] - 6

  log.dates <- seq.Date(first.day, last.day, by = "days")
  mon <- log.dates[weekdays(log.dates) == "Monday"]
  mon.smpl <- sort(sample(mon, n))

  dwnlds <- lapply(mon.smpl, function(x) {
    x <- cranDownloads(from = x, to = x + 6)[["cranlogs.data"]]
    x$day <- weekdays(x$date, abbreviate = TRUE)
    x$year <- as.numeric(format(x$date, "%Y"))
    x$day.id <- 1:7
    x$week.id <- x$date[1]
    x$percent <- 100 * x$count / sum(x$count)
    x
  })

  out <- do.call(rbind, dwnlds)
  class(out) <- c("weeklyDownloads", class(out))
  out
}

#' Plot method for annualDownloads().
#'
#' @param x object.
#' @param statistic Character. "count" or "percent".
#' @param aggregate Logical.
#' @param nrow Numeric. Number of rows for ggplot2 facets.
#' @param ... Additional plotting parameters.
#' @export

plot.weeklyDownloads <- function(x, statistic = "percent", aggregate = TRUE,
  nrow = 3L, ...) {

  if (aggregate) {
    if (statistic == "count") {
      agg.data <- data.frame(day = unique(x$day), day.id = 1:7,
        percent = tapply(x$count, x$day.id, mean))
      plot(agg.data$day.id, agg.data$percent, type = "o", xaxt = "n",
        xlab = "Day", ylab = "Count",
        main = "Mean Daily Count of Weekly CRAN Downloads",
        sub = paste("n =", length(unique(x$week.id)), "weeks"))
      axis(1, at = 1:7, labels = agg.data$day)

    } else if (statistic == "percent") {
      agg.data <- data.frame(day = unique(x$day), day.id = 1:7,
        percent = tapply(x$percent, x$day.id, mean))
      plot(agg.data$day.id, agg.data$percent, type = "o", xaxt = "n",
        xlab = "Day", ylab = "Percent",
        main = "Mean Daily Percent of Weekly CRAN Downloads",
        sub = paste("n =", length(unique(x$week.id)), "weeks"))
      axis(1, at = 1:7, labels = agg.data$day)

    } else stop('statistic must be "count" or "percent".')

  } else {
    if (statistic == "count") {
      p <- ggplot(data = x, aes_string(x = "day.id", y = "count")) +
        ylab("Count") +
        facet_wrap(~ week.id, nrow = nrow, scales = "free_y") +
        ggtitle("Daily Count of Weekly CRAN Downloads")

    } else if (statistic == "percent") {
      p <- ggplot(data = x, aes_string(x = "day.id", y = "percent")) +
        ylab("Percent") +
        facet_wrap(~ week.id, nrow = nrow) +
        ggtitle("Percent of Weekly CRAN Downloads")

    } else stop('statistic must be "count" or "percent".')

    p + geom_line() +
      geom_point() +
      scale_x_continuous(breaks = seq(2, 6, 2),
                         labels = c("Tue", "Thu", "Sat")) +
      xlab("Day") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  }
}
