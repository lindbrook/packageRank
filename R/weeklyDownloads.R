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
#' @param typical.value Character. "mean" or "median".
#' @param aggregation Character. "week" or "day".
#' @param nrow Numeric. Number of rows for ggplot2 facets.
#' @param ... Additional plotting parameters.
#' @export
#' @examples
#' \dontrun{
#' plot(weeklyDownloads())
#' plot(weeklyDownloads(n = 9), aggregation = "week")
#' }

plot.weeklyDownloads <- function(x, statistic = "percent", aggregation = "day",
  typical.value = "mean", nrow = 3L, ...) {

  if (aggregation == "day") {
    days <- unique(x$day)
    day.id <- unique(x$day.id)
    if (statistic == "count") {
      plot(x$day.id, x$count, xaxt = "n", xlab = "Day", ylab = "Count")
      if (typical.value == "mean") {
       typ.val <- tapply(x$count, x$day.id, mean)
      } else if (typical.value == "median") {
       typ.val <- tapply(x$count, x$day.id, stats::median)
      }
      lines(day.id, typ.val, col = "red")
      axis(1, at = seq_along(days), labels = days)
      title(main = "Daily Count of CRAN Downloads",
            sub = paste(length(unique(x$week.id)), "Randomly Sampled Weeks"))
    } else if (statistic == "percent") {
      plot(x$day.id, x$percent, xaxt = "n", xlab = "Day", ylab = "Percent")
      if (typical.value == "mean") {
       typ.val <- tapply(x$percent, x$day.id, mean)
      } else if (typical.value == "median") {
       typ.val <- tapply(x$percent, x$day.id, stats::median)
      }
      lines(day.id, typ.val, col = "red")
      axis(1, at = seq_along(days), labels = days)
      title(main = "Percent of Weekly CRAN Downloads by Day",
            sub = paste(length(unique(x$week.id)), "Randomly Sampled Weeks"))
    } else stop('statistic must be "count" or "percent".')

  } else if (aggregation == "week") {
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
  } else stop('aggregation must be "day" or "week".')
}
