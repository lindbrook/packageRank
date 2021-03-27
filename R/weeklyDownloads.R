#' Sample Weekly CRAN Downloads Data.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param start.yr Numeric or Integer.
#' @param n Numeric or Integer. Number of weeks (samples).
#' @export

weeklyDownloads <- function(start.yr = 2013, n = 9) {
  first <- as.Date(paste0(start.yr, "-01-01"))
  last.wk <- seq.Date(logDate() - 7, logDate(), by = "days")
  last.wk.day <- weekdays(last.wk)
  last <- last.wk[last.wk.day == "Sunday"] - 6

  tmp <- seq.Date(first, last, by = "days")
  mon <- tmp[weekdays(tmp) == "Monday"]
  mon.smpl <- sort(sample(mon, n))

  dwnlds <- lapply(mon.smpl, function(x) {
    x <- cranDownloads(from = x, to = x + 6)[["cranlogs.data"]]
    x$day <- weekdays(x$date)
    x$year <- as.numeric(format(x$date, "%Y"))
    x$day.id <- 1:7
    x$week.id <- x$date[1]
    x
  })

  out <- do.call(rbind, dwnlds)
  class(out) <- c("weeklyDownloads", class(out))
  out
}

#' Plot method for annualDownloads().
#'
#' @param x object.
#' @param nrow Numeric. Number of rows for ggplot2 facets.
#' @param ... Additional plotting parameters.
#' @export

plot.weeklyDownloads <- function(x, nrow = 3L, ...) {
  ggplot(data = x, aes_string(x = "day.id", y = "count")) +
  geom_line() +
  geom_point() +
  facet_wrap(~ week.id, nrow = nrow, scales = "free_y") +
  xlab("Day") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(2, 6, 2),
                     labels = c("Tue", "Thu", "Sat"))
}
