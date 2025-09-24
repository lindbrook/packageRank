#' Sample Weekly CRAN Downloads Data.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param start.yr Numeric or Integer.
#' @param n Numeric or Integer. Number of weeks (samples).
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

weeklyDownloads <- function(start.yr = 2013, n = 50, multi.core = FALSE) {
  cores <- multiCore(multi.core)
  first.day <- as.Date(paste0(start.yr, "-01-01"))
  last.wk <- seq.Date(logDate() - 7, logDate(), by = "days")
  last.wk.day <- weekdays(last.wk)
  last.day <- max(last.wk[last.wk.day == "Sunday"] - 6)

  log.dates <- seq.Date(first.day, last.day, by = "days")
  mon <- log.dates[weekdays(log.dates) == "Monday"]
  mon.smpl <- sort(sample(mon, n))

  dwnlds <- parallel::mclapply(mon.smpl, function(x) {
    x <- cranDownloads(from = x, to = x + 6)[["cranlogs.data"]]
    x$day <- weekdays(x$date, abbreviate = TRUE)
    x$year <- as.numeric(format(x$date, "%Y"))
    x$day.id <- 1:7
    x$week.id <- x$date[1]
    x$percent <- 100 * x$count / sum(x$count)
    x
  }, mc.cores = cores)

  out <- do.call(rbind, dwnlds)
  class(out) <- c("weeklyDownloads", class(out))
  out
}

#' Plot method for weeklyDownloads().
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

  days <- unique(x$day)
  day.id <- unique(x$day.id)
  smpl.size <- length(unique(x$week.id))

  if (aggregation == "day") {
    if (typical.value == "mean") {
      typ.val <- tapply(x[, statistic], x$day.id, mean)
    } else if (typical.value == "median") {
      typ.val <- tapply(x[, statistic], x$day.id, stats::median)
    } else stop('typical.value must be "mean" or "median".')

    if (statistic == "count") {
      title.main <- "Daily Count of CRAN Downloads"
    } else if (statistic == "percent") {
      title.main <- "Percent of Weekly CRAN Downloads by Day"
    } else stop('statistic must be "count" or "percent".')

    plot(x$day.id, x[, statistic], xaxt = "n", xlab = "Day",
      ylab = tools::toTitleCase(statistic))
    lines(day.id, typ.val, col = "red", lwd = 1.5)
    axis(1, at = seq_along(days), labels = days)
    title(main = title.main, sub = paste(smpl.size, "Randomly Sampled Weeks"))

  } else if (aggregation == "week") {
    if (statistic == "count") {
      p <- ggplot2::ggplot(data = x, ggplot2::aes(x = .data$day.id, 
          y = .data$count)) +
        ggplot2::facet_wrap(ggplot2::vars(.data$week.id), nrow = nrow, 
          scales = "free_y") +
        ggplot2::labs(title = "Daily Count of Weekly CRAN Downloads")
    } else if (statistic == "percent") {
      p <- ggplot2::ggplot(data = x, ggplot2::aes(x = .data$day.id, 
          y = .data$percent)) +
        ggplot2::facet_wrap(ggplot2::vars(.data$week.id), nrow = nrow) +
        ggplot2::labs(title = "Percent of Weekly CRAN Downloads")
    } else stop('statistic must be "count" or "percent".')

    p + ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(breaks = c(2, 4, 6),
                                  labels = days[c(2, 4, 6)]) +
      ggplot2::labs(x = "Day") +
      ggplot2::labs(y = tools::toTitleCase(statistic)) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5))

  } else stop('aggregation must be "day" or "week".')
}
