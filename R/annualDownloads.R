#' Count Total CRAN Download.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param start.yr Numeric or Integer.
#' @param end.yr Numeric or Integer.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

annualDownloads <- function(start.yr = 2015, end.yr = 2020, multi.core = TRUE) {
  cores <- multiCore(multi.core)

  dwnlds <- parallel::mclapply(start.yr:end.yr, function(x) {
    cranDownloads(from = x, to = x)$cranlogs.data
  }, mc.cores = cores)

  annual.data <- lapply(dwnlds, function(x) {
    x$year <- as.numeric(format(x$date, "%Y"))
    x$day.mo <- format(x$date, "%d %b")
    x
  })

  out <- do.call(rbind, annual.data)
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
#' @param nrow Numeric. Number of rows for ggplot2 facets.
#' @param ... Additional plotting parameters.
#' @export

plot.annualDownloads <- function(x, nrow = 2, ...) {
  day.month <- dayMonth()
  p <- ggplot(data = x, aes_string(x = "date.id", y = "count")) +
  geom_line(colour = "gray") +
  facet_wrap(~ year, nrow = nrow, scales = "free_y") +
  scale_y_continuous(trans = "log10") +
  geom_smooth(method = "loess", formula = "y ~ x", se = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(1, 122, 245),
                     labels = day.month$date.nm[c(1, 122, 245)]) +
  geom_vline(xintercept = c(122, 245), colour = "red", linetype = "dashed") +
  xlab("Date") +
  ylab("Log Downloads")
  p
}

dayMonth <- function() {
  tmp <- seq.Date(as.Date("2016-1-1"), as.Date("2016-12-31"), by = "days")
  date.nm <- format(tmp, "%d %b")
  data.frame(date.id = seq_along(date.nm), date.nm = date.nm)
}
