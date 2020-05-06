#' Counts v. Rank Percentiles for 'cholera' for First Week of March 2020.
#'
#' Document code for blog graph.
#' @param package Character.
#' @param size.filter Logical.
#' @export

countsRanks <- function(package = "cholera", size.filter = FALSE) {
  Downloads <- summary(cranDownloads(packages = package, from = "2020-03-01",
    to = "2020-03-07", check.package = FALSE))$count
  Date <- as.Date(c("2020-03-01", "2020-03-02", "2020-03-03", "2020-03-04",
    "2020-03-05", "2020-03-06", "2020-03-07"))

  out <- list(package = package,
              Downloads = Downloads,
              Date = Date,
              percentiles = packageRank::blog.data$percentiles)

  class(out) <- "countsRanks"
  out
}

#' Plot method for countsRanks().
#'
#' @param x object.
#' @param ... Additional plotting parameters.
#' @export

plot.countsRanks <- function(x, ...) {
  Downloads <- x$Downloads
  Date <- x$Date
  percentiles <- x$percentiles
  par(mar = c(5, 4, 4, 4))
  plot(Date, Downloads, type = "o", pch = 0, bty = "n")
  par(new = TRUE)
  plot(Date, percentiles, axes = FALSE, bty = "n", type = "o", pch = 16,
    col= "red", xlab = NA, ylab = NA, lwd = 2)
  axis(4, at = pretty(range(percentiles)), col.axis = "red", col = "red")
  mtext("Rank Percentiles", side = 4, line = 3, col = "red")
  abline(h = 50, lty = "dotted", col = "red")
  title_a <- "Counts v. Rank Percentiles: '"
  title_b <- "' March 1-7, 2020"
  title(main = paste0(title_a, x$package, title_b))
  title(sub = paste("r =", round(stats::cor(Downloads, percentiles), 3)))
  legend(x = "topleft",
         legend = c("Percentile", "Count"),
         col = c("red", "black"),
         pch = c(15, 0),
         bg = "white",
         cex = 2/3,
         lwd = 1,
         title = NULL)
  par(mar = c(5, 4, 4, 2))
}
