#' Inflation plots of effects of "small" downloads and prior versions for October 2019: 'cholera', 'ggplot2', and 'VR'.
#'
#' Document code for blog graph.
#' @param package Character.
#' @param filter Character. Size, version, or size and version
#' @param legend.loc Character. Location of legend.
#' @export

inflationPlot <- function(package = "cholera", filter = "size",
  legend.loc = "topleft") {

  if (package %in% c("cholera", "ggplot2", "VR") == FALSE) {
    stop('package must be "cholera", "ggplot2", or "VR".')
  }

  if (!filter %in% c("size", "version", "size.version")) {
    stop('Filter must be "size", "version", or "size.version".')
  }

  pt1 <- paste0("0", 1:9)
  oct.root <- "2019-10-"
  oct <- as.Date(c(paste0(oct.root, pt1), paste0(oct.root, 10:31)))
  id <- which(weekdays(oct, abbreviate = TRUE) == "Wed")

  dat <- packageRank::blog.data[[paste0(package, ".data")]]

  plot(oct, dat$ct, type = "o", col = "red", pch = 15,
    ylim = range(dat[, 1:4]), xlab = "Date", ylab = "Count")
  filter.id <- paste0("ct.", tolower(filter))
  lines(oct, dat[, filter.id], type = "o", col = "black", pch = 16)
  lines(stats::lowess(oct, dat$ct), lty = "dotted", col = "red")
  lines(stats::lowess(oct, dat[, filter.id]), lty = "dotted")
  abline(v = oct[id], col = "gray", lty = "dotted")
  axis(3, at = oct[id], labels = rep("W", length(id)), cex.axis = 0.5,
    col.ticks = "black")

  if (filter == "size") {
    title(main = paste0(package, ": Size"))
    sel <- "ct.size"
  } else if (filter == "version") {
    title(main = paste0(package, ": Version"))
    sel <- "ct.version"
  } else if (filter == "size.version") {
    title(main = paste0(package, ": Size & Version"))
    sel <- "ct.size.version"
  }

  tot <- colSums(dat)
  ptA <- paste0("Downloads: all = ", format(tot["ct"], big.mark = ","),
    "; filtered = ")
  ptB <- paste0("% | ", unique(dat$version.ct), " versions")
  delta.pct <- round(100 * (tot["ct"] - tot[sel]) / tot[sel], 1)
  title(sub = paste0(ptA, format(tot[sel], big.mark = ","), "; inflation = ",
    format(delta.pct, big.mark = ","), ptB))
  legend(x = legend.loc,
         legend = c("all", "filtered"),
         col = c("red", "black"),
         pch = c(15, 16),
         bg = "white",
         cex = 2/3,
         lwd = 1,
         title = NULL)
}
