#' Polygon for missing log dates.
#'
#' Seven days: 8/25-26, 8/29-9/02.
#' @param ylim Numeric. Vector of plot ylim.
#' @param log.y Logical. Logarithm of statistic.
#' @param col Character. Color.
#' @param alpha.f Numeric. Alpha level transparency.
#' @noRd

missingDatesPolygons <- function(ylim, log.y = FALSE, col = "lightgray", 
  alpha.f = 0.5) {
    
  if (log.y) {
    y1 <- c(0.5, ylim[2] * 1.9)
    y2 <- rev(y1)
  } else {
    y1 <- c(ylim[2] - ylim[2] * 1.04, ylim[2] * 1.04)
    y2 <- rev(y1)
  }

  xs1 <- c(rep(packageRank::missing.dates[1], 2),
           rep(packageRank::missing.dates[2], 2))
  xs2 <- c(rep(packageRank::missing.dates[3], 2),
           rep(packageRank::missing.dates[7], 2))

  graphics::polygon(x = xs1, y = c(y1, y2), border = NA,
    col = grDevices::adjustcolor(col, alpha.f = alpha.f))
  graphics::polygon(x = xs2, y = c(y1, y2), border = NA,
    col = grDevices::adjustcolor(col, alpha.f = alpha.f))
  
  null.set <- expression(symbol("\306"))

  axis(3, at = mean(as.numeric(packageRank::missing.dates[1:2])),
    labels = null.set, cex.axis = 2/3, padj = 0.9)
  axis(3, at = mean(as.numeric(packageRank::missing.dates[3:7])),
    labels = null.set, cex.axis = 2/3, padj = 0.9)
}
