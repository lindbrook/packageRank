#' Polygon for missing log dates.
#'
#' Seven days: 8/25-26, 8/29-9/02 in 2025.
#' @param dat Object. Computed cranDownloads() data.
#' @param ylim Numeric. Vector of plot ylim.
#' @param log.y Logical. Logarithm of statistic.
#' @param col Character. Color.
#' @param alpha.f Numeric. Alpha level transparency.
#' @noRd

missingDatesPolygons <- function(dat, ylim, log.y = FALSE, col = "lightgray", 
  alpha.f = 0.5) {
    
  if (log.y) {
    y1 <- c(0.5, ylim[2] * 1.9)
    y2 <- rev(y1)
  } else {
    y1 <- c(ylim[2] - ylim[2] * 1.04, ylim[2] * 1.04)
    y2 <- rev(y1)
  }

  null.set <- expression(symbol("\306"))
  date.range <- range(dat$date)
  exp.dates <- seq.Date(from = date.range[1], to = date.range[2])
  obs.missing <- packageRank::missing.dates %in% exp.dates

  if (all(obs.missing)) {
    xs1 <- c(rep(packageRank::missing.dates[1], 2),
             rep(packageRank::missing.dates[2], 2))
    xs2 <- c(rep(packageRank::missing.dates[3], 2),
             rep(packageRank::missing.dates[7], 2))
    graphics::polygon(x = xs1, y = c(y1, y2), border = NA,
      col = grDevices::adjustcolor(col, alpha.f = alpha.f))
    graphics::polygon(x = xs2, y = c(y1, y2), border = NA,
      col = grDevices::adjustcolor(col, alpha.f = alpha.f))
    axis(3, at = mean(as.numeric(packageRank::missing.dates[1:2])),
      labels = null.set, cex.axis = 2/3, padj = 0.9)
    axis(3, at = mean(as.numeric(packageRank::missing.dates[3:7])),
      labels = null.set, cex.axis = 2/3, padj = 0.9)
  
  } else if (any(obs.missing == FALSE)) {
    missingA <- obs.missing[1:2]
    missingB <- obs.missing[3:7]

    if (all(missingA) & all(missingB == FALSE)) {
      xs1 <- c(rep(packageRank::missing.dates[1], 2),
               rep(packageRank::missing.dates[2], 2))
      graphics::polygon(x = xs1, y = c(y1, y2), border = NA,
        col = grDevices::adjustcolor(col, alpha.f = alpha.f))
      at1 <- mean(packageRank::missing.dates[1:2])
      axis(3, at = at1, labels = null.set, cex.axis = 2/3, padj = 0.9)
    
    } else if (all(missingA == FALSE) & all(missingB)) {
      xs2 <- c(rep(packageRank::missing.dates[3], 2),
               rep(packageRank::missing.dates[7], 2))
      graphics::polygon(x = xs2, y = c(y1, y2), border = NA,
        col = grDevices::adjustcolor(col, alpha.f = alpha.f))
      at2 <- packageRank::missing.dates[5]
      axis(3, at = at2, labels = null.set, cex.axis = 2/3, padj = 0.9)


    } else if (all(missingA) & any(missingB == FALSE)) {
      xs1 <- c(rep(packageRank::missing.dates[1], 2),
               rep(packageRank::missing.dates[2], 2))
      graphics::polygon(x = xs1, y = c(y1, y2), border = NA,
        col = grDevices::adjustcolor(col, alpha.f = alpha.f))
      at1 <- mean(packageRank::missing.dates[1:2])
      axis(3, at = at1, labels = null.set, cex.axis = 2/3, padj = 0.9)

      if (sum(missingB) > 1) {
        xs.trimmed <- packageRank::missing.dates[3:7][missingB]
        xs2 <- c(rep(xs.trimmed[1], 2),
                 rep(xs.trimmed[length(xs.trimmed)], 2))
        at2 <- mean(xs.trimmed)
        graphics::polygon(x = xs2, y = c(y1, y2), border = NA,
          col = grDevices::adjustcolor(col, alpha.f = alpha.f))
        axis(3, at = at2, labels = null.set, cex.axis = 2/3, padj = 0.9)
      } else if (sum(missingB) == 1) {
        at2 <- packageRank::missing.dates[3:7][missingB]
        abline(v = at2, col = col)
        axis(3, at = at2, labels = null.set, cex.axis = 2/3, padj = 0.9)
      }

    } else if (any(missingA == FALSE) & all(missingB)) {
      if (sum(missingA) == 1) {
        at1 <- packageRank::missing.dates[1:2][missingA]
        abline(v = at1, col = col)
        axis(3, at = at1, labels = null.set, cex.axis = 2/3, padj = 0.9)
      }
      
      xs2 <- c(rep(packageRank::missing.dates[3], 2),
               rep(packageRank::missing.dates[7], 2))
      graphics::polygon(x = xs2, y = c(y1, y2), border = NA,
        col = grDevices::adjustcolor(col, alpha.f = alpha.f))
      at2 <- packageRank::missing.dates[5]
      axis(3, at = at2, labels = null.set, cex.axis = 2/3, padj = 0.9)

    } else if (all(missingA == FALSE) & any(missingB == FALSE)) {
      if (sum(missingB) > 1) {
        xs.trimmed <- packageRank::missing.dates[3:7][missingB]
        xs2 <- c(rep(xs.trimmed[1], 2),
                 rep(xs.trimmed[length(xs.trimmed)], 2))
        at2 <- mean(xs.trimmed)
        graphics::polygon(x = xs2, y = c(y1, y2), border = NA,
          col = grDevices::adjustcolor(col, alpha.f = alpha.f))
        axis(3, at = at2, labels = null.set, cex.axis = 2/3, padj = 0.9)
      } else if (sum(missingB) == 1) {
        at2 <- packageRank::missing.dates[3:7][missingB]
        abline(v = at2, col = col)
        axis(3, at = at2, labels = null.set, cex.axis = 2/3, padj = 0.9)
      }

    } else if (any(missingA == FALSE) & all(missingB == FALSE)) {
      if (sum(missingA) == 1) {
        at1 <- packageRank::missing.dates[1:2][missingA]
        abline(v = at1, col = col)
        axis(3, at = at1, labels = null.set, cex.axis = 2/3, padj = 0.9)
      }

    } else if (any(missingA == FALSE) & any(missingB == FALSE)) {
      if (sum(missingA) == 1) {
        at1 <- packageRank::missing.dates[1:2][missingA]
        abline(v = at1, col = col)
        axis(3, at = at1, labels = null.set, cex.axis = 2/3, padj = 0.9)
      }

      if (sum(missingB) > 1) {
        xs.trimmed <- packageRank::missing.dates[3:7][missingB]
        xs2 <- c(rep(xs.trimmed[1], 2),
                 rep(xs.trimmed[length(xs.trimmed)], 2))
        at2 <- mean(xs.trimmed)
        graphics::polygon(x = xs2, y = c(y1, y2), border = NA,
          col = grDevices::adjustcolor(col, alpha.f = alpha.f))
        axis(3, at = at2, labels = null.set, cex.axis = 2/3, padj = 0.9)
      } else if (sum(missingB) == 1) {
        at2 <- packageRank::missing.dates[3:7][missingB]
        abline(v = at2, col = col)
        axis(3, at = at2, labels = null.set, cex.axis = 2/3, padj = 0.9)
      }
    }
  }
}

#' 'ggplot2' rectangle annotation layer for missing log dates.
#'
#' Seven days: 8/25-26, 8/29-9/02 in 2025.
#' @param p Object. 'ggplot2' plot object. 
#' @param ymin Numeric. For edge to edge polygons; 0 for y.log = TRUE, -Inf for y.log = FALSE.
#' @param polygon.adjustment Object. R list of djustements for missing polygons.
#' @noRd

gg_missingDatesPolygons <- function(p, ymin, polygon.adjustment = NULL) {
  rectA <- ggplot2::annotate(geom = "rect", 
                             xmin = packageRank::missing.dates[1], 
                             xmax = packageRank::missing.dates[2], 
                             ymin = ymin, ymax = Inf, color = NA,
                             fill = "gray", alpha = 0.5)

  rectB <- ggplot2::annotate(geom = "rect", 
                             xmin = packageRank::missing.dates[3], 
                             xmax = packageRank::missing.dates[7], 
                             ymin = ymin, ymax = Inf, color = NA,
                             fill = "gray", alpha = 0.5)

  if (is.null(polygon.adjustment)) {
    p + rectA + rectB
  
  } else if (all(polygon.adjustment$missingA) & 
             all(polygon.adjustment$missingB == FALSE)) {
    p + rectA
  
  } else if (all(polygon.adjustment$missingA == FALSE) & 
             all(polygon.adjustment$missingB)) {
    p + rectB
  
  } else if (all(polygon.adjustment$missingA) & 
             any(polygon.adjustment$missingB == FALSE)) {
    id.b <- which(polygon.adjustment$missingB) + 2L
    if (sum(polygon.adjustment$missingB) == 1) {
      p + rectA + 
        ggplot2::geom_vline(xintercept = packageRank::missing.dates[id.b],
                            colour = "gray", alpha = 0.5)
    } else {
      xmin <- min(packageRank::missing.dates[id.b])
      xmax <- max(packageRank::missing.dates[id.b])
      
      p + rectA + ggplot2::annotate(geom = "rect", 
                                    xmin = xmin, xmax = xmax, 
                                    ymin = ymin, ymax = Inf, 
                                    color = NA, fill = "gray", alpha = 0.5)
    }
  
  } else if (any(polygon.adjustment$missingA == FALSE) & 
             all(polygon.adjustment$missingB)) {
    id <- which(polygon.adjustment$missingA) # only 2 possibilities
    p + rectB + ggplot2::geom_vline(xintercept = packageRank::missing.dates[id],
                                    colour = "gray", alpha = 0.5)
  
  } else if (all(polygon.adjustment$missingA == FALSE) & 
             any(polygon.adjustment$missingB == FALSE)) {
    if (sum(polygon.adjustment$missingB) == 1) {
      id <- which(polygon.adjustment$missingB) + 2L
      p + ggplot2::geom_vline(xintercept = packageRank::missing.dates[id],
                              colour = "gray", alpha = 0.5)
    } else {
      id.b <- which(polygon.adjustment$missingB) + 2L
      xmin <- min(packageRank::missing.dates[id.b])
      xmax <- max(packageRank::missing.dates[id.b])
      p + ggplot2::annotate(geom = "rect", 
                            xmin = xmin, xmax = xmax, 
                            ymin = ymin, ymax = Inf, 
                            color = NA, fill = "gray", alpha = 0.5)
    }
  
  } else if (any(polygon.adjustment$missingA == FALSE) & 
             all(polygon.adjustment$missingB == FALSE)) {
    id <- which(polygon.adjustment$missingA) # only 2 possibilities
    p + ggplot2::geom_vline(xintercept = packageRank::missing.dates[id],
                            colour = "gray", alpha = 0.5)
  
  } else if (any(polygon.adjustment$missingA == FALSE) & 
             any(polygon.adjustment$missingB == FALSE)) {
    id.a <- which(polygon.adjustment$missingA) # only 2 possibilities
    id.b <- which(polygon.adjustment$missingB) + 2L
    if (sum(polygon.adjustment$missingB) == 1) { # 9/26-9/29
      p + ggplot2::geom_vline(xintercept = packageRank::missing.dates[id.a],
                              colour = "gray", alpha = 0.5) +
          ggplot2::geom_vline(xintercept = packageRank::missing.dates[id.b],
                              colour = "gray", alpha = 0.5)
    } else {
      xmin <- min(packageRank::missing.dates[id.b])
      xmax <- max(packageRank::missing.dates[id.b])
      p + ggplot2::geom_vline(xintercept = packageRank::missing.dates[id.a],
                              colour = "gray", alpha = 0.5) +
          ggplot2::annotate(geom = "rect",
                            xmin = xmin, xmax = xmax,
                            ymin = ymin, ymax = Inf,
                            color = NA, fill = "gray", alpha = 0.5)
    }
  }
}
