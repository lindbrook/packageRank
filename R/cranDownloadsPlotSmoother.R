addRPlotSmoother <- function(x, complete, f, span, pltfrm, pltfrm.col,
  statistic, wk1.backdate) {

  dat <- x$cranlogs.data

  if (any(dat$in.progress)) {
    invisible(lapply(seq_along(complete), function(i) {
      tmp <- complete[[i]]
      if (nrow(tmp) > 7) {
        smooth.data <- stats::loess(tmp[, statistic] ~ as.numeric(tmp$date), 
          span = span)
        x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
        lines(x.date, smooth.data$fitted, lwd = 1.5, col = pltfrm.col[i])
      } else {
        smooth.data <- stats::lowess(tmp$date, tmp[, statistic], f = f)
        lines(smooth.data, lwd = 1.5, col = pltfrm.col[i])
      }
    }))
  } else if (any(dat$partial)) {
    invisible(lapply(seq_along(complete), function(i) {
      tmp <- rbind(wk1.backdate[i, ], complete[[i]])
      if (nrow(tmp) > 7) {
        smooth.data <- stats::loess(tmp[, statistic] ~ as.numeric(tmp$date), 
          span = span)
        x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
        lines(x.date, smooth.data$fitted, lwd = 1.5, col =  pltfrm.col[i])
      } else {
        smooth.data <- stats::lowess(tmp$date, tmp[, statistic], f = f)
        lines(smooth.data$x, smooth.data$fitted, lwd = 1.5, col = pltfrm.col[i])
      }
    }))
  } else {
    if (any(packageRank::missing.dates %in% dat$date)) {
      dat <- dat[!dat$date %in% packageRank::missing.dates, ]
    }
    invisible(lapply(seq_along(pltfrm), function(i) {
      tmp <- dat[dat$platform == pltfrm[i], ]
      if (nrow(tmp) > 7) {
        smooth.data <- stats::loess(tmp[, statistic] ~
          as.numeric(tmp$date), span = span)
        x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
        lines(x.date, smooth.data$fitted, lwd = 1.5, col = pltfrm.col[i])
      } else {
        smooth.data <- stats::lowess(tmp$date, tmp[, statistic], f = f)
        lines(smooth.data, lwd = 1.5, col = pltfrm.col[i])
      }
    }))
  }
  
  if (nrow(dat) > 7) {
    title(sub = paste("loess span =", round(span, 2)), cex.sub = 0.9)
  } else {
    title(sub = paste("lowess f =", round(f, 2)), cex.sub = 0.9)
  }
}

addRTotPlotSmoother <- function(dat, complete, f, span, statistic,
  wk1.backdate) {

  if (any(dat$in.progress)) {
    tmp <- complete
    if (nrow(tmp) > 7) {
      smooth.data <- stats::loess(tmp[, statistic] ~
        as.numeric(tmp$date), span = span)
      x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
      lines(x.date, smooth.data$fitted, col = "blue", lwd = 1.25)
      title(sub = paste("loess span =", round(span, 2)), cex.sub = 0.9)
    } else {
      lines(stats::lowess(tmp$date, tmp[, statistic], f = f), col = "blue", 
        lwd = 1.25)
      title(sub = paste("lowess f =", round(f, 2)), cex.sub = 0.9)
    }
  } else if (any(dat$partial)) {
    tmp <- rbind(wk1.backdate, complete)
    if (nrow(tmp) > 7) {
      smooth.data <- stats::loess(tmp[, statistic] ~
        as.numeric(tmp$date), span = span)
      x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
      lines(x.date, smooth.data$fitted, col = "blue", lwd = 1.25)
      title(sub = paste("loess span =", round(span, 2)), cex.sub = 0.9)
    } else {
      lines(stats::lowess(tmp$date, tmp[, statistic], f = f), col = "blue",
        lwd = 1.25)
      title(sub = paste("lowess f =", round(f, 2)), cex.sub = 0.9)
    }
  } else {
    if (any(packageRank::missing.dates %in% dat$date)) {
      dat <- dat[!dat$date %in% packageRank::missing.dates, ]
    }

    if (nrow(dat) > 7) {
      smooth.data <- stats::loess(dat[, statistic] ~
        as.numeric(dat$date), span = span)
      x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
      lines(x.date, smooth.data$fitted, col = "blue", lwd = 1.25)
      title(sub = paste("loess span =", round(span, 2)), cex.sub = 0.9)
    } else {
      lines(stats::lowess(dat$date, dat[, statistic], f = f), col = "blue", 
        lwd = 1.25)
      title(sub = paste("lowess f =", round(f, 2)), cex.sub = 0.9)
    }
  }
}
