addSmoother <- function(x, complete, current.wk, f, span, wk1, y.nm) {
  dat <- x$cranlogs.data
  if (nrow(dat) > 7) {
    if (any(dat$in.progress)) {
      smooth.data <- stats::loess(complete[, y.nm] ~
        as.numeric(complete$date), span = span)
    } else if (any(dat$partial)) {
      smooth.data <- smoothMonthData(x, complete, current.wk, f, span, wk1,
        y.nm)
    } else {
      smooth.data <- stats::loess(dat[, y.nm] ~ as.numeric(dat$date),
        span = span)
    }
  } else {
    if (any(dat$in.progress)) {
      smooth.data <- stats::lowess(complete$date, complete[, y.nm], f = f)
    } else if (any(dat$partial)) {
      smooth.data <- smoothMonthData(x, current.wk, f, span, wk1)
    } else {
      smooth.data <- stats::lowess(dat$date, dat[, y.nm], f = f)
    }
  }
  if (nrow(dat) > 7) {
    x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
    lines(x.date, smooth.data$fitted, col = "blue", lwd = 1.25)
  } else {
    lines(smooth.data, col = "blue", lwd = 1.25)
  }
}

smoothMonthData <- function(x, complete, current.wk, f, span, wk1, y.nm) {
  dat <- x$cranlogs.data
  wk1.start <- dat$date[1]
  if (weekdays(x$from) == "Sunday") {
    wk1.partial <- dat[dat$date == wk1.start, ]
    wk1.backdate <- wk1.partial
  } else {
    sel <- dat$partial & dat$date == wk1.start
    wk1.partial <- dat[sel, ]
    wk1.backdate <- wk1.partial
    wk1.backdate$count <- sum(wk1$cranlogs.data$count)
    wk1.backdate$cumulative <- wk1.backdate$count
  }
  tmp <- rbind(wk1.backdate, complete)
  if (weekdays(x$last.obs.date) == "Saturday") tmp <- rbind(tmp, current.wk)
  if (nrow(dat) > 7) {
    stats::loess(tmp[, y.nm] ~ as.numeric(tmp$date), span = span)
  } else {
    stats::lowess(tmp$date, tmp[, y.nm], f = f)
  }
}

addSinglePlotSmoother <- function(x, complete, current.wk, f, span,
  wk1.backdate, y.nm) {
  dat <- x$cranlogs.data
  last.obs.date <- x$last.obs.date
  if (any(dat$in.progress)) {
    smooth.data <- complete
    if (nrow(smooth.data) > 7) {
      smooth.data <- stats::loess(smooth.data[, y.nm] ~
        as.numeric(smooth.data$date), span = span)
    }
  } else if (any(dat$partial)) {
    if (any(dat$partial)) {
      tmp <- rbind(wk1.backdate, complete)
      if (weekdays(last.obs.date) == "Saturday") {
        tmp <- rbind(tmp, current.wk)
      }
      if (nrow(dat) > 7) {
        smooth.data <- stats::loess(tmp[, y.nm] ~ as.numeric(tmp$date),
          span = span)
      } else {
        smooth.data <- stats::lowess(tmp$date, tmp[, y.nm], f = f)
      }
    }
  } else {
    if (nrow(dat) > 7) {
      smooth.data <- stats::loess(dat[, y.nm] ~ as.numeric(dat$date),
        span = span)
    }
  }
  if (nrow(dat) > 7) {
    x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
    lines(x.date, smooth.data$fitted, col = "blue", lwd = 1.25)
  } else {
    lines(smooth.data$date, smooth.data[, y.nm], col = "blue")
  }
}

addMultiPlotSmoother <- function(i, x, complete, cbPalette, f, span,
  statistic, vars, wk1.backdate, tmp) {

  dat <- x$cranlogs.data
  current.wk <- dat[nrow(dat), ]
  last.obs.date <- x$last.obs.date

  if (any(dat$in.progress)) {
    smooth.data <- complete
    if (nrow(smooth.data) > 7) {
      smooth.data <- stats::loess(smooth.data[, statistic] ~
        as.numeric(smooth.data$date), span = span)
      x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
      lines(x.date, smooth.data$fitted, col = cbPalette[i])
    } else {
      lines(stats::lowess(smooth.data$date, smooth.data[, statistic], f = f),
        col = cbPalette[i])
    }
  } else if (any(dat$partial)) {
    tmp <- rbind(wk1.backdate, complete)
    if (weekdays(last.obs.date) == "Saturday") {
      tmp <- rbind(tmp, current.wk)
    }
    if (nrow(dat) > 7) {
      smooth.data <- stats::loess(tmp[, statistic] ~
        as.numeric(tmp$date), span = span)
      x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
      lines(x.date, smooth.data$fitted, col = cbPalette[i])
    } else {
      smooth.data <- stats::lowess(tmp$date, tmp[, statistic], f = f)
      lines(smooth.data[, vars], col = cbPalette[i])
    }
  } else {
    if (nrow(dat) > 7) {
      smooth.data <- stats::loess(tmp[, statistic] ~
        as.numeric(tmp$date), span = span)
      x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
      lines(x.date, smooth.data$fitted, col = cbPalette[i])
    } else {
      lines(stats::lowess(tmp[dat$package == x$packages[i], vars], f = f),
        col = cbPalette[i])
    }
  }
}

addRPlotSmoother <- function(x, complete, f, span, pltfrm, pltfrm.col,
  statistic, wk1.backdate) {

  dat <- x$cranlogs.data

  if (any(dat$in.progress)) {
    invisible(lapply(seq_along(complete), function(i) {
      tmp <- complete[[i]]
      if (nrow(dat) > 7) {
        smooth.data <- stats::loess(tmp[, statistic] ~
          as.numeric(tmp$date), span = span)
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
        smooth.data <- stats::loess(tmp[, statistic] ~
          as.numeric(tmp$date), span = span)
        x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
        lines(x.date, smooth.data$fitted, lwd = 1.5, col =  pltfrm.col[i])
      } else {
        smooth.data <- stats::lowess(tmp$date, tmp[, statistic], f = f)
        lines(smooth.data$x, smooth.data$fitted, lwd = 1.5,
          col = pltfrm.col[i])
      }
    }))
  } else {
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
    } else {
      lines(stats::lowess(tmp$date, tmp[, statistic], f = f),
        col = "blue", lwd = 1.25)
    }
  } else if (any(dat$partial)) {
    tmp <- rbind(wk1.backdate, complete)
    if (nrow(tmp) > 7) {
      smooth.data <- stats::loess(tmp[, statistic] ~
        as.numeric(tmp$date), span = span)
      x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
      lines(x.date, smooth.data$fitted, col = "blue", lwd = 1.25)
    } else {
      lines(stats::lowess(tmp$date, tmp[, statistic], f = f),
        col = "blue", lwd = 1.25)
    }
  } else {
    if (nrow(dat) > 7) {
      smooth.data <- stats::loess(dat[, statistic] ~
        as.numeric(dat$date), span = span)
      x.date <- as.Date(smooth.data$x, origin = "1970-01-01")
      lines(x.date, smooth.data$fitted, col = "blue", lwd = 1.25)
    } else {
      lines(stats::lowess(dat$date, dat[, statistic], f = f),
        col = "blue", lwd = 1.25)
    }
  }
}
