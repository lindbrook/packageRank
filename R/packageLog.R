#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param package Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @param all.filters Logical. Master switch for filters.
#' @param ip.filter Logical.
#' @param sequence.filter Logical.
#' @param size.filter Logical.
#' @param small.filter Logical. TRUE filters out downloads less than 1000 bytes.
#' @param version.filter Logical. TRUE selects only most recent version.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @return An R data frame.
#' @export

packageLog <- function(package = "cholera", date = NULL, all.filters = FALSE,
  ip.filter = FALSE, sequence.filter = FALSE, size.filter = FALSE, 
  small.filter = FALSE, version.filter = FALSE, memoization = TRUE, 
  check.package = TRUE) {

  if ("R" %in% package) {
    if (length(package) == 1) {
      if (package == "R") {
        stop('packageLog("R") currently does not work.', call. = FALSE)
      }
    } else {
      package <- package[package != "R"]
      message('Note: packageLog("R") currently does not work.')
    }
  }
  
  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  if (check.package) package <- checkPackage(package)
  
  log.date <- logDate(date)
  cran_log <- fetchCranLog(date = log.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  ymd <- rev_fixDate_2012(log.date)

  unobs.pkgs <- !package %in% cran_log$package
  if (any(unobs.pkgs)) pkg.msg <- paste(package[unobs.pkgs], collapse = ", ")

  if (all(unobs.pkgs)) {
    stop("No downloads for ", pkg.msg, " on ", ymd, ".", call. = FALSE)
  } else if (any(unobs.pkgs)) {
    message("No downloads for ", pkg.msg, " on ", ymd, ".")
    package <- package[!unobs.pkgs]
  }

  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
    size.filter <- TRUE
    version.filter <- TRUE
  }
  
  if (ip.filter) cran_log <- ipFilter(cran_log, multi.core = FALSE)
  
  pkg.data <- lapply(package, function(p) cran_log[cran_log$package == p, ])
  
  out <- lapply(seq_along(pkg.data), function(i) { 
    p.dat <- pkg.data[[i]]
    p <- package[i]
    
    if (nrow(p.dat) != 0) {
      if (small.filter) p.dat <- smallFilter(p.dat)
      
      p.dat$date.time <- dateTime(p.dat$date, p.dat$time)
      if (sequence.filter) p.dat <- sequenceFilter(p.dat, p, ymd)
      
      if (size.filter) p.dat <- sizeFilter(p.dat, p)
      if (version.filter) p.dat <- versionFilter(p.dat, p, ymd)
      p.dat <- p.dat[order(p.dat$date.time), ]
      p.dat$date.time <- NULL
    }
    p.dat
  })
  
  names(out) <- package
  
  pkgs.survived <- names(vapply(out, nrow, integer(1L)) > 0)
  pkg.not_survived <- setdiff(package, pkgs.survived)
  
  if (length(pkg.not_survived) > 0) {
    message("No filtered downloads for ", paste(pkg.not_survived, 
      collapse = ", "), ".")
  }
  
  class(out) <- c(class(out), "packageLog")
  out
}

#' Plot method for packageLog().
#'
#' @param x Object.
#' @param type Character. "1D" or "2D".
#' @param time.unit Character. "second", "minute", or "hour".
#' @param smooth Logical. Add smoother.
#' @param points Logical. For "hour" and "minute" in 2D plots.
#' @param same.xy Logical. Use same scale for multiple packages for type = "2D".
#' @param local.timezone Logical or Character. TRUE for Sys.timezone(). See OlsonNames() for other time zones.
#' @param ... Additional parameters.
#' @export

plot.packageLog <- function(x, type = "1D", time.unit = "second",
  smooth = FALSE, points = TRUE, same.xy = TRUE, local.timezone = TRUE, ...) {

  x <- x[vapply(x, nrow, integer(1L)) != 0]
  log.date <- unique(x[[1]]$date)
  x.time <- c("00:00:00 UTC", "06:00:00 UTC", "12:00:00 UTC", "18:00:00 UTC")
  x.tick <- c(dateTime(log.date, x.time), dateTime(log.date + 1, x.time[1]))

  if (isTRUE(local.timezone)) {
    x2.tick <- as.POSIXct(x.tick, tz = Sys.timezone())
  } else if (is.character(local.timezone)) {
    if (local.timezone %in% OlsonNames()) {
      x2.tick <- as.POSIXct(x.tick, tz = local.timezone)
    } else {
      message('Invalid time zone. See OlsonNames().')
      x2.tick <- NA
    }
  } else if (isFALSE(local.timezone)) {
    x2.tick <- NA
  }

  obs.time <- lapply(x, function(pkg) dateTime(pkg$date, pkg$time))
  
  plot.data <- lapply(obs.time, function(t) {
    if (time.unit == "second") {
      data.frame(time = unique(t),
                 count = c(table(t)), 
                 row.names = NULL)
    } else if (time.unit == "minute") {
      obs.minute <- format(t, format = "%H:%M")
      tab.minute <- table(obs.minute)
      data.frame(time = dateTime(log.date, names(tab.minute)),
                 count = c(tab.minute),
                 row.names = NULL)
    } else if (time.unit == "hour") {
      obs.hour <- format(t, format = "%H")
      tab.hour <- table(obs.hour)
      data.frame(time = dateTime(log.date, names(tab.hour)),
                 count = c(tab.hour),
                 row.names = NULL)
    }
  })
  
  if (type == "2D") {
    if (same.xy) {
      cts <- vapply(plot.data, function(dat) max(dat$count), integer(1L))
      ylim <- c(0, max(cts))
    } else {
      ylim <- NULL
    }    
  }
  
  if (length(x) > 1) grDevices::devAskNewPage(ask = TRUE)
  
  invisible(lapply(names(plot.data), function(nm) {
    logPlot(plot.data[[nm]], type, time.unit, points,
      log.date, x.tick, ylim, nm, smooth, x2.tick)
  }))
  
  if (length(x) > 1) grDevices::devAskNewPage(ask = FALSE)
}

logPlot <- function(pkg, type, time.unit, points, log.date, x.tick, ylim, nm,
  smooth, x2.tick) {
  
  xlab <- "UTC 24-Hour Clock"
 
  if (type == "1D") {    
    plot(pkg$time, rep(1, nrow(pkg)), pch = 0, xaxt = "n", yaxt = "n",
      xlab = xlab, ylab = NA, xlim = range(x.tick)) 
  } else if (type == "2D") {
    plot(pkg$time, pkg$count, xaxt = "n", xlab = xlab, ylab = "Count", 
      xlim = range(x.tick), ylim = ylim, type = "l")
    if (points) points(pkg$time, pkg$count, pch = 0, cex = 0.75)
    if (smooth) {
      smooth.data <- stats::loess(pkg$count ~ as.numeric(pkg$time))
      lines(pkg$time, smooth.data$fitted, col = "blue", lwd = 1.25)  
    }
  }
  
  axis(1, at = x.tick, labels = format(x.tick, "%H"))
  if (all(!is.na(x2.tick))) {
    axis(3, at = x2.tick, labels = format(x2.tick, "%H"), cex.axis = 2/3,
      col.axis = "red", col.ticks = "red", padj = 0.9)
    other.timezone <- unique(format(x2.tick, format = "%Z"))
    obs.days <- unique(format(x2.tick[-length(x2.tick)], format = "%m/%d"))
    if (length(obs.days != 1)) obs.days <- paste(obs.days, collapse = " - ")
    legend(x = "top", legend = paste(obs.days, other.timezone), col = "red",
      pch = NULL, bty = "n", bg = "white", cex = 3/4, lwd = 1, title = NULL)
  }
  day <- weekdays(log.date, abbreviate = TRUE)
  title(main = paste0(nm, " @ ", log.date, " (", day, ")"))
  title(sub = paste0("time.unit: ", time.unit), cex.sub = 0.9)
}

#' Print method for packageLog().
#' @param x object.
#' @param ... Additional parameters.
#' @export

print.packageLog <- function(x, ...) {
  attr(x, "class") <- NULL
  if (length(x) == 1) {
    print(x[[1]])
  } else {
    print.default(x, ...)
  }
}