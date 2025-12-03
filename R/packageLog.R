#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
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

packageLog <- function(packages = "cholera", date = NULL, all.filters = FALSE,
  ip.filter = FALSE, sequence.filter = FALSE, size.filter = FALSE, 
  small.filter = FALSE, version.filter = FALSE, memoization = TRUE, 
  check.package = TRUE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  if (check.package) packages <- checkPackage(packages)
  
  log.date <- logDate(date)
  cran_log <- fetchCranLog(date = log.date, memoization = memoization)
  cran_log <- cleanLog(cran_log)
  ymd <- rev_fixDate_2012(log.date)

  unobs.pkgs <- !packages %in% cran_log$package
  if (any(unobs.pkgs)) pkg.msg <- paste(packages[unobs.pkgs], collapse = ", ")

  if (all(unobs.pkgs)) {
    stop("No downloads for ", pkg.msg, " on ", ymd, ".", call. = FALSE)
  } else if (any(unobs.pkgs)) {
    message("No downloads for ", pkg.msg, " on ", ymd, ".")
    packages <- packages[!unobs.pkgs]
  }

  if (all.filters) {
    ip.filter <- TRUE
    small.filter <- TRUE
    size.filter <- TRUE
    version.filter <- TRUE
  }
  
  if (ip.filter) cran_log <- ipFilter(cran_log, multi.core = FALSE)
  
  pkg.data <- lapply(packages, function(p) cran_log[cran_log$package == p, ])
  
  out <- lapply(seq_along(pkg.data), function(i) { 
    p.dat <- pkg.data[[i]]
    p <- packages[i]
    
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
  
  names(out) <- packages
  
  pkgs.survived <- names(vapply(out, nrow, integer(1L)) > 0)
  pkg.not_survived <- setdiff(packages, pkgs.survived)
  
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
#' @param unit.observation Character. "second", "minute", or "hour".
#' @param ... Additional parameters.
#' @export

plot.packageLog <- function(x, type = "1D", unit.observation = "second", ...) {
  if (length(x) > 1) grDevices::devAskNewPage(ask = TRUE)
  invisible(lapply(x, function(pkg) logPlot(pkg, type, unit.observation)))
  if (length(x) > 1) grDevices::devAskNewPage(ask = FALSE)
}

logPlot <- function(pkg, type, unit.observation) {
  pkg.date <- unique(pkg$date)
  obs.time <- dateTime(pkg$date, pkg$time)
  x.time <- c("00:00:00 UTC", "06:00:00 UTC", "12:00:00 UTC", "18:00:00 UTC")
  x.tick <- c(dateTime(pkg.date, x.time), dateTime(pkg.date + 1, x.time[1]))
  
  if (unit.observation == "minute") {
    obs.minute <- format(obs.time, format = "%H:%M")
    obs.time <- dateTime(pkg$date, obs.minute)
  } else if (unit.observation == "hour") {
    obs.hour <- format(obs.time, format = "%H")
    obs.time <- dateTime(pkg$date, obs.hour)
  }
  
  unique.time <- unique(obs.time)
  
  if (type == "1D") {
    plot(unique.time, rep(1, length(unique.time)), pch = 0, xaxt = "n",
      yaxt = "n", xlab = "Hour", ylab = NA, xlim = range(x.tick))
  } else if (type == "2D") {
    ct <- c(table(obs.time))
    if (unit.observation == "hour") {
      plot(unique.time, ct, pch = 0, cex = 0.75, xaxt = "n", xlab = "24-Hour", 
        ylab = "Count", xlim = range(x.tick), type = "o")  
    } else if (unit.observation == "minute") {
      plot(unique.time, ct, pch = 0, cex = 0.75, xaxt = "n", xlab = "24-Hour", 
        ylab = "Count", xlim = range(x.tick), type = "l")
    } else if (unit.observation == "second") {
      plot(unique.time, ct, pch = NA, type = "h", xaxt = "n", xlab = "24-Hour", 
        ylab = "Count", xlim = range(x.tick))
    }
  }
  
  axis(1, at = x.tick, labels = format(x.tick, "%H"))
  title(main = paste(unique(pkg$package), "@", pkg.date))
  title(sub = paste0("unit of observation: ", unit.observation), cex.sub = 0.9)
}

#' Print method for packageLog().
#' @param x object.
#' @param ... Additional parameters.
#' @export

print.packageLog <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x, ...)
}