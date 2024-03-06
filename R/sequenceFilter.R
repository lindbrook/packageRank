#' Filter downloads of full-sized sequential versions (prototype).
#'
#' @param dat Object.
#' @param packages Object. An R vector of package names.
#' @param ymd Date. Log date.
#' @param cores Numeric. Number of cores to use.
#' @param download.time Numeric. Package download time allowance (seconds).
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @noRd

sequenceFilter <- function(dat, packages, ymd, cores, download.time = 30,
  dev.mode = dev.mode) {
  
  histories <- packageHistory(packages, check.package = FALSE)
  
  if (is.data.frame(histories)) {
    sel <- histories$Date <= ymd & histories$Repository == "Archive"
    arch.pkg.history <- list(histories[sel, ])
  } else if (is.list(histories)) {
    arch.pkg.history <- lapply(histories, function(x) {
      x[x$Date <= ymd & x$Repository == "Archive", ]
    })
  }
  
  removeSequences(dat, arch.pkg.history, download.time = download.time)
}

#' Extract Archive sequences from logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object.
#' @param arch.pkg.history Object.
#' @param download.time Numeric. Package download time allowance (seconds).
#' @noRd

removeSequences <- function(dat, arch.pkg.history, download.time = 30) {
  lapply(seq_along(dat), function(i) {
    pkg.data <- dat[[i]]
    pkg.history <- arch.pkg.history[[i]]

    if (nrow(pkg.history) != 0) {
      pkg.data$t0 <- strptime(paste(pkg.data$date, pkg.data$time),
        "%Y-%m-%d %T", tz = "GMT")
      
      pkg.data <- pkg.data[order(pkg.data$t0), ]
      archive.seq <- pkg.data$version %in% pkg.history$Version
      
      if (any(archive.seq)) {
        candidate <- pkg.data[archive.seq, ]
        time.range <- range(candidate$t0)
        time.window <- download.time * nrow(candidate)
        time.range.delta <- difftime(time.range[2], time.range[1],
          units = "sec")
        if (time.range.delta < time.window) pkg.data[!archive.seq, ]
      } else pkg.data
    } else pkg.data
  })
}
