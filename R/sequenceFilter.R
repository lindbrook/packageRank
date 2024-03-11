#' Filter downloads of full-sized sequential versions (prototype).
#'
#' @param dat Object.
#' @param packages Object. An R vector of package names.
#' @param ymd Date. Log date.
#' @param cores Numeric. Number of cores to use.
#' @param download.time Numeric. Package download time allowance (seconds).
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @noRd

sequenceFilter <- function(dat, packages, ymd, cores, download.time = 5,
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

removeSequences <- function(dat, arch.pkg.history, download.time = 5) {
  lapply(seq_along(dat), function(i) {
    pkg.data <- dat[[i]]
    pkg.history <- arch.pkg.history[[i]]
    
    if (nrow(pkg.history) != 0) {
      version.id <- seq_along(pkg.history$Version)
      pkg.data$t0 <- dateTime(pkg.data$date, pkg.data$time)
      pkg.data <- pkg.data[order(pkg.data$t0), ]
      
      runs <- rle(pkg.data$version)
      rle.data <- data.frame(ver = runs$values, length = runs$lengths)
      
      obs.stop <- cumsum(rle.data$length)
      obs.start <- c(0, obs.stop[-length(obs.stop)]) + 1
      
      rle.data$start <- obs.start
      rle.data$stop <- obs.stop
      
      # single instance sequences
      candidates <- rle.data[rle.data$length == 1, ]
  
      seq.start <- which(candidates$ver == pkg.history$Version[1])
      seq.stop <- which(candidates$ver == pkg.history$Version[nrow(pkg.history)])
      
      if (length(seq.start) != 0 & length(seq.stop) != 0) {
        seq.check <- vapply(seq_along(seq.start), function(i) {
          tmp <- candidates[seq.start[i]:seq.stop[i], ]
          # download sequence may not be in version order (esp. full downloads)
          all(sort(match(tmp$ver, pkg.history$Version)) == version.id)
        }, logical(1L))
        
        if (all(seq.check)) {
          obs.exclude <- unlist(lapply(seq_along(seq.start), function(i) {
            seq.tmp.obs <- row.names(candidates[seq.start[i]:seq.stop[i], ])
            start.stop <- rle.data[seq.tmp.obs, ]
            obs.chk <- unique(unlist(start.stop[, c("start", "stop")]))
            
            tmp <- pkg.data[obs.chk, ]
            tmp$t0 <- dateTime(tmp$date, tmp$time)
            
            time.range <- range(tmp$t0)
            time.window <- download.time * nrow(tmp)
            time.range.delta <- difftime(time.range[2], time.range[1],
                                         units = "sec")
            dwnld.time.window <- time.range.delta < time.window
            
            if (dwnld.time.window) obs.chk
          }))
          out <- pkg.data[-obs.exclude, ]
        } else out <- pkg.data
      } else out <- pkg.data
    } else out <- pkg.data
    out$t0 <- NULL
    out
  })
}
