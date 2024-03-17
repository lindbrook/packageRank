#' Filter downloads of full-sized sequential versions (prototype).
#'
#' @param dat Object.
#' @param packages Object. An R vector of package names.
#' @param ymd Date. Log date.
#' @param cores Numeric. Number of cores to use.
#' @param delta.time Numeric. Time between package downloads (seconds).
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @noRd

sequenceFilter <- function(dat, packages, ymd, delta.time = 10,
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
  
  removeSequences(dat, arch.pkg.history, delta.time = delta.time)
}

#' Extract Archive sequences from logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object.
#' @param arch.pkg.history Object.
#' @param delta.time Numeric. Time between package downloads (seconds).
#' @noRd

removeSequences <- function(dat, arch.pkg.history, delta.time = 10) {
  lapply(seq_along(dat), function(i) {
    pkg.data <- dat[[i]]
    pkg.history <- arch.pkg.history[[i]]
    
    if (nrow(pkg.history) != 0) {
      pttrn <- paste0(pkg.history$Version, collapse = " ")
      obs.versions <- paste0(pkg.data$version, collapse = " ")
      version.seq <- grepl(pttrn, obs.versions)

      if (version.seq) {
        version.id <- seq_along(pkg.history$Version)
        pkg.data$t2 <- dateTime(pkg.data$date, pkg.data$time)
        pkg.data <- pkg.data[order(pkg.data$t2), ]
        
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
            # sequence may not be in version order (esp. full downloads)
            all(sort(match(tmp$ver, pkg.history$Version)) == version.id)
          }, logical(1L))
          
          if (all(seq.check)) {
            rle.exclude <- unlist(lapply(seq_along(seq.start), function(i) {
              seq.tmp.obs <- row.names(candidates[seq.start[i]:seq.stop[i], ])
              start.stop <- rle.data[seq.tmp.obs, ]
              obs.chk <- unique(unlist(start.stop[, c("start", "stop")]))
              tmp <- pkg.data[obs.chk, ]
              tmp$t2 <- dateTime(tmp$date, tmp$time)
              time.range <- range(tmp$t2)
              time.window <- delta.time * nrow(tmp)
              time.range.delta <- difftime(time.range[2], time.range[1],
                units = "sec")
              if (time.range.delta < time.window) obs.chk
            }))
          
            obs.exclude <- row.names(pkg.data[rle.exclude, ])
          }
        } 
      } else {
        first.pkg.version <- pkg.history[1, ]$Version
        
        if (first.pkg.version %in% pkg.data$version) {
          sel <- pkg.data$version == first.pkg.version
          first.pkg.ip <- unique(pkg.data[sel, ]$ip_id)
          
          candidate <- pkg.data[pkg.data$ip_id %in% first.pkg.ip, ]
          candidate <- candidate[candidate$version %in% pkg.history$Version, ]
          all.archive.vers <- all(pkg.history$Version %in% candidate$version)
          
          candidate$t2 <- dateTime(candidate$date, candidate$time)
          candidate <- candidate[order(candidate$t2), ]
          
          seq.start <- candidate[candidate$version == first.pkg.version, ]
          
          time.window <- delta.time * nrow(pkg.history)
          
          candidate.endpts <- lapply(seq_len(nrow(seq.start)), function(i) {
            data.frame(alpha = seq.start[i, ]$t2 - time.window,
                       omega = seq.start[i, ]$t2 + time.window)
          })
          
          obs.exclude <- unlist(lapply(candidate.endpts, function(x) {
            sel <- candidate$t2 >= x$alpha & candidate$t2 <= x$omega
            tmp <- candidate[sel, ]
            if (all(pkg.history$Version %in% tmp$version)) row.names(tmp)
          }))
        }
      }

      if (exists("obs.exclude")) {
        out <- pkg.data[!row.names(pkg.data) %in% obs.exclude, ]
      } else out <- pkg.data
    } else out <- pkg.data
    out
  })
}
