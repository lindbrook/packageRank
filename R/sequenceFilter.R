#' Filter downloads of full-sized sequential versions (prototype).
#'
#' @param dat Object.
#' @param packages Object. An R vector of package names.
#' @param ymd Date. Log date.
#' @param cores Numeric. Number of cores to use.
#' @param delta.time Numeric. Time between package downloads (seconds).
#' @noRd

sequenceFilter <- function(dat, p, ymd, delta.time = 240) {
  obs.versions <- unique(dat$version)
  if (length(obs.versions) > 1) {
    history <- packageHistory(p, check.package = FALSE)
    sel <- history$Date <= ymd & history$Repository == "Archive"
    arch.pkg.history <- history[sel, ]
    removeSequences(dat, arch.pkg.history, delta.time = delta.time)
  } else if (length(obs.versions) == 1) dat
}

#' Extract Archive sequences from logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object.
#' @param arch.pkg.history Object.
#' @param delta.time Numeric. Time between first and last package download (seconds).
#' @noRd

removeSequences <- function(dat, arch.pkg.history, delta.time) {
  pkg.history <- arch.pkg.history
  history.obs <- nrow(pkg.history)
  
  if (history.obs != 0) {
    pttrn <- paste0(pkg.history$Version, collapse = " ")
    obs.versions <- paste0(dat$version, collapse = " ")
    version.seq <- grepl(pttrn, obs.versions)

    if (version.seq) {
      version.id <- seq_along(pkg.history$Version)
      dat <- dat[order(dat$date.time), ]
      
      runs <- rle(dat$version)
      rle.data <- data.frame(ver = runs$values, length = runs$lengths)
      
      obs.stop <- cumsum(rle.data$length)
      obs.start <- c(0, obs.stop[-length(obs.stop)]) + 1
      
      rle.data$start <- obs.start
      rle.data$stop <- obs.stop
      
      # single instance sequences
      candidates <- rle.data[rle.data$length == 1, ]
      candidates <- candidates[!duplicated(candidates$ver), ]

      seq.start <- which(candidates$ver == pkg.history$Version[1])
      seq.stop <- which(candidates$ver == pkg.history$Version[history.obs])
      
      # match number of seq.start and number of seq.stop
      seq.start <- seq.start[length(seq.stop)]
      
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
            tmp <- dat[obs.chk, ]
            time.range <- range(tmp$date.time)
            time.window <- delta.time * nrow(tmp)
            time.range.delta <- difftime(time.range[2], time.range[1],
              units = "sec")
            if (time.range.delta < time.window) obs.chk
          }))
          obs.exclude <- row.names(dat[rle.exclude, ])
        }
      } 
    } else {
      first.pkg.version <- pkg.history[1, ]$Version
      
      if (first.pkg.version %in% dat$version) {
        sel <- dat$version == first.pkg.version
        first.pkg.ip <- unique(dat[sel, ]$ip_id)
        
        candidate <- dat[dat$ip_id %in% first.pkg.ip, ]
        candidate <- candidate[candidate$version %in% pkg.history$Version, ]
        all.archive.vers <- all(pkg.history$Version %in% candidate$version)
        
        candidate <- candidate[order(candidate$date.time), ]
        
        seq.start <- candidate[candidate$version == first.pkg.version, ]
        
        time.window <- delta.time * history.obs
        
        candidate.endpts <- lapply(seq_len(nrow(seq.start)), function(i) {
          data.frame(alpha = seq.start[i, ]$date.time - time.window,
                     omega = seq.start[i, ]$date.time + time.window)
        })
        
        obs.exclude <- unlist(lapply(candidate.endpts, function(x) {
          sel <- candidate$date.time >= x$alpha & candidate$date.time <= x$omega
          tmp <- candidate[sel, ]
          if (all(pkg.history$Version %in% tmp$version)) row.names(tmp)
        }))
      }
    }

    if (exists("obs.exclude")) {
      out <- dat[!row.names(dat) %in% obs.exclude, ]
    } else out <- dat
  } else out <- dat
  out
}
