#' Filter downloads of full-sized sequential versions (prototype).
#'
#' @param dat Object.
#' @param packages Object. An R vector of package names.
#' @param ymd Date. Log date.
#' @param cores Numeric. Number of cores to use.
#' @param download.time Numeric. Package download time allowance (seconds).
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export

sequenceFilter <- function(dat, packages, ymd, cores, download.time = 30,
  dev.mode = dev.mode) {

  # win.exception <- .Platform$OS.type == "windows" & cores > 1

  # if (dev.mode | win.exception) {
  if (dev.mode) {
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = c("packages", "ymd"))
    arch.pkg.history <- parallel::parLapply(cl, packages, function(x) {
      tmp <- packageHistory(x)
      tmp[tmp$Date <= ymd & tmp$Repository == "Archive", ]
    })
    parallel::stopCluster(cl)

  } else {
    if (.Platform$OS.type == "windows") cores <- 1L
    arch.pkg.history <- parallel::mclapply(packages, function(x) {
      tmp <- packageHistory(x)
      tmp[tmp$Date <= ymd & tmp$Repository == "Archive", ]
    }, mc.cores = cores)
  }

  sequences <- identifySequences(dat, arch.pkg.history,
    download.time = download.time)

  if (!is.null(sequences)) {
    delete <- row.names(sequences)
    if (!is.null(delete)) {
      dat[!row.names(dat) %in% delete, ]
    } else dat
  } else dat
}

#' Extract Archive sequences from logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/

#' @param dat Object.
#' @param arch.pkg.history Object.
#' @param download.time Numeric. Package download time allowance (seconds).
#' @noRd

identifySequences <- function(dat, arch.pkg.history, download.time = 30) {
  lapply(seq_along(dat), function(i) {
    pkg.data <- dat[[i]]
    pkg.history <- arch.pkg.history[[i]]

    pkg.data$t0 <- strptime(paste(pkg.data$date, pkg.data$time), "%Y-%m-%d %T",
      tz = "GMT")
    pkg.data <- pkg.data[order(pkg.data$t0), ]

    rle.data <- rle(pkg.data$ver)
    rle.out <- data.frame(lengths = rle.data$lengths, values = rle.data$values)

    archive.obs <- pkg.history$Version %in%
      rle.out[rle.data$lengths == 1, "values"]

    if (all(archive.obs)) {
      rle.out$idx <- seq_len(nrow(rle.out))
      breaks <- rle.out[rle.out$lengths != 1, "idx"]

      candidate.seqs <- lapply(seq_along(breaks), function(i) {
        if (i < length(breaks)) {
          (breaks[i] + 1):(breaks[i + 1] - 1)
        } else if (breaks[i] < max(rle.out$idx)) {
          (breaks[i] + 1):max(rle.out$idx)
        } else NA
      })

      candidate.seqs <- candidate.seqs[!is.na(candidate.seqs)]

      candidate.check <- unlist(lapply(candidate.seqs, function(sel) {
        dat <- rle.out[sel, ]
        elements.check <- identical(sort(dat$values), pkg.history$Version)
        if (elements.check) {
          t.range <- range(pkg.data[cumsum(rle.out$lengths)[sel], "t0"])
          time.window <- download.time * nrow(dat)
          difftime(t.range[2], t.range[1], units = "sec") < time.window
        } else FALSE
      }))

      obs.sel <- unlist(candidate.seqs[candidate.check])
      pkg.data[cumsum(rle.out$lengths)[obs.sel], names(pkg.data) != "t0"]
    }
  })
}
