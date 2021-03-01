#' Filter downloads of full-sized sequential versions (prototype).
#'
#' @param pkg.data Object.
#' @param arch.pkg.history Object.
#' @param download.time Numeric. Package download time allowance (seconds).
#' @export

sequenceFilter <- function(pkg.data, arch.pkg.history, download.time = 30) {
  sequences <- identifySequences(pkg.data, arch.pkg.history,
    download.time = download.time)
  if (!is.null(sequences)) {
    delete <- row.names(sequences)
    if (!is.null(delete)) {
      pkg.data[!row.names(pkg.data) %in% delete, ]
    } else pkg.data
  } else pkg.data
}

#' Extract Archive sequences from logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/

#' @param pkg.data Object.
#' @param arch.pkg.history Object.
#' @param download.time Numeric. Package download time allowance (seconds).
#' @noRd

identifySequences <- function(pkg.data, arch.pkg.history, download.time = 30) {
  pkg.data$t0 <- strptime(paste(pkg.data$date, pkg.data$time), "%Y-%m-%d %T",
    tz = "GMT")
  pkg.data <- pkg.data[order(pkg.data$t0), ]

  rle.data <- rle(pkg.data$ver)
  rle.out <- data.frame(lengths = rle.data$lengths, values = rle.data$values)

  archive.obs <- arch.pkg.history$Version %in%
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
      elements.check <- identical(sort(dat$values), arch.pkg.history$Version)
      if (elements.check) {
        t.range <- range(pkg.data[cumsum(rle.out$lengths)[sel], "t0"])
        time.window <- download.time * nrow(dat)
        difftime(t.range[2], t.range[1], units = "sec") < time.window
      } else FALSE
    }))

    obs.sel <- unlist(candidate.seqs[candidate.check])
    pkg.data[cumsum(rle.out$lengths)[obs.sel], names(pkg.data) != "t0"]
  }
}
