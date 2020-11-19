#' Extract Archive sequences from logs (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param package Character. Package name.
#' @param date Character. Date.
#' @param download.time Numeric. Package download time allowance (seconds).
#' @param memoization Logical. Use memoization when downloading logs.
#' @param dev.mode Logical.
#' @export

identifySequences <- function(package = "cholera", date = Sys.Date() - 1,
  download.time = 30, memoization = TRUE, dev.mode = FALSE) {

  packages <- checkPackage(package)
  pkg.hist <- packageHistory(package)
  pkg.hist <- pkg.hist[pkg.hist$Date <= as.Date(date), ]
  arch.pkg.hist <- pkg.hist[pkg.hist$Repository == "Archive", ]

  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization,
    dev.mode = dev.mode)
  cran_log <- cleanLog(cran_log)

  pkg.dat <- cran_log[cran_log$package == package, ]
  pkg.dat$t0 <- strptime(paste(pkg.dat$date, pkg.dat$time), "%Y-%m-%d %T",
    tz = "Europe/Vienna")
  pkg.dat <- pkg.dat[order(pkg.dat$t0), ]
  
  pkg.dat <- tripletFilter(pkg.dat)
  pkg.dat <- smallFilter0(pkg.dat)

  rle.data <- rle(pkg.dat$ver)
  rle.out <- data.frame(lengths = rle.data$lengths, values = rle.data$values)

  archive.obs <- arch.pkg.hist$Version %in%
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

    candidate.check <- vapply(candidate.seqs, function(sel) {
      dat <- rle.out[sel, ]
      elements.check <- identical(sort(dat$values), arch.pkg.hist$Version)
      if (elements.check) {
        t.range <- range(pkg.dat[cumsum(rle.out$lengths)[sel], "t0"])
        time.window <- download.time * nrow(dat)
        difftime(t.range[2], t.range[1], units = "sec") < time.window
      }
    }, logical(1L))

    obs.sel <- unlist(candidate.seqs[candidate.check])
    pkg.dat[cumsum(rle.out$lengths)[obs.sel], names(pkg.dat) != "t0"]
  }
}
