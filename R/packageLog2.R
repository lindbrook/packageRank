#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param triplet.filter Logical.
#' @param ip.filter Logical.
#' @param small.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @param clean.output Logical. NULL row names.
#' @return An R data frame.
#' @export

packageLog2 <- function(packages = NULL, date = Sys.Date() - 1,
  triplet.filter = TRUE, ip.filter = TRUE, small.filter = TRUE,
  memoization = TRUE, check.package = TRUE, dev.mode = FALSE,
  clean.output = FALSE) {

  if (!is.null(packages)) {
    if (!"R" %in% packages) {
      if (check.package) {
        packages <- checkPackage(packages, dev.mode)
      }
    }
  }

  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)

  if (!is.null(packages)) {
    cran_log <- cran_log[!is.na(cran_log$package) & !is.na(cran_log$size), ]
    out <- lapply(packages, function(p) cran_log[cran_log$package == p, ])

    if (triplet.filter) {
      out <- lapply(out, function(x) {
        filtered.data <- tripletFilter(x)
        do.call(rbind, filtered.data)
      })
    }
  }

  if (ip.filter) {
    ip.delete <- ipFilter3(cran_log)
    if (!is.null(packages)) {
      out <- lapply(out, function(x) {
        x[!x$ip_id %in% ip.delete, ]
      })
    } else {
      cran_log <- cran_log[!cran_log$ip_id %in% ip.delete, ]
    }
  }

  if (small.filter) {
    if (!is.null(packages)) {
      out <- lapply(out, smallFilter0)
    } else {
      cran_log <- smallFilter(cran_log)
    }
  }

  if (length(packages) == 1) {
    out <- out[[1]]
    out$date.time <- as.POSIXlt(paste(out$date, out$time), tz = "Europe/Vienna")
    out <- out[order(out$date.time), ]
    out$date.time <- NULL
    if (clean.output) rownames(out) <- NULL
  } else if (length(packages > 1)) {
    names(out) <- packages
    out <- lapply(out, function(x) {
      x$date.time <- as.POSIXlt(paste(x$date, x$time), tz = "Europe/Vienna")
      tmp <- x[order(x$date.time), ]
      tmp$date.time <- NULL
      tmp
    })
  } else if (is.null(packages)) {
    cran_log$date.time <- as.POSIXlt(paste(cran_log$date, cran_log$time),
      tz = "Europe/Vienna")
    cran_log <- cran_log[order(cran_log$date.time), ]
    cran_log$date.time <- NULL
  }

  if (is.null(packages)) cran_log else out
}
