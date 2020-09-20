#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param triplet.filter Logical.
#' @param ip.filter Logical.
#' @param small.filter Logical.
#' @param sequence.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @param clean.output Logical. NULL row names.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

packageLog <- function(packages = "cholera", date = Sys.Date() - 1,
  triplet.filter = TRUE, ip.filter = TRUE, small.filter = TRUE,
  sequence.filter = TRUE, memoization = TRUE, check.package = TRUE,
  dev.mode = FALSE, clean.output = FALSE, multi.core = TRUE) {

  cores <- multiCore(multi.core)
  orig.pkg.order <- packages

  if (check.package) packages <- checkPackage(packages, dev.mode)
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  cran_log <- cleanLog(cran_log)

  out <- lapply(packages, function(p) cran_log[cran_log$package == p, ])
  zero.downloads <- vapply(out, nrow, integer(1L))

  if (any(zero.downloads == 0)) {
    zero.sel <- zero.downloads == 0
    zero.packages <- packages[zero.sel]
    zero.out <- out[zero.sel]
    packages <- packages[!zero.sel]
    out <- out[!zero.sel]
  }

  if (triplet.filter) {
    if (length(packages) == 1) {
      out <- lapply(out, function(x) {
        filtered.data <- tripletFilter(x)
        do.call(rbind, filtered.data)
      })
    } else if (length(packages) > 1) {
      out <- parallel::mclapply(out, function(x) {
        filtered.data <- tripletFilter(x)
        do.call(rbind, filtered.data)
      }, mc.cores = cores)
    }
  }

  if (ip.filter) {
    ip.outliers <- ipFilter3(cran_log)
    row.delete <- unlist(parallel::mclapply(ip.outliers, function(ip) {
      campaigns(ip, cran_log)
    }, mc.cores = cores))
    out <- lapply(out, function(x) {
      x[!row.names(x) %in% row.delete, ]
    })
  }

  # if (small.filter) {
  #   size.audit <- vapply(out, function(x) {
  #     length(unique(round(log10(x$size))))
  #   }, integer(1L))
  #   if (any(size.audit > 1)) {
  #     filtered <- lapply(out[size.audit > 1], smallFilter)
  #     out[which(size.audit > 1)] <- filtered
  #   }
  # }

  if (small.filter) {
    out <- parallel::mclapply(out, smallFilter0, mc.cores = cores)
  }

  if (sequence.filter) out <- lapply(out, sequenceFilter)

  if (any(zero.downloads == 0)) {
    packages <- c(packages, zero.packages)
    out <- c(out, zero.out)
  }

  if (length(packages) == 1) {
    out <- out[[1]]
    if (nrow(out) != 0) {
      if (!"t2" %in% names(out)) {
        out$t2 <- as.POSIXlt(paste(out$date, out$time), tz = "Europe/Vienna")
      }
      out <- out[order(out$t2), ]
      out$t2 <- NULL
      if (clean.output) rownames(out) <- NULL
    }
  } else if (length(packages) > 1) {
    names(out) <- packages
    out <- parallel::mclapply(out, function(x) {
      if (!"t2" %in% names(x)) {
        x$date.time <- as.POSIXlt(paste(x$date, x$time), tz = "Europe/Vienna")
      }
      tmp <- x[order(x$date.time), ]
      tmp$date.time <- NULL
      tmp
    }, mc.cores = cores)
    
    out <-  out[orig.pkg.order]
  }

  out 
}

#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date.
#' @param memoization Logical. Use memoization when downloading logs.
#' @return An R data frame.
#' @export

packageLog0 <- function(date = Sys.Date() - 1, memoization = TRUE) {
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  fetchCranLog(date = ymd, memoization = memoization)
}
