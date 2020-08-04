#' Get Package Download Logs.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param small.filter Logical.
#' @param triplet.filter Logical.
#' @param ip.filter Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @param clean.output Logical. NULL row names.
#' @return An R data frame.
#' @export

packageLog <- function(packages = NULL, date = Sys.Date() - 1,
  small.filter = TRUE, triplet.filter = TRUE, ip.filter = TRUE,
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

  # cran_log <- cran_log[!is.na(cran_log$package), ]
  cran_log <- cran_log[!is.na(cran_log$package) & !is.na(cran_log$size), ]

  if (ip.filter) {
    sel <- !cran_log$ip_id %in% ipFilter3(cran_log)
    cran_log <- cran_log[sel, ]
  }

  if (!is.null(packages)) {
    cran_log <- lapply(packages, function(p) cran_log[cran_log$package == p, ])

    if (!is.null(nrow(cran_log))) {
      if (triplet.filter) {
        cran_log <- lapply(cran_log, function(x) {
          filtered.data <- tripletFilter(x)
          do.call(rbind, filtered.data)
        })
      }

      if (small.filter) cran_log <- lapply(cran_log, smallFilter0)
      cran_log <- lapply(cran_log, function(x) x[order(x$time), ])
    }
  }

  if (length(packages) == 1) {
    out <- cran_log[[1]]
    if (clean.output) rownames(out) <- NULL
  } else {
    out <- cran_log
    names(out) <- packages
  }

  out
}
