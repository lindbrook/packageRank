#' Tabulate a country's package downloads.
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param country Character. country abbreviation.
#' @param date Character. Date. yyyy-mm-dd.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param sort Logical. Sort by download count.
#' @param small.filter Logical.
#' @param triplet.filter Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @param output Character. "top.ten", "bottom.ten", or "all".
#' @note "US" outlier 6-7 min with size filters.
#' @export

countryPackage <- function(country = "US", date = Sys.Date() - 1,
  memoization = TRUE, sort = TRUE, small.filter = FALSE, triplet.filter = FALSE,
  multi.core = TRUE, output = "top.ten") {

  cores <- multiCore(multi.core)
  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)

  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  sel <- !is.na(cran_log$package) &
         !is.na(cran_log$country) &
         !is.na(cran_log$size) &
         cran_log$country == country

  cran_log <- cran_log[sel, ]

  if (triplet.filter) {
    tri.filtered <- parallel::mclapply(unique(cran_log$package), function(p) {
      x <- cran_log[cran_log$package == p, ]
      do.call(rbind, tripletFilter(x, small.filter = FALSE))
    }, mc.cores = cores)
    
    cran_log <- do.call(rbind, tri.filtered)
  }

  if (small.filter) {
    cran_log <- smallFilter(cran_log, filter = small.filter)
  }

  crosstab <- table(cran_log$package)

  if (sort) {
    out <- sort(crosstab, decreasing = TRUE)
  } else {
    out <- crosstab
  }

  if (output == "top.ten") {
    utils::head(out, 10)
  } else if (output == "bottom.ten") {
    utils::tail(out, 10)
  } else if (output == "all") {
    out
  } else {
    stop('output must be "top.ten", "bottom.ten", or "all".')
  }
}
