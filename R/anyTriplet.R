#' Detect small downloads triplets (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param pkg Character. Package name.
#' @param cran_log Object. Package log.
#' @export

anyTriplet <- function(pkg, cran_log) {
  dat <- cran_log[cran_log$package == pkg, ]
  dat$id <- paste0(dat$package, "-",
                   dat$time, "-",
                   dat$ip_id, "-",
                   dat$r_version, "-",
                   dat$r_os, "-",
                   dat$version)

  crosstab <- table(dat$id)

  triplets <- vapply(names(crosstab[crosstab == 3]), function(x) {
    size <- ceiling(log10(dat[dat$id == x, "size"]))
    length(unique(size)) == 3
  }, logical(1L))

  any(triplets)
}
