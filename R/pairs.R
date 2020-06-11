#' Detect small downloads pairs (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param x Object. Package log.
#' @param sample.pct Numeric. Percent of packages to sample.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores to use. Note that due to performance considerations, the number of cores defaults to one on Windows.
#' @export

pairID <- function(x, sample.pct = 5, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  pkg.names <- unique(x$package)
  if (sample.pct > 0 & sample.pct < 100) {
    sample.size <- ceiling(length(pkg.names) * sample.pct / 100)
    pkgs <- sample(unique(x$package), sample.size)
  } else if (sample.pct == 100) {
    pkgs <- unique(x$package)
  } else if (sample.pct <= 0 | sample.pct > 100) {
    stop("0 < sample.pct <= 100.")
  }

  pair.id <- parallel::mclapply(pkgs, function(p) {
    dat <- x[x$package == p, ]
    dat$id <- paste0(dat$package, "-", dat$time, "-", dat$ip_id, "-",
      dat$r_version, "-", dat$r_arch, "-", dat$r_os, "-", dat$version)
    crosstab <- table(dat$id)
    pairs <- vapply(names(crosstab[crosstab == 2]), function(nm) {
      time.stamp <- dat[dat$id == nm, ]
      test_500 <- any(time.stamp$size < 1000)
      size <- ceiling(log10(dat[dat$id == nm, "size"]))
      test_size <- length(unique(size)) == 2
      test_500 & test_size
    }, logical(1L))
    names(crosstab[crosstab == 2][pairs])
  }, mc.cores = cores)

  unlist(pair.id)
}

#' Extract small downloads pairs (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param id Character. pairLogID() output.
#' @param x Object. Package log.
#' @export

pairEntry <- function(id, x) {
  x$id <- paste0(x$package, "-", x$time, "-", x$ip_id, "-", x$r_version, "-",
    x$r_arch, "-", x$r_os, "-", x$version)
  out <- x[x$id == id, ]
  out$id <- NULL
  out
  
}
