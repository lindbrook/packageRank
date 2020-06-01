#' Detect small downloads triplets (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param x Object. Package log.
#' @param sample.pct Numeric. Percent of packages to sample.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores to use. Note that due to performance considerations, the number of cores defaults to one on Windows.
#' @export

anyTriplet <- function(x, sample.pct = 5, multi.core = TRUE) {
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

  tri.test <- parallel::mclapply(pkgs, function(p) {
    dat <- x[x$package == p, ]
    dat$id <- paste0(dat$time, "-",
                     dat$ip_id, "-",
                     dat$r_version, "-",
                     dat$r_arch, "-",
                     dat$r_os, "-",
                     dat$version)
    crosstab <- table(dat$id)
    if (any(crosstab == 3)) {
      triplets <- vapply(names(crosstab[crosstab == 3]), function(x) {
        time.stamp <- dat[dat$id == x, ]
        test_500 <-  any(time.stamp$size < 1000)
        size <- ceiling(log10(dat[dat$id == x, "size"]))
        test_size <- length(unique(size)) == 3
        test_500 & test_size
      }, logical(1L))
      any(triplets)
    } else {
      FALSE
    }
  }, mc.cores = cores)

  tri.test <- unlist(tri.test)
  list(yes = pkgs[tri.test], no = pkgs[!tri.test])
}
