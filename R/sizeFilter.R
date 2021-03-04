#' Filter out size anomalies (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @param packages Character. Vector of package name(s).
#' @param cores Integer. Number of cores for parallelization.
#' @export

sizeFilter <- function(dat, packages, cores) {
  size.data <- lapply(packages, cranPackageSize)
  cores <- ifelse(length(packages) > 4, cores, 1L)
  parallel::mclapply(seq_along(dat), function(i) {
    sz <- size.data[[i]]
    tmp <- dat[[i]]
    if (length(unique(sz$version)) != 1) {
      sz2 <- sz[sz$date == max(sz$date), ]
      latest.ver <- unique(sz2$version)
      leftover <- tmp[tmp$version != latest.ver, ]
      tmp <- tmp[tmp$version == latest.ver, ]
      latest.sz <- min(sz2$bytes)
      tmp <- tmp[tmp$size >= latest.sz, ]
    } else {
      leftover <- tmp[tmp$version != unique(sz$version), ]
      tmp <- tmp[tmp$version == unique(sz$version), ]
      tmp <- tmp[tmp$size >= min(sz$bytes), ]
    }
    rbind(tmp, leftover)
  }, mc.cores = cores)
}
