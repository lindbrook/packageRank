#' Filter out size anomalies (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @param packages Character. Vector of package name(s).
#' @param cores Integer. Number of cores for parallelization.
#' @export

sizeFilter <- function(dat, packages, cores) {
  size.data <- lapply(packages, cranPackageSize)
  version.data <- lapply(packages, packageHistory)
  cores <- ifelse(length(packages) > 4, cores, 1L)
  parallel::mclapply(seq_along(dat), function(i) {
    sz <- size.data[[i]]
    ver <- version.data[[i]]
    tmp <- dat[[i]]
    latest.ver <- ver[nrow(ver), "Version"]
    latest.size <- sz[sz$version %in% latest.ver, ]
    leftover <- tmp[tmp$version != latest.ver, ]
    tmp <- tmp[tmp$version == latest.ver & tmp$size >= min(latest.size$bytes), ]
    rbind(tmp, leftover)
  }, mc.cores = cores)
}
