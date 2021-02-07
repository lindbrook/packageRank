#' Filter out size anomalies (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @param packages Character. Vector of package name(s).
#' @export

sizeFilter <- function(dat, packages) {
  size.data <- lapply(packages, cranPackageSize)
  lapply(seq_along(dat), function(i) {
    sz <- size.data[[i]]
    tmp <- dat[[i]]
    leftover <- tmp[tmp$version != unique(sz$version), ]
    tmp <- tmp[tmp$version == unique(sz$version), ]
    tmp <- tmp[tmp$size >= min(sz$bytes), ]
    rbind(tmp, leftover)
  })
}
