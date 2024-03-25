#' Filter out size anomalies (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @param package Character. Package name.
#' @noRd

sizeFilter <- function(dat, package) {
  pkgs <- mpackages_partitioned()
  cran <- !package %in% pkgs$archive.only
  if (package %in% pkgs$archive.only) archive <- package %in% pkgs$archive.only
  
  if (exists("archive")) {
    if (archive) {
      archive.pkg <- package
      size.data <- packageArchive(archive.pkg, size = TRUE)
      size.data$bytes <- computeFileSizeA(size.data$Size)
        
      filter.out <- unlist(lapply(unique(dat$version), function(ver) {
        ver.size <- size.data[size.data$Version == ver, ]$bytes
        obs.size <- dat[dat$version == ver, ]$size
        obs.size < ver.size
      }))
  
      if (any(filter.out)) out <- dat[!filter.out, ]
      else out <- dat
    }
  }

  if (cran) {
    cran.pkg <- package
    size.data <- cranPackageSize(cran.pkg)
    version.data <- packageHistory(cran.pkg, check.package = FALSE)

    sz <- size.data
    ver <- version.data
   
    latest.ver <- ver[nrow(ver), "Version"]
    # Exception: source, no binary
    if (latest.ver %in% sz$version) {
      latest.size <- sz[sz$version %in% latest.ver, ]  
    } else {
      latest.ver <- ver[nrow(ver) - 1, "Version"]
      latest.size <- sz[sz$version %in% latest.ver, ]
    }
    
    sel <- dat$version == latest.ver & dat$size >= min(latest.size$bytes)
    current.ver <- dat[sel, ]
    
    leftover.sel <- dat$version != latest.ver
    
    if (any(leftover.sel)) {
      leftover <- dat[leftover.sel, ]
      leftover.pkg <- unique(leftover$package)
      leftover.history <- packageHistory0(leftover.pkg, size = TRUE)
      left.ver <- unique(leftover[, "version"])
      
      src.size <- vapply(left.ver, function(v) {
        leftover.history[leftover.history$Version == v, "Size"]
      }, character(1L))
      
      src.size <- vapply(src.size, computeFileSizeB, numeric(1L))
      left.data <- data.frame(version = left.ver, size = src.size)
      
      filtered <- lapply(seq_len(nrow(left.data)), function(i) {
        tmp <- left.data[i, ]
        sel <- leftover$version %in% tmp$version
        filter.test <- leftover$size >= tmp$size
        leftover[sel & filter.test, ]
      })
      
      past.ver <- do.call(rbind, filtered)
      out <- rbind(current.ver, past.ver)
    } else out <- current.ver
  }
  out
}
