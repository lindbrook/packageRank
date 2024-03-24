#' Filter out size anomalies (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @param packages Character. Vector of package name(s).
#' @noRd

sizeFilter <- function(dat, packages) {
  pkgs <- mpackages_partitioned()
  cran <- !packages %in% pkgs$archive.only
  
  if (packages %in% pkgs$archive.only) {
    archive <- packages %in% pkgs$archive.only
  }
  
  if (exists("archive")) {
    if (any(archive)) {
      archive.pkgs <- packages[archive]
      size.data <- packageArchive(archive.pkgs, size = TRUE)
      size.data$bytes <- computeFileSizeA(size.data$Size)
      version.data <- packageHistory(archive.pkgs, check.package = TRUE)
      latest.ver <- size.data[which.max(size.data$Date), "Version"]
      
      if (latest.ver %in% size.data$version) {
        latest.size <- size.data[size.data$version %in% latest.ver, ]  
      } else {
        obs.sz <- size.data[size.data$Version %in% unique(dat$version), ]
        latest.size <- obs.sz[which.max(obs.sz$Date), ]
      }
      
      sel <- dat$size >= min(latest.size$bytes)
      if (any(sel)) out <- dat[sel, ]
      else out <- dat
    }
  }

  if (cran) {
    packages <- packages[cran]
    
    size.data <- cranPackageSize(packages)
    version.data <- packageHistory(packages, check.package = FALSE)

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
