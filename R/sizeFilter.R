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
      archive.dat <- dat[archive]
      
      size.data <- lapply(archive.pkgs, packageArchive, size = TRUE)
      
      for (i in seq_along(size.data)) {
        size.data[[i]]$bytes <- computeFileSizeA(size.data[[i]]$Size)
      }
      
      version.data <- lapply(archive.pkgs, packageHistory, 
        check.package = FALSE)
      
      out <- lapply(seq_along(archive.dat), function(i) {
        sz <- size.data[[i]]
        tmp <- archive.dat[[i]]
        latest.ver <- sz[which.max(sz$Date), "Version"]
        if (latest.ver %in% sz$version) {
          latest.size <- sz[sz$version %in% latest.ver, ]  
        } else {
          obs.sz <- sz[sz$Version %in% unique(tmp$version), ]
          latest.size <- obs.sz[which.max(obs.sz$Date), ]
        }
        
        sel <- tmp$size >= min(latest.size$bytes)
        if (!all(sel)) tmp[sel, ]
        else tmp
      })
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
      filtered <- rbind(current.ver, past.ver)
    } else filtered <- current.ver
  }
  filtered
}
