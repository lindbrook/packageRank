#' Filter out size anomalies (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @param packages Character. Vector of package name(s).
#' @param cores Integer. Number of cores for parallelization.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export

sizeFilter <- function(dat, packages, cores, dev.mode = dev.mode) {
  pkgs <- mpackages_partitioned()
  cran <- !packages %in% pkgs$archive.only
  
  if (any(packages %in% pkgs$archive.only)) {
    archive <- packages %in% pkgs$archive.only
  }
  
  if (exists("archive")) {
    if (any(archive)) {
      archive.pkgs <- packages[archive]
      archive.dat <- dat[archive]
      
      size.data <- lapply(archive.pkgs, packageArchive, check.package = FALSE, 
        size = TRUE)
      
      for (i in seq_along(size.data)) {
        size.data[[i]]$bytes <- computeFileSizeA(size.data[[i]]$Size)
      }
      
      version.data <- lapply(archive.pkgs, packageHistory, 
        check.package = FALSE)
      
      out <- parallel::mclapply(seq_along(archive.dat), function(i) {
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
      }, mc.cores = cores)
    }
  }

  if (any(cran)) {
    packages <- packages[cran]
    cran.dat <- dat[cran]
    
    size.data <- lapply(packages, cranPackageSize)
    version.data <- lapply(packages, packageHistory, check.package = FALSE)
    cores <- ifelse(length(packages) > 4, cores, 1L)
    # win.exception <- .Platform$OS.type == "windows" & cores > 1
    # if (dev.mode | win.exception) {
    if (dev.mode) {
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl = cl, envir = environment(),
        varlist = c("dat", "size.data", "version.data"))
      filtered <- parallel::parLapply(cl, seq_along(dat), function(i) {
        sz <- size.data[[i]]
        ver <- version.data[[i]]
        tmp <- dat[[i]]
        latest.ver <- ver[nrow(ver), "Version"]
        # Exception: source, no binary
        if (latest.ver %in% sz$version) {
          latest.size <- sz[sz$version %in% latest.ver, ]  
        } else {
          latest.ver <- ver[nrow(ver) - 1, "Version"]
          latest.size <- sz[sz$version %in% latest.ver, ]
        }

        sel <- tmp$version == latest.ver & tmp$size >= min(latest.size$bytes)
        current.ver <- tmp[sel, ]
        
        leftover.sel <- tmp$version != latest.ver
        
        if (any(leftover.sel)) {
          leftover <- tmp[leftover.sel, ]
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
          rbind(current.ver, past.ver)
        } else current.ver
      })
      parallel::stopCluster(cl)
    } else {
      if (.Platform$OS.type == "windows") cores <- 1L
      
      filtered <- parallel::mclapply(seq_along(cran.dat), function(i) {
        sz <- size.data[[i]]
        ver <- version.data[[i]]
        tmp <- cran.dat[[i]]
        latest.ver <- ver[nrow(ver), "Version"]
        # Exception: source, no binary
        if (latest.ver %in% sz$version) {
          latest.size <- sz[sz$version %in% latest.ver, ]  
        } else {
          latest.ver <- ver[nrow(ver) - 1, "Version"]
          latest.size <- sz[sz$version %in% latest.ver, ]
        }
        
        sel <- tmp$version == latest.ver & tmp$size >= min(latest.size$bytes)
        current.ver <- tmp[sel, ]
        
        leftover.sel <- tmp$version != latest.ver
        
        if (any(leftover.sel)) {
          leftover <- tmp[leftover.sel, ]
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
          rbind(current.ver, past.ver)
        } else current.ver
      }, mc.cores = cores)
    }
  }
  filtered
}
