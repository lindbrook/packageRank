#' Extract packge source file information from CRAN.
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export

packageInfo <- function(multi.core = TRUE, dev.mode = FALSE) {
  url <- "https://cran.r-project.org/src/contrib/"
  web_page <- readLines(url)

  if (.Platform$OS.type == "windows" & multi.core > 1) {
    cores <- 1L
  } else cores <- multiCore(multi.core)

  # parallel::parLapply() slower.
  if (dev.mode) {
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = "web_page")
    pkg.data <- parallel::parLapply(cl, web_page, function(x) {
      grepl("tar.gz", x)
    })
    parallel::stopCluster(cl)
  } else {
    pkg.data <- parallel::mclapply(web_page, function(x) grepl("tar.gz", x),
      mc.cores = cores)
  }

  pkg.data <- unlist(pkg.data)

  if (dev.mode) {
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = "web_page")
    dat <- parallel::parLapply(cl, which(pkg.data), function(x) {
      dat <- gsub("<.*?>", " ", web_page[x])
      dat <- trimws(unlist(strsplit(dat, '.tar.gz')))
      pkg.ver <- unlist(strsplit(dat[1], "_"))
      time.stamp <- unlist(strsplit(gsub("\\s+", " ", dat[2]), " "))
      data.frame(package = pkg.ver[1],
                 version = pkg.ver[2],
                 date = paste(time.stamp[1], time.stamp[2]),
                 size = time.stamp[3],
                 stringsAsFactors = FALSE)
    })
    parallel::stopCluster(cl)
  } else {
    dat <- parallel::mclapply(which(pkg.data), function(x) {
      dat <- gsub("<.*?>", " ", web_page[x])
      dat <- trimws(unlist(strsplit(dat, '.tar.gz')))
      pkg.ver <- unlist(strsplit(dat[1], "_"))
      time.stamp <- unlist(strsplit(gsub("\\s+", " ", dat[2]), " "))
      data.frame(package = pkg.ver[1],
                 version = pkg.ver[2],
                 date = paste(time.stamp[1], time.stamp[2]),
                 size = time.stamp[3],
                 stringsAsFactors = FALSE)
    }, mc.cores = cores)
  }

  do.call(rbind, dat)
}
