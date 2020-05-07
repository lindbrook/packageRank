#' Extract package information from CRAN.
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @param platform Character.
#' @param r.ver Character.
#' @param source Logical.
#' @export

packageInfo <- function(multi.core = TRUE, platform = "win", r.ver = "release",
  source = TRUE) {

  if (.Platform$OS.type == "windows" & multi.core > 1) {
    cores <- 1L
  } else {
    cores <- multiCore(multi.core)
  }

  base.url <- "https://cran.r-project.org/"

  # source url
  source.url <- "src/contrib/"

  # win binary urls
  r.devel.win <- "bin/windows/contrib/4.1/"
  r.release.win <- "bin/windows/contrib/4.0/"
  r.oldrel.win <- "bin/windows/contrib/3.6/"

  # mac binary urls
  r.release.mac <- "bin/macosx/contrib/4.0/"
  r.oldrel.mac <- "bin/macosx/el-capitan/contrib/3.6/"

  if (source) {
    url <- paste0(base.url, source.url)
  } else {
    if (platform == "mac") {
      if (r.ver == "release") {
        url <- paste0(base.url, r.release.mac)
      } else if (r.ver == "oldrel") {
        url <- paste0(base.url, r.oldrel.mac)
      } else {
        stop('For Mac, ver must be "release", or "oldrel".')
      }
    } else if (platform == "win") {
      if (r.ver == "devel") {
        url <- paste0(base.url, r.devel.win)
      } else if (r.ver == "release") {
        url <- paste0(base.url, r.release.win)
      } else if (r.ver == "oldrel") {
        url <- paste0(base.url, r.oldrel.win)
      } else {
        stop('For Win, ver must be "devel", "release", or "oldrel".')
      }
    } else {
      stop('If binary file, platform must be "mac" or "win".')
    }
  }

  web_page <- readLines(url)

  if (source) {
    ext <- ".tar.gz"
  } else {
    if (platform == "mac") {
      ext <- ".tgz"
    } else if (platform == "win") {
      ext <- ".zip"
    }
  }

  pkg.data <- parallel::mclapply(web_page, function(x) grepl(ext, x),
    mc.cores = cores)

  pkg.data <- unlist(pkg.data)

  pkg_info <- function(x, ext) {
    dat <- gsub("<.*?>", " ", web_page[x])
    dat <- trimws(unlist(strsplit(dat, ext)))
    pkg.ver <- unlist(strsplit(dat[1], "_"))
    stamp <- unlist(strsplit(gsub("\\s+", " ", dat[2]), " "))
    data.frame(package = pkg.ver[1],
               version = pkg.ver[2],
               date = paste(stamp[1], stamp[2]),
               size = stamp[3],
               stringsAsFactors = FALSE)
  }

  dat <- parallel::mclapply(which(pkg.data), function(x) {
    pkg_info(x, ext)
  }, mc.cores = cores)

  pkg.info <- do.call(rbind, dat)
  kB.id <- grep("K", pkg.info$size)
  MB.id <- grep("M", pkg.info$size)
  kB <- as.numeric(unlist(strsplit(pkg.info[kB.id, "size"], "K"))) * 10^3
  MB <- as.numeric(unlist(strsplit(pkg.info[MB.id, "size"], "M"))) * 10^6
  pkg.info$byte <- NA
  pkg.info[kB.id, "byte"] <- kB
  pkg.info[MB.id, "byte"] <- MB
  pkg.info
}
