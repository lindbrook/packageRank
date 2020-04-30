#' Extract packge source file information from CRAN.
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac?Unix only.
# #' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export

packageInfo <- function(multi.core = TRUE) {
  url <- "https://cran.r-project.org/src/contrib/"
  web_page <- readLines(url)

  if (.Platform$OS.type == "windows" & multi.core > 1) {
    cores <- 1L
  } else {
    cores <- multiCore(multi.core)
  }

  pkg.data <- parallel::mclapply(web_page, function(x) grepl("tar.gz", x),
    mc.cores = cores)

  pkg.data <- unlist(pkg.data)

  pkg_info <- function(x) {
    dat <- gsub("<.*?>", " ", web_page[x])
    dat <- trimws(unlist(strsplit(dat, ".tar.gz")))
    pkg.ver <- unlist(strsplit(dat[1], "_"))
    stamp <- unlist(strsplit(gsub("\\s+", " ", dat[2]), " "))
    data.frame(package = pkg.ver[1],
               version = pkg.ver[2],
               date = paste(stamp[1], stamp[2]),
               size = stamp[3],
               stringsAsFactors = FALSE)
  }

  dat <- parallel::mclapply(which(pkg.data), pkg_info, mc.cores = cores)

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
