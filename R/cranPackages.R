#' Scrape CRAN package information.
#'
#' Current version, date and size (source and binary).
#' @param binary Logical. Compute size of binary files.
#' @param bytes Logical. Compute approximate numeric file size in bytes.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

cranPackages <- function(binary = FALSE, bytes = FALSE, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  dat <- getData(code = "source", cores)
  if (binary) dat <- getBinaryData(dat, cores)
  if (bytes) {
    if("size" %in% names(dat)) {
      dat$bytes <- computeFileSize(dat$size)
    } else if (all(c("source", "mac", "win") %in% names(dat))) {
      for (nm in c("source", "mac", "win")) {
        dat[, paste0(nm, ".B")] <- NA
        dat[, paste0(nm, ".B")] <- computeFileSize(dat[, nm])
      }
    }
  }
  dat
}

computeFileSize <- function(x) {
  kB.id <- grepl("K", x)
  MB.id <- grepl("M", x)
  kB <- as.numeric(unlist(strsplit(x[kB.id], "K"))) * 10^3
  MB <- as.numeric(unlist(strsplit(x[MB.id], "M"))) * 10^6
  out <- rep(NA, length(x))
  out[kB.id] <- kB
  out[MB.id] <- MB
  out
}

getData <- function(code = "source", cores) {
  if (code == "source") {
    url <- "https://cran.r-project.org/src/contrib/"
  } else if (code == "mac") {
    url <- "https://cran.r-project.org/bin/macosx/contrib/r-release/"
  } else if (code == 'win') {
    url <- "https://cran.r-project.org/bin/windows/contrib/r-release/"
  }
  web.data <- scrapeData(url)
  toDataFrame(web.data, code = code, cores)
}

getBinaryData <- function(dat, cores) {
  platform <- c("mac", "win")
  binary.data <- lapply(platform, function(x) getData(x, cores))
  names(binary.data) <- platform
  tmp <- merge(dat, binary.data$mac[, c("package", "size")], by = "package")
  names(tmp)[grep("size", names(tmp))] <- c("source", "mac")
  tmp <- merge(tmp, binary.data$win[, c("package", "size")], by = "package")
  names(tmp)[names(tmp) == "size"] <- "win"
  vars <- c("package", "version", "date", "source", "mac", "win",
    "repository")
  tmp[, vars]
}

scrapeData <- function(url) {
  web_page <- mreadLines(url)
  pkg.id <- grepString("compressed.gif", web_page)
  dat <- gsub("<.*?>", "", web_page[pkg.id])
  dat[!grepl("PACKAGES", dat, fixed = TRUE)] # error 2020-09-22
}

toDataFrame <- function(web.data, code = "source", cores) {
  if (code == "source") extension <- '.tar.gz'
  if (code == "win") extension <- '.zip'
  if (code == "mac") extension <- '.tgz'

  out <- parallel::mclapply(seq_along(web.data), function(i) {
    dat <- unlist(strsplit(web.data[i], extension, fixed = TRUE))
    ptA <- unlist(strsplit(dat[1], "_"))
    ptB <- unlist(strsplit(dat[2], " "))
    data.frame(package = ptA[1],
               version = ptA[2],
               date = as.Date(ptB[1]),
               size = unlist(strsplit(ptB[length(ptB)], "&nbsp;")),
               repository = "CRAN",
               stringsAsFactors = FALSE)
  }, mc.cores = cores)

  do.call(rbind, out)
}
