#' Scrape package data from CRAN.
#'
#' Version, date and size (source file) of most recent publication.
#' @param package Character. Package name.
#' @param check.package Logical. Validate and "spell check" package.
#' @param size Logical. Include size of source file.
#' @param r.ver Character. Current R version; used in directory path.
#' @param mac.ver Character. Processor.
#' @param bytes Logical. Compute approximate file size (bytes).
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame or NULL.
#' @noRd

cranPackageSize <- function(package = "cholera", check.package = FALSE,
  size = TRUE, r.ver = "4.4",  mac.ver = "arm", bytes = TRUE, 
  multi.core = FALSE) {

  # R default is 60
  orig.timeout <- getOption("timeout")
  if (orig.timeout < 600L) options(timeout = 600L)

  if (check.package) package <- checkPackage(package)
  cores <- multiCore(multi.core)
  root.url <- "https://cran.r-project.org/"
  
  if (mac.ver == "arm") {
    mac.url.suffix <- paste0("bin/macosx/big-sur-arm64/contrib/", r.ver, "/")
  } else if (mac.ver == "intel") {
    mac.url.suffix <- paste0("bin/macosx/big-sur-x86_64/contrib/", r.ver, "/")
  } else stop('mac.ver must be "arm" or "intel".')
  
  win.url.suffix <- paste0("bin/windows/contrib/", r.ver, "/")

  src <- data.frame(ext = ".tar.gz", url = paste0(root.url, "/src/contrib"))
  mac <- data.frame(ext = ".tgz", url = paste0(root.url, mac.url.suffix))
  win <- data.frame(ext = ".zip", url = paste0(root.url, win.url.suffix))

  arguments <- list(src = src, mac = mac, win = win)

  out <- parallel::mclapply(arguments, function(x) {
    web_page <- mreadLines(x$url)
    pkg.match <- grepl(package, web_page, fixed = TRUE)

    if (any(pkg.match)) {
      pkg.data <- gsub("<.*?>", "", web_page[pkg.match])
      if (length(pkg.data) > 1) {
        multiple.matches <- unname(vapply(pkg.data, function(x) {
          unlist(strsplit(x[1], "_"))[1]
        }, character(1L)))
        if (package %in% multiple.matches) {
          sel <- multiple.matches %in% package
          # package update transition (multiple versions)
          if (length(unique(multiple.matches)) == 1) {
            pkg.data <- pkg.data[sel][length(pkg.data)]
          } else {
            pkg.data <- pkg.data[sel]
          }
          out <- cran_package_info(pkg.data, x$ext)
          } else out <- NULL
      } else if (length(pkg.data) == 1) {
        out <- cran_package_info(pkg.data, x$ext)
      }

      if (!is.null(out)) {
        if (identical(out$package, package)) {
          if (size) out
          else out[, names(out) != "size"]
        }
      } else NA
    } else NA
  }, mc.cores = cores)

  options(timeout = orig.timeout)
  out <- do.call(rbind, out)
  out$type <- row.names(out)
  row.names(out) <- NULL
  if (bytes) out$bytes <- computeFileSizeB(out$size)
  out
}

cran_package_info <- function(pkg.data, ext, repository = "CRAN") {
  dat <- unlist(strsplit(pkg.data, ext))
  ptA <- unlist(strsplit(dat[1], "_"))
  ptB <- unlist(strsplit(dat[2], " "))
  data.frame(package = ptA[1],
             version = ptA[2],
             date = as.Date(ptB[1]),
             size = unlist(strsplit(ptB[length(ptB)], "&nbsp;")),
             repository = "CRAN",
             stringsAsFactors = FALSE)
}

computeFileSizeB <- function(x) {
  kB.test <- grepl("K", x)
  MB.test <- grepl("M", x)
  out <- rep(NA, length(x))
  if (any(kB.test)) {
    out[kB.test] <- as.numeric(unlist(strsplit(x[kB.test], "K"))) * 10^3
  }
  if (any(MB.test)) {
    out[MB.test] <- as.numeric(unlist(strsplit(x[MB.test], "M"))) * 10^6
  }
  out
}
