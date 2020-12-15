#' Extract package data from MRAN (protoype).
#'
#' Binary or source size.
#' @param package Character. Package name.
#' @param date Character.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @note Depending on when syncrhonization occurred, you may need to add 3 or 4 days to CRAN publication date, see packageHistory(), to find the package or version you're looking for.
#' @export

packageMRAN <- function(package = "cholera", date = Sys.Date() - 1,
  check.package = TRUE, multi.core = TRUE) {

  cores <- multiCore(multi.core)
  if (check.package) package <- checkPackage(package)
  ymd <- check10CharDate(date, repository = "MRAN")
  mran.url <- "https://cran.microsoft.com/snapshot/"
  root.url <- paste0(mran.url, ymd)

  if (ymd < as.Date("2017-07-08")) {
    mac.url.suffix <- "/bin/macosx/mavericks/contrib/r-release/"
  } else if (ymd >= as.Date("2017-07-08")) {
    mac.url.suffix <- "/bin/macosx/el-capitan/contrib/r-release/"
  }

  src <- data.frame(file.extension = '.tar.gz',
                    url.suffix = "/src/contrib")
  mac <- data.frame(file.extension = '.tgz',
                    url.suffix = mac.url.suffix)
  win <- data.frame(file.extension = '.zip',
                    url.suffix = "/bin/windows/contrib/r-release/")

  args <- list(src = src, mac = mac, win = win)

  out <- parallel::mclapply(args, function(x) {
    pkgData(x$url.suffix, x$file.extension, root.url, package, ymd)
  }, mc.cores = cores)

  out <- do.call(rbind, out)
  out$type <- row.names(out)
  row.names(out) <- NULL
  out
}

pkgData <- function(url.suffix, file.extension, ymd, root.url = root.url,
  package = package) {

  url <- paste0(root.url, url.suffix)
  web_page <- mreadLines(url)
  pkg.sel <- grepl(file.extension, web_page, fixed = TRUE)
  pkg.data <- gsub("<.*?>", "", web_page[pkg.sel])
  pkg.data <- gsub("\\s+", " ", pkg.data)
  pkg.data <- strsplit(pkg.data, " ")
  pkg.match <- grepl(package, pkg.data, fixed = TRUE)

  if (sum(pkg.match) == 0) {
    stop("Not on MRAN (on date). Try adding 3 or 4 days to date.",
      call. = FALSE)
  } else if (sum(pkg.match) == 1) {
    pkg.data <- unlist(pkg.data[pkg.match])
  } else if (sum(pkg.match) > 1) {
    pkg.data <- pkg.data[pkg.match]
    pkg.nms <- vapply(pkg.data, function(x) {
      tmp <- unlist(strsplit(x[1], file.extension))
       unlist(strsplit(tmp, "_"))[1]
    }, character(1L))
    pkg.data <- unlist(pkg.data[pkg.nms == package])
  }

  tmp <- unlist(strsplit(pkg.data[1], file.extension))
  nm.ver <- unlist(strsplit(tmp, "_"))

  mo.id <- vapply(month.abb, function(m) grepl(m, pkg.data[2]), logical(1L))
  mo <- which(mo.id)
  mo <- ifelse(mo < 10, paste0("0", mo), mo)

  dt <- unlist(strsplit(pkg.data[2], "-"))
  dt[2] <- mo
  dt <- paste(rev(dt), collapse = "-")
  dt <- as.POSIXlt(dt, tz = "GMT")

  data.frame(package = nm.ver[1], version = nm.ver[2],
    size = as.numeric(pkg.data[4]), timestamp = dt, snapshot = ymd)
}
