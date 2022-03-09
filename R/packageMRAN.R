#' Extract package data from MRAN (prototype).
#'
#' Binary or source size.
#' @param package Character. Vector of package name(s).
#' @param date Character. NULL uses latest available log.
#' @param check.package Logical. Validate and "spell check" package.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @note Depending on when synchronization occurred, you may need to add 3 or 4 days to CRAN publication date, see packageHistory(), to find the package or version you're looking for.
#' @export

packageMRAN <- function(package = "cholera", date = NULL, check.package = TRUE,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)
  if (check.package) package <- checkPackage(package)

  ymd <- logDate(date, repository = "MRAN")

  mran.url <- "https://cran.microsoft.com/snapshot/"
  root.url <- paste0(mran.url, ymd)

  if (ymd < as.Date("2017-07-08")) {
    mac.url.suffix <- "/bin/macosx/mavericks/contrib/r-release/"
  } else if (ymd >= as.Date("2017-07-08")) {
    mac.url.suffix <- "/bin/macosx/el-capitan/contrib/r-release/"
  }

  src <- data.frame(extension = '.tar.gz',
                    url = paste0(root.url, "/src/contrib"))
  mac <- data.frame(extension = '.tgz',
                    url = paste0(root.url, mac.url.suffix))
  win <- data.frame(extension = '.zip',
                    url = paste0(root.url, "/bin/windows/contrib/r-release/"))

  arguments <- list(src = src, mac = mac, win = win)

  if (length(package) == 1) {
    out <- parallel::mclapply(arguments, function(x) {
      pkgData(x$url, x$extension, ymd, package)
    }, mc.cores = cores)

    if (!is.null(package)) {
      out <- do.call(rbind, out)
      out$type <- row.names(out)
      row.names(out) <- NULL
    } else {
      names(out) <- names(arguments)
    }
  } else if (length(package) > 1) {
    out <- parallel::mclapply(package, function(p) {
      p.data <- lapply(arguments, function(x) {
        pkgData(x$url, x$extension, ymd, p)
      })

      if (!is.null(p)) {
        p.data <- do.call(rbind, p.data)
        p.data$type <- row.names(p.data)
        row.names(p.data) <- NULL
      } else {
        names(p.data) <- names(arguments)
      }
      p.data
    }, mc.cores = cores)
  }
  out
}

pkgData <- function(url, extension, ymd, package = package) {
  web_page <- mreadLines(url)
  pkg.sel <- grepl(extension, web_page, fixed = TRUE)
  pkg.data <- gsub("<.*?>", "", web_page[pkg.sel])
  pkg.data <- gsub("\\s+", " ", pkg.data)
  pkg.data <- strsplit(pkg.data, " ")

  if (!is.null(package)) {
    pkg.match <- grepl(package, pkg.data, fixed = TRUE)

    if (sum(pkg.match) == 0) {
      stop("Not on MRAN (on date). Try adding 3 or 4 days to date.",
        call. = FALSE)
    } else if (sum(pkg.match) == 1) {
      pkg.data <- pkg.data[pkg.match]
    } else if (sum(pkg.match) > 1) {
      pkg.data <- pkg.data[pkg.match]
      pkg.nms <- vapply(pkg.data, function(x) {
        tmp <- unlist(strsplit(x[1], extension))
        unlist(strsplit(tmp, "_"))[1]
      }, character(1L))
      pkg.data <- pkg.data[pkg.nms == package]
    }
  }

  out <- lapply(pkg.data, function(x) {
    tmp <- unlist(strsplit(x[1], extension))
    nm.ver <- unlist(strsplit(tmp, "_"))

    mo.id <- vapply(month.abb, function(m) grepl(m, x[2]), logical(1L))
    mo <- which(mo.id)
    mo <- ifelse(mo < 10, paste0("0", mo), mo)

    dt <- unlist(strsplit(x[2], "-"))
    dt[2] <- mo
    dt <- paste(rev(dt), collapse = "-")
    dt <- as.POSIXlt(dt, tz = "GMT")

    data.frame(package = nm.ver[1], version = nm.ver[2],
      size = as.numeric(x[4]), timestamp = dt, snapshot = ymd)
  })

  do.call(rbind, out)
}
