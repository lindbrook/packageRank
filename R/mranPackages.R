#' Extract package data from MRAN (protoype).
#'
#' Binary or source size.
#' @param date Character.
#' @param type Character. "source", "mac", or "win".
#' @export

mranPackages <- function(date = Sys.Date() - 1, type = "source") {
  ymd <- check10CharDate(date, repository = "MRAN")
  mran.url <- "https://cran.microsoft.com/snapshot/"
  root.url <- paste0(mran.url, ymd)

  if (type == "source") {
    file.extension <- '.tar.gz'
    url.suffix <- "/src/contrib"
  } else if (type == "mac") {
    file.extension <- '.tgz'
    if (ymd < as.Date("2017-07-08")) {
      url.suffix <- "/bin/macosx/mavericks/contrib/r-release/"
    } else if (ymd >= as.Date("2017-07-08")) {
      url.suffix <- "/bin/macosx/el-capitan/contrib/r-release/"
    }
  } else if (type == "win") {
    file.extension <- '.zip'
    url.suffix <- "/bin/windows/contrib/r-release/"
  } else stop('type must be "source", "mac", or "win".')

  url <- paste0(root.url, url.suffix)
  web_page <- mreadLines(url)
  pkg.sel <- grepl(file.extension, web_page, fixed = TRUE)
  pkg.data <- gsub("<.*?>", "", web_page[pkg.sel])
  pkg.data <- gsub("\\s+", " ", pkg.data)
  pkg.data <- strsplit(pkg.data, " ")

  out <- lapply(pkg.data, function(x) {
    tmp <- unlist(strsplit(x[1], file.extension))
    nm.ver <- unlist(strsplit(tmp, "_"))

    mo.id <- vapply(month.abb, function(m) grepl(m, x[2]), logical(1L))
    mo <- which(mo.id)
    mo <- ifelse(mo < 10, paste0("0", mo), mo)

    dt <- unlist(strsplit(x[2], "-"))
    dt[2] <- mo
    dt <- paste(rev(dt), collapse = "-")
    dt <- as.POSIXlt(dt, tz = "GMT")

    data.frame(package = nm.ver[1], version = nm.ver[2],
      size = as.numeric(x[4]), timestamp = dt, snapshot = as.Date(date),
      type = type)
  })

  do.call(rbind, out)
}
