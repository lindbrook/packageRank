#' Extract package data from MRAN (protoype).
#'
#' Binary or source size.
#' @param package Character. Package name.
#' @param date Character.
#' @param type Character. "source", "mac", or "win".
#' @param check.package Logical. Validate and "spell check" package.
#' @export

packageMRAN <- function(package = "cholera", date = Sys.Date() - 1,
  type = "source", check.package = TRUE) {

  if (check.package) package <- checkPackage(package)
  ymd <- check10CharDate(date, repository = "MRAN")
  mran.url <- "https://cran.microsoft.com/snapshot/"
  root.url <- paste0(mran.url, ymd)

  if (type == "source") {
    url.suffix <- "/src/contrib"
    file.extension <- '.tar.gz'
  } else if (type == "mac") {
    url.suffix <- "/bin/macosx/el-capitan/contrib/r-release/"
    file.extension <- '.tgz'
  } else if (type == "win") {
    url.suffix <- "/bin/windows/contrib/r-release/"
    file.extension <- '.zip'
  } else stop('type must be "source", "mac", or "win".')

  url <- paste0(root.url, url.suffix)
  web_page <- mreadLines(url) # packageHistory.R
  pkg.sel <- grepl(file.extension, web_page, fixed = TRUE)
  pkg.data <- gsub("<.*?>", "", web_page[pkg.sel])
  pkg.data <- gsub("\\s+", " ", pkg.data)
  pkg.data <- strsplit(pkg.data, " ")
  pkg.match <- grepl(package, pkg.data, fixed = TRUE)

  if (sum(pkg.match) == 1) {
    pkg.data <- unlist(pkg.data[pkg.match])
  } else if (sum(pkg.match) > 1) {
    pkg.data <- pkg.data[pkg.match]
    pkg.nms <- vapply(pkg.data, function(x) {
      tmp <- unlist(strsplit(x[1], file.extension))
       unlist(strsplit(tmp, "_"))[1]
    }, character(1L))
    pkg.data <- unlist(pkg.data[pkg.nms == package])
  } else if (sum(pkg.match) == 0) {
    stop('Not on MRAN (on date). Check packageHistory(package) and add a day or two.')
  }

  tmp <- unlist(strsplit(pkg.data[1], file.extension))
  nm.ver <- unlist(strsplit(tmp, "_"))

  mo.id <- vapply(month.abb, function(m) grepl(m, pkg.data[2]), logical(1L))
  mo <- which(mo.id)
  mo <- ifelse(mo < 10, paste0("0", mo), mo)

  dt <- unlist(strsplit(pkg.data[2], "-"))
  dt[2] <- mo
  dt <- paste(rev(dt), collapse = "-")
  dt <- as.POSIXlt(dt, tz = "Europe/Vienna")

  data.frame(package = nm.ver[1], version = nm.ver[2],
    size = as.numeric(pkg.data[4]), timestamp = dt, snapshot = as.Date(date),
    type = type)
}
