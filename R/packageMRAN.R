#' Extract package data from MRAN (protoype).
#'
#' Binary or source size.
#' @param date Character.
#' @param type Character. "source", "mac", or "win".
#' @export

packageMRAN <- function(date = Sys.Date() - 1, type = "source") {
  ymd <- check10CharDate(date, repository = "MRAN")
  root.url <- paste0("https://cran.microsoft.com/snapshot/", ymd)

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
  web_page <- mreadLines(url)
  pkg.sel <- grepl(file.extension, web_page, fixed = TRUE)
  pkg.data <- gsub("<.*?>", "", web_page[pkg.sel])
  pkg.data <- gsub("\\s+", " ", pkg.data)
  pkg.data <- strsplit(pkg.data, " ")

  out <- lapply(pkg.data, function(x) {
    tmp <- unlist(strsplit(x[1], file.extension))
    ptA <- unlist(strsplit(tmp, "_"))
    data.frame(package = ptA[1], version = ptA[2], date = x[2], time = x[3],
      size = x[4], snapshot = date, type = type)
  })

  do.call(rbind, out)
}
