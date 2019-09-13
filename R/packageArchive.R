#' Extract package archives from CRAN.
#'
#' @param package Character. Package name.
#' @export

packageArchive <- function(package = "cholera") {
  root.url <- "https://cran.r-project.org/src/contrib/Archive/"
  url <- paste0(root.url, package)

  if (RCurl::url.exists(url)) {
    web_page <- readLines(url)

    archive.check <- vapply(seq_along(web_page), function(i) {
      grepl("</td><td align=\"right\">", web_page[i])
    }, logical(1L))

    if (any(archive.check)) {
      line.id <- which(archive.check)[-1]
      archive.data <- lapply(line.id, function(i) {
        gsub("<.*?>", "", web_page[i])
      })
      version.date <- lapply(archive.data, function(x) {
        unlist(strsplit(x, '.tar.gz'))
      })
      version.date <-lapply(version.date, function(x) {
        data.frame(date = unlist(strsplit(x[2], " "))[1],
                   version = unlist(strsplit(x[1], "_"))[2],
                   stringsAsFactors = FALSE)
      })
      cbind(do.call(rbind, version.date), package)
    }
  } else NA
}
