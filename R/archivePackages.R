#' Packages in CRAN archive.
#'
#' @param include.date Logical. Return data frame with package name and last publication date.
#' @export

archivePackages <- function(include.date = FALSE) {
  url <- "https://cran.r-project.org/src/contrib/Archive/"
  web_page <- readLines(url)

  archive.start <- vapply(seq_along(web_page), function(i) {
    grepl("Parent Directory", web_page[i])
  }, logical(1L))

  archive.stop <- vapply(seq_along(web_page), function(i) {
    grepl("colspan=", web_page[i])
  }, logical(1L))

  start <- which(archive.start) + 2
  stop <- which(archive.stop)[2] - 1
  web_page <- web_page[start:stop]

  readme.test <- vapply(web_page, function(x) grepl("README", x), logical(1L))
  if (any(readme.test)) web_page <- web_page[!readme.test]

  if (include.date) {
    out <- lapply(seq_along(web_page), function(i) {
      pkg.data <- gsub("<.*?>", "", web_page[i])
      parsed.data <- unlist(strsplit(pkg.data, "/"))
      pkg.name <- parsed.data[1]
      date.data <- parsed.data[2]
      date <- unlist(strsplit(date.data, " "))[1]
      data.frame(package = pkg.name, date = date, stringsAsFactors = FALSE)
    })
    do.call(rbind, out)

  } else {
    vapply(seq_along(web_page), function(i) {
      pkg.data <- gsub("<.*?>", "", web_page[i])
      unlist(strsplit(pkg.data, "/"))[1]
    }, character(1L))
  }
}
