#' Vector of all packages in archive.
#'
#' @export

archivePackages <- function() {
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

  vapply(seq_along(web_page), function(i) {
    pkg.data <- gsub("<.*?>", "", web_page[i])
    unlist(strsplit(pkg.data, "/"))[1]
  }, character(1L))
}
