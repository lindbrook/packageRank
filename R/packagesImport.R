#' Extract package Imports from CRAN.
#'
#' @param package Character. Package name.
#' @export

packagesImport <- function(package = "cholera") {
  root.url <- "https://CRAN.R-project.org/package"
  url <- paste0(root.url, "=", package)
  web_page <- readLines(url)

  field.check <- vapply(seq_along(web_page), function(i) {
    grepl("Imports", web_page[i])
  }, logical(1L))

  if (any(field.check)) {
    line.id <- which(field.check) + 1
    line.parsed <- unlist(strsplit(web_page[line.id], '[/]'))
    pkgs <- seq(2, length(line.parsed), 3)
    pkgs <- pkgs[pkgs != rev(pkgs)[1]]
    line.parsed[pkgs]
  } else NA
}
