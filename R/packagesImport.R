#' Extract package Imports from CRAN.
#'
#' @param package Character. Package name.
#' @export

packagesImport <- function(package = "cholera") {
  root.url <- "https://CRAN.R-project.org/package"
  url <- paste0(root.url, "=", package)
  web_page <- readLines(url)

  imports.check <- vapply(seq_along(web_page), function(i) {
    grepl("Imports", web_page[i])
  }, logical(1L))

  if (any(imports.check)) {
    line.id <- which(imports.check) + 1
    pkgs <- gsub("<.*?>", "", web_page[line.id])
    pkgs <- unlist(strsplit(pkgs, ", "))

    version.check <- vapply(pkgs, function(x) grepl(" ", x), logical(1L))

    if (any(version.check)) {
      version.removed <- lapply(names(which(version.check)), function(nm) {
        unlist(strsplit(nm, " "))[1]
      })
      pkgs <- sort(c(pkgs[version.check == FALSE], unlist(version.removed)))
    }

    pkgs

  } else NA
}
