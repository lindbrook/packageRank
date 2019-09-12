#' Extract package Published from CRAN.
#'
#' Date of most recent version.
#' @param package Character. Package name.
#' @export

packagePublished <- function(package = "cholera") {
  root.url <- "https://CRAN.R-project.org/package"
  url <- paste0(root.url, "=", package)
  web_page <- readLines(url)

  published.check <- vapply(seq_along(web_page), function(i) {
    grepl("Published:", web_page[i])
  }, logical(1L))

  if (any(published.check)) {
    line.id <- which(published.check) + 1
    gsub("<.*?>", "", web_page[line.id])
  } else NA
}
