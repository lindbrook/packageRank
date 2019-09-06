#' Get vector of available packages on CRAN.
#'
#' Scrapes https://cran.r-project.org/web/packages
#' @note  More up to date than utils::available.packages().
#' @export

availablePackages <- function() {
  url.a <- "https://cran.r-project.org/web/packages"
  url.b <- "/available_packages_by_name.html"
  web_page <- readLines(paste0(url.a, url.b))

  start <- which(vapply(seq_along(web_page), function(i) {
    grepl("Available CRAN packages by name.", web_page[i])
  }, logical(1L))) + 3

  stop <- which(vapply(seq_along(web_page), function(i) {
    grepl("</table>", web_page[i])
  }, logical(1L))) - 1

  pages <- web_page[start:stop]
  pkgs <- lapply(pages, function(x) unlist(strsplit(x, '[/]'))[5])
  pkgs <- unlist(pkgs)
  pkgs[!is.na(pkgs)]
}
