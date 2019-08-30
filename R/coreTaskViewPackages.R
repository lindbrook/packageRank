#' Extract core CRAN Task View packages
#'
#' url Character. Task view URL.
#' @return A character vector of package names.
#' @export
#' @examples
#' # Bayesian Inference
#' url <- "https://cran.r-project.org/web/views/Bayesian.html"
#' coreTaskViewPackages(url)
#'
#' # Chemometrics and Computational Physics
#' url <- "https://cran.r-project.org/web/views/ChemPhys.html"
#' coreTaskViewPackages(url)

coreTaskViewPackages <- function(url) {
  web_page <- readLines(url)

  start <- which(vapply(seq_along(web_page), function(i) {
    grepl("CRAN packages:", web_page[i])
  }, logical(1L))) + 2

  stop <- which(vapply(seq_along(web_page), function(i) {
    grepl("Related links:", web_page[i])
  }, logical(1L))) - 3

  pkgs <- web_page[start:stop]

  id <- which(vapply(seq_along(pkgs), function(i) {
    grepl("core", pkgs[i] )
  }, logical(1L)))

  core.packages <- lapply(seq_along(pkgs[id]), function(i) {
    unlist(strsplit(pkgs[i], '[/]'))[3]
  })

  unlist(core.packages)
}
