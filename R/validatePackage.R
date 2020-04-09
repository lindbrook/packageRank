#' Check for valid package names.
#'
#' @param packages Character. Vector of package name(s).
#' @export

validatePackage <- function(packages) {
  check <- vapply(packages, function(x) {
    class(try(pkgsearch::cran_package(x), silent = TRUE))
  }, character(1L))

  if (any(check == "try-error")) {
    list(invalid = names(check[check == "try-error"]),
         valid = names(check[check == "cran_package"]))
  } else packages
}
