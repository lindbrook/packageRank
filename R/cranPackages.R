#' Current R packages (vector of package names).
#'
#' Uses tools::CRAN_package_db().
#' @export

cranPackages <- function() {
  cran_db <- tools::CRAN_package_db()
  priority.recommended <- vapply(cran_db$Depends, function(x) {
    grepl("R \\(>= 3.7\\)", x)
  }, logical(1L))
  cran_db[!priority.recommended, "Package"]
}

