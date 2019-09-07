#' Extract package dependencies from CRAN.
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @note Documents cran.dependencies. Uses 'miniCRAN' package.
#' @export

cranDependencies <- function(multi.core = TRUE) {
  cores <- multiCore(multi.core)
  pkg.names <- availablePackages()

  dependencies <- parallel::mclapply(pkg.names, function(p) {
    print(miniCRAN::pkgDep(p, enhances = FALSE, suggests = FALSE))
  }, mc.cores = cores)

  stats::setNames(dependencies, pkg.names)
  # usethis::use_data(cran.dependencies)
}
