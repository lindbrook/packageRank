#' Imports dependencies.
#'
#' Documents cran.dependencies.
#' @export
#' @note 20 September 2019 : 14,960 (14,945)

cranDependencies <- function() {
  cran_db <- tools::CRAN_package_db()
  pkgs <- packageImports2(cran_db)
  # duplicates in Priority: recommended
  # names(pkgs)[duplicated(names(pkgs))]
  priority.recommended <- vapply(cran_db$Depends, function(x) {
    grepl("R \\(>= 3.7\\)", x)
  }, logical(1L))
  pkgs[-which(priority.recommended)]
}

packageImports2 <- function(cran_db) {
  out <- lapply(cran_db$Imports, function(x) {
    if (is.na(x)) {
      pkgs <- x
    } else {
      # specified package version & carriage returns)
      if (grepl("\\(", x) & grepl("\n", x)) {
        version.rm <- gsub("* \\(.*?\\) *", "", x)
        cr.rm <- unlist(strsplit(version.rm, ",\\n"))
        pkgs <- cr.rm

      # just carriage returns
      } else if (grepl("\n", x)) {
        cr.rm <- unlist(strsplit(x, ",\\n"))
        pkgs <- cr.rm

      # just specified package version
      } else if (grepl("\\(", x)) {
        version.rm <- gsub("* \\(.*?\\) *", "", x)
        pkgs <- version.rm
      
      } else pkgs <- x
      
    }  
    unlist(strsplit(pkgs, ", "))
  })
  stats::setNames(out, cran_db$Package)
}

# cran.dependencies <- cranDependencies()
# usethis::use_data(cran.dependencies, overwrite = TRUE)
