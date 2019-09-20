#' Extract package Imports from CRAN.
#'
#' @param package Character. Package name.
#' @export

packageImports <- function(package = "cholera") {
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

#' Extract package Imports from CRAN.
#'
#' @note Documents cran.dependencies.
#' @export

packageImports2 <- function() {
  cran_db <- tools::CRAN_package_db()
  imp <- lapply(cran_db$Imports, function(x) {
    if (is.na(x)) {
      pkgs <- x
    } else {
      # remove specified package version e.g., DT(>= 0.4)
      if (grepl("\\(", x)) {
        first.pass <- gsub(" *\\(.*?\\) *", "", x)
        carriage.returns <- grepl("\n", first.pass)
        if (any(carriage.returns)) {
          second.pass <- unlist(strsplit(first.pass, ",\\n"))
          pkgs <- unlist(strsplit(second.pass, ", "))
        } else pkgs <- unlist(strsplit(first.pass, ", "))
      } else pkgs <- unlist(strsplit(x, ", "))
    }
    pkgs
  })
  stats::setNames(imp, cran_db$Package)
}

# 20 September 2019 : 14,960 (14,945)
# cran.dependencies <- packageImports2()
# # names(cran.dependencies)[duplicated(names(cran.dependencies))]
# priority.recommended <- vapply(cran_db$Depends, function(x) {
#   grepl("R \\(>= 3.7\\)", x)
# }, logical(1L))
# cran.dependencies <- cran.dependencies[-which(priority.recommended)]
# usethis::use_data(cran.dependencies, overwrite = TRUE)
