#' Extract package Depends from CRAN.
#'
#' @param package Character. Package name.
#' @export

packageDepends <- function(package = "cholera") {
  root.url <- "https://CRAN.R-project.org/package"
  url <- paste0(root.url, "=", package)
  web_page <- readLines(url)

  depends.check <- vapply(seq_along(web_page), function(i) {
    grepl("Depends", web_page[i])
  }, logical(1L))

  if (any(depends.check)) {
    line.id <- which(depends.check) + 1
    pkgs <- gsub("<.*?>", "", web_page[line.id])
    pkgs <- unlist(strsplit(pkgs, ", "))

    R.check <- vapply(pkgs, function(x) grepl("R ", x), logical(1L))

    if (sum(R.check) > 1) {
      stop("Error.")
    } else if (any(R.check) & length(pkgs) > 1) {
      pkgs <- pkgs[R.check == FALSE]
    } else if (any(R.check) & length(pkgs) == 1) {
      pkgs <- NA
    }

    version.check <- vapply(pkgs, function(x) grepl(" ", x), logical(1L))

    if (any(version.check)) {
      version.parsed <- lapply(names(which(version.check)), function(nm) {
        vec <- unlist(strsplit(nm, " "))
        data.frame(pkg = vec[1], ver = unlist(strsplit(vec[3], ")")),
          stringsAsFactors = FALSE)
      })

      # version placeholder
      version.parsed <- do.call(rbind, version.parsed)
      pkgs <- sort(version.parsed$pkg)
    }

    pkgs
  } else NA
}
