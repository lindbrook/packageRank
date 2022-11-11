#' Extract package or R version history.
#'
#' Date and version of all publications.
#' @param package Character. Vector of package names (including "R").
#' @param check.package Logical. Validate and "spell check" package.
#' @export

packageHistoryB <- function(package = "cholera", check.package = TRUE) {
  package0 <- package

  if ("R" %in% package) {
    pkg.idx <- seq_along(package)
    r.position <- which(package == "R")
    pkg.idx <- pkg.idx[pkg.idx != r.position]

    r_v <- rversions::r_versions()
    names(r_v) <- tools::toTitleCase(names(r_v))
    r_v$Date <- as.Date(r_v$Date)
    nms <- names(r_v)
    r_v$Package <- "R"
    r_v <- list(r_v[, c("Package", nms)])

    package <- package[-r.position]
  }

  if (check.package) package <- checkPackage(package)

  # Use packageHistory0() for "missing" and latest packages.
  # e.g.,"VR" in cran_package() but not cran_package_history()
  history <- try(lapply(package, pkgsearch::cran_package_history),
    silent = TRUE)

  if (any(class(history) == "try-error")) {
    out <- lapply(package, packageHistory0)
  } else {
    out <- lapply(history, function(x) {
      if ("Repository" %in% colnames(x)) {
         tmp <- data.frame(x[, c("Package", "Version", "date", "Repository")])
         row.names(tmp) <- NULL
         tmp$Date <- format(as.Date(tmp$date), "%Y-%m-%d")
         tmp$date <- NULL
         if (nrow(tmp) > 1) tmp[-nrow(tmp), "Repository"] <- "Archive"
         tmp <- tmp[, c("Package", "Version", "Date", "Repository")]
      } else {
        tmp <- data.frame(x[, c("Package", "Version", "date")])
        row.names(tmp) <- NULL
        tmp$Date <- format(as.Date(tmp$date), "%Y-%m-%d")
        tmp$date <- NULL
        tmp$Repository <- "Archive"
      }
     tmp
    })
  }

  if ("R" %in% package0) {
    out <- c(out[seq_along(out) < r.position], r_v,
      out[seq_along(out) >= r.position])
  }

  if (length(out) == 1) out[[1]]
  else out
}
