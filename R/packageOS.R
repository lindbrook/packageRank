#' OS specific packages.
#'
#' @export

packageOS <- function() {
  cran_db <- tools::CRAN_package_db()
  out <- cran_db[is.na(cran_db$OS_type) == FALSE, c("Package", "OS_type")]
  row.names(out) <- NULL
  out
}
