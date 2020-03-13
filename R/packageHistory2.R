#' Extract package version history CRAN and Archive (prototype).
#'
#' Date and version of all publications.
#' @param package Character. Package name.
#' @param short.date Logical
#' @export

packageHistory2 <- function(package = "cholera", short.date = TRUE) {
  vars <- c("Package", "Version", "Date/Publication", "crandb_file_date",
    "date")
  history <- pkgsearch::cran_package_history(package)[vars]
  history <- data.frame(history)

  all.archive <- pkgsearch::cran_package(package, "all")$archived

  if (all.archive) {
    repository <- rep("Archive", nrow(history))
  } else {
    repository <- c(rep("Archive", nrow(history) - 1), "CRAN")
  }

  if (short.date) {
    date <- strsplit(history$Date.Publication, "[ ]")
    date <- as.Date(vapply(date, function(x) x[1], character(1L)))
    data.frame(history[, c("Package", "Version")], Date = date,
      Repository = repository, stringsAsFactors = FALSE)
  } else {
    data.frame(history, Repository = repository, stringsAsFactors = FALSE)
  }
}
