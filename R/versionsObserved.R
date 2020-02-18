#' Count of package-version downloads.
#'
#' @param package Character. Package Name
#' @param date Character. Date. "yyyy-mm-dd".
#' @param all.history Logical. Include entire package history.
#' @param size.filter Logical or Numeric. If Logical, TRUE filters out downloads less than 1000 bytes. If Numeric, a postive value sets the minimum download size (in bytes) to consider; a negative value sets the maximum download size to consider.
#' @export

versionsObserved <- function(package = "cholera", date = Sys.Date() - 1,
  all.history = FALSE, size.filter = FALSE) {

  history <- packageHistory(package)
  obs <- packageLog(package, date = date)

  if (size.filter) {
    if (is.numeric(size.filter)) {
      if (size.filter >= 0) {
          obs <- obs[obs$size >= size.filter, ]
        } else if (size.filter < 0) {
          obs <- obs[obs$size < -size.filter, ]
        }
    } else if (is.logical(size.filter)) {
      obs <- obs[obs$size >= 1000, ]
    } else stop("'size.filter' must be Logical or Numeric.")
  }

  crosstab <- table(obs$version)
  crosstab <- as.data.frame(crosstab, stringsAsFactors = FALSE)
  if (all.history) {
    out <- merge(history, crosstab, by.x = "version", by.y = "Var1",
      all.x = TRUE)
  } else {
    out <- merge(history, crosstab, by.x = "version", by.y = "Var1")
  }
  out[order(out$date), ]
}
