#' Filter downloads of full-sized sequential versions (prototype).
#'
#' @param pkg.data Object.
#' @param arch.pkg.history Object.
#' @param download.time Numeric. Package download time allowance (seconds).
#' @export

sequenceFilter <- function(pkg.data, arch.pkg.history, download.time = 30) {
  sequences <- identifySequences(pkg.data, arch.pkg.history,
    download.time = download.time)
  if (!is.null(sequences)) {
    delete <- row.names(sequences)
    if (!is.null(delete)) {
      pkg.data[!row.names(pkg.data) %in% delete, ]
    } else pkg.data
  } else pkg.data
}
