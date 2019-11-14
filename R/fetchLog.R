#' Fetch Package Logs.
#'
#' @param x Character. URL
#' @import data.table memoise
#' @export
#' @note mFetchLog() is memoized version.

fetchLog <- function(x) as.data.frame(data.table::fread(x))
mfetchLog <- memoise::memoise(fetchLog)
