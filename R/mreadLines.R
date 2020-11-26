#' Memoized readLines().
#'
#' @noRd

mreadLines <- memoise::memoise(readLines)
