#' Use actual RStudio logs for "dupicated" 'cranlogs' results.
#'
#' Affects eight days in 2012/2013.
#' @param out Object. An R list.
#' @note See getCorrectLogs().
#' @return An R list.
#' @noRd

fixCranlogs <- function(out) {
  err.dates <- as.Date(names(packageRank::rstudio.logs))
  error.test <- err.dates %in% unique(out$cranlogs.data$date)

  if (any(error.test)) {
    dates.to.fix <- err.dates[error.test]
    sel <- as.Date(names(packageRank::rstudio.logs)) %in% dates.to.fix
    correct.logs <- packageRank::rstudio.logs[sel]
    logs <- lapply(correct.logs, cleanLog)

    if (is.null(out$packages)) {
      correct.ct <- vapply(correct.logs, nrow, integer(1L))
      sel <- out$cranlogs.data$date %in% dates.to.fix
      out$cranlogs.data[sel, "count"] <- correct.ct
      out$cranlogs.data$cumulative <- cumsum(out$cranlogs.data$count)
    } else {
      correct.ct <- lapply(logs, function(x) {
        vapply(out$packages, function(p) nrow(x[x$package == p, ]), integer(1L))
      })
      dt <- as.Date(names(correct.ct))
      ct <- c(do.call(rbind, correct.ct))
      pkg <- rep(out$packages, each = length(correct.ct))
      correct.data <- data.frame(date = dt, count = ct, package = pkg)

      sel <- out$cranlogs.data$date %in% correct.data$date &
             out$cranlogs.data$package %in% correct.data$package
      vars <- c("date", "count", "package")
      out$cranlogs.data[sel, vars] <- correct.data

      cumulative <- unlist(lapply(out$packages, function(p) {
        tmp <- out$cranlogs.data[out$cranlogs.data$package == p, ]
        cumsum(tmp$count)
      }))

      vars <- c("date", "count")
      out$cranlogs.data <- data.frame(out$cranlogs.data[, vars],
        cumulative = cumulative, package = out$cranlogs.data$package)
    }
  }
  out
}
