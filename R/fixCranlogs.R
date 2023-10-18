#' Amend "duplicated" CRAN and package download counts in cranlogs::cranDownloads().
#'
#' Eight days in 2012/2013 and thirteen days in 2023
#' @param out Object. An R list.
#' @note See getCorrectLogs().
#' @return An R list.
#' @noRd

fixCranlogs <- function(out) {
  err.date <- as.Date(names(packageRank::rstudio.logs))
  
  if (!is.null(out$packages)) {
    error.test <- lapply(out$packages, function(p) {
      dat <- out$cranlogs.data
      err.date %in% dat[dat$package == p, "date"]
    })
    
    if (any(unlist(error.test))) {
      for (i in seq_along(error.test)) {
        if (any(error.test[[i]])) {
          dates.to.fix <- err.date[error.test[[i]]]
          sel <- as.Date(names(packageRank::rstudio.logs)) %in% dates.to.fix
          correct.logs <- packageRank::rstudio.logs[sel]
          logs <- lapply(correct.logs, cleanLog)
          sel <- out$cranlogs.data$package == out$packages[i]
          dat <- out$cranlogs.data[sel, ]
          data.fix <- dat[dat$date %in% dates.to.fix, ]
          data.fix$count <- vapply(logs, function(x) {
            nrow(x[x$package == out$packages[i], ])
          }, integer(1L))
          
          sel <- out$cranlogs.data$package == out$packages[i] &
            out$cranlogs.data$date %in% dates.to.fix
          out$cranlogs.data[sel, ] <- data.fix
          
          recalc.cumsum <- unlist(tapply(out$cranlogs.data$count,
            out$cranlogs.data$package, cumsum))
          out$cranlogs.data$cumulative <- recalc.cumsum
        }
      }
    } 
  } else {
    dat <- out$cranlogs.data
    error.test <- err.date %in% dat$date
    
    if (any(error.test)) {
      dates.to.fix <- err.date[error.test]
      sel <- as.Date(names(packageRank::rstudio.logs)) %in% dates.to.fix
      correct.logs <- packageRank::rstudio.logs[sel]
      logs <- lapply(correct.logs, cleanLog)
      data.fix <- dat[dat$date %in% dates.to.fix, ]
      data.fix$count <- vapply(logs, nrow, integer(1L))
      sel <- out$cranlogs.data$date %in% dates.to.fix
      out$cranlogs.data[sel, ] <- data.fix
      out$cranlogs.data$cumulative <- cumsum(out$cranlogs.data$count)
    }
  }
  out
}
