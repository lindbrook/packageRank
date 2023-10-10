#' Amend "duplicated" CRAN and package download counts in  in cranlogs::cranDownloads().
#'
#' Eight days in 2012/2013 and thirteen days in 2023
#' @param out Object. An R list.
#' @note See getCorrectLogs().
#' @return An R list.
#' @noRd

fixCranlogs <- function(out) {
  err.dateA <- as.Date(names(packageRank::rstudio.logs))
  
  alpha <- as.Date("2023-09-19")
  omega <- as.Date("2023-10-01")
  err.dateB <- seq.Date(alpha, omega, by = "days")
  
  if (!is.null(out$packages)) {
    error.testA <- lapply(out$packages, function(p) {
      dat <- out$cranlogs.data
      err.dateA %in% dat[dat$package == p, "date"]
    })
    
    error.testB <- out$cranlogs.data$date %in% err.dateB
    
    if (any(unlist(error.testA))) {
      for (i in seq_along(error.testA)) {
        if (any(error.testA[[i]])) {
          dates.to.fix <- err.dateA[error.testA[[i]]]
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
    } else if (any(unlist(error.testB))) {
      count.fix <- out$cranlogs.data[error.testB, ]$count / 2L
      out$cranlogs.data[error.testB, "count"] <- count.fix
      
      recalc.cumsum <- tapply(out$cranlogs.data$count, 
        out$cranlogs.data$package, cumsum)
      recalc.cumsum <- unlist(recalc.cumsum[unique(out$cranlogs.data$package)])
      out$cranlogs.data$cumulative <- recalc.cumsum
    }

  } else {
    dat <- out$cranlogs.data
    error.testA <- err.dateA %in% dat$date
    error.testB <- dat$date %in% err.dateB
    
    if (any(error.testA)) {
      dates.to.fix <- err.dateA[error.testA]
      sel <- as.Date(names(packageRank::rstudio.logs)) %in% dates.to.fix
      correct.logs <- packageRank::rstudio.logs[sel]
      logs <- lapply(correct.logs, cleanLog)
      data.fix <- dat[dat$date %in% dates.to.fix, ]
      data.fix$count <- vapply(logs, nrow, integer(1L))
      sel <- out$cranlogs.data$date %in% dates.to.fix
      out$cranlogs.data[sel, ] <- data.fix
      out$cranlogs.data$cumulative <- cumsum(out$cranlogs.data$count)
    } else if (any(error.testB)) {
      count.fix <- dat[error.testB, ]$count / 2L
      dat[error.testB, "count"] <- count.fix
      dat$cumulative <- cumsum(dat$count)
      out$cranlogs.data <- dat
    }
  }
  out
}
