#' Amend "duplicated" R application download counts in cranlogs::cranDownloads().
#'
#' Twenty days between 2023-09-13 through 2023-10-02.
#' @param out Object. An R list.
#' @note See getCorrectLogs().
#' @return An R list.
#' @noRd
  
fixRCranlogs <- function(out) {  
  dates <- seq.Date(as.Date("2023-09-12"), as.Date("2023-10-02"), by = "days")
  rlog.err <- dates[!dates %in% "2023-09-28"] # 1:1
  sep30 <- rlog.err %in% "2023-09-30"
  
  rlog.2x.err <- out$cranlogs.data$date %in% rlog.err[!sep30] # 2:1
  rlog.3x.err <- out$cranlogs.data$date %in% rlog.err[sep30]  # 3:1
  
  if (any(rlog.3x.err)) {
    count.fix <- out$cranlogs.data[rlog.3x.err, ]$count / 3L
    out$cranlogs.data[rlog.3x.err, "count"] <- count.fix
  }
    
  if (any(rlog.2x.err)) {
    count.fix <- out$cranlogs.data[rlog.2x.err, ]$count / 2L
    out$cranlogs.data[rlog.2x.err, "count"] <- count.fix
  }
  
  recalc <- tapply(out$cranlogs.data$count, out$cranlogs.data$platform, cumsum)
  out$cranlogs.data$cumulative <- unlist(recalc)
  out
}