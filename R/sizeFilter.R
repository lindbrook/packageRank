#' Filter out small downloads (prototype).
#'
#' @param dat Object. Package log entries.
#' @export

sizeFilter <- function(dat) {
  sm.test <- dat$size < 1000

  if (any(sm.test)) {
    small.byte <- dat[sm.test, ]
    dat2 <- dat[dat$ip %in% unique(small.byte$ip_id), ]
    dat2$t2 <- dat2$time

    time.stamp <- unique(dat2$time)
    time.window <- lapply(time.stamp, timeWindow)

    neighbor.test <- lapply(seq_along(time.stamp), function(i) {
      sel <- time.stamp %in% time.stamp[1:i] == FALSE
      time.window[[i]] %in% time.stamp[sel]
    })

    any.neighbors <- any(unlist(neighbor.test))

    if (any.neighbors) {
      neighbor.id <- which(vapply(neighbor.test, any, logical(1L)))
      window.id <- vapply(neighbor.test[neighbor.id], which, integer(1L))

      err <- vapply(seq_along(time.window[neighbor.id]), function(i) {
        time.window[neighbor.id][[i]][window.id[i]]
      }, character(1L))

      fix <- time.stamp[neighbor.id]

      for (i in seq_along(err)) {
        dat2[dat2$time %in% err[i], "t2"] <- fix[i]
      }
    }

    dat2$small.id <- paste0(dat2$ip_id, "-", dat2$t2)
    small.id <- dat2[dat2$size < 1000, "small.id"]

    small <- unlist(lapply(small.id , function(x) {
      sm.data <- dat2[dat2$small.id %in% x, ]
      as.numeric(row.names(sm.data[sm.data$size != max(sm.data$size), ]))
    }))

    dat[-small, ]
  } else dat
}
