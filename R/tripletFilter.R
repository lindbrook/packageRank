#' Filter out small downloads triplets (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @param time.window Numeric. Seconds.
#' @export

tripletFilter <- function(dat, time.window = 2) {
  ver <- unique(dat$version)
  out <- lapply(ver, function(v) {
    v.data <- dat[dat$version == v, ]
    if (nrow(v.data) < 3) {
      v.data
    } else {
      v.data$machine <- paste0(v.data$ip_id, "-", v.data$r_version, "-",
        v.data$r_arch, "-", v.data$r_os)
      v.data$id <- paste0(v.data$time, "-", v.data$machine)

      if (nrow(v.data) == 3 & length(unique(v.data$id)) == 1) {
        if (any(v.data$size < 1000)) {
          size.heterogeneity <- length(unique(round(log10(v.data$size)))) == 3
          sz <- round(log10(v.data$size))
          sm.pkg.triA <- any(sz <= 6 & sz > 3)
          sm.pkg.triB <- sum(sz == max(sz)) == 2
          small.package.triplet <- sm.pkg.triA & sm.pkg.triB
          if (size.heterogeneity) {
            obs <- v.data[v.data$size != max(v.data$size), ]
            tri.delete <- as.numeric(row.names(obs))
          } else if (small.package.triplet) {
            z <- v.data[v.data$size != max(v.data$size), ]
            tri.delete <- as.numeric(row.names(z))
          } else {
            tri.delete <- NULL
          }
        }

        if (!is.null(tri.delete)) {
          v.data <- v.data[row.names(v.data) %in% tri.delete == FALSE, ]
        }

      } else {
        freqtab <- table(v.data$id)
        triplets <- names(freqtab[freqtab == 3])
        if (!is.null(triplets)) {
          tri.delete <- unlist(lapply(triplets, function(id) {
            tmp <- v.data[v.data$id %in% id, ]
            if (any(tmp$size < 1000)) {
              size.heterogeneity <- length(unique(round(log10(tmp$size)))) == 3
              sz <- round(log10(tmp$size))
              sm.pkg.triA <- any(sz <= 6 & sz > 3)
              sm.pkg.triB <- sum(sz == max(sz)) == 2
              small.package.triplet <- sm.pkg.triA & sm.pkg.triB
              if (size.heterogeneity) {
                obs <- tmp[tmp$size != max(tmp$size), ]
                as.numeric(row.names(obs))
              } else if (small.package.triplet) {
                sel <- tmp$size != max(tmp$size)
                as.numeric(row.names(tmp[sel, ]))
              } else NULL
            }
          }))
        } else tri.delete <- NULL

        if (!is.null(tri.delete)) {
          v.data <- v.data[row.names(v.data) %in% tri.delete == FALSE, ]
        }

        small.id <- unique(v.data[v.data$size < 1000, "id"])

        if (length(small.id) != 0) {
          if (!is.null(triplets)) {
            possible.triplets <- setdiff(small.id, triplets)
          } else {
            possible.triplets <- small.id
          }

          time.fix <- lapply(possible.triplets, function(x) {
            possible.data <- v.data[v.data$id %in% x, ]
            possible.data$date.time <- as.POSIXlt(paste(possible.data$date,
              possible.data$time), tz = "Europe/Vienna")

            before.after <- c(possible.data$date.time + 1:time.window,
                              possible.data$date.time - 1:time.window)
            before.after <- strftime(before.after, format = "%H:%M:%S",
              tz = "Europe/Vienna")

            candidates <- paste0(before.after, "-", possible.data$machine)
            neighbor.test <- vapply(candidates, function(x) {
              x %in% v.data$id
            }, logical(1L))

            if (any(neighbor.test)) {
              data.frame(fix = x, err = names(neighbor.test[neighbor.test]),
                stringsAsFactors = FALSE)
            }
          })

          if (!is.null(time.fix)) {
            sel <- vapply(time.fix, function(x) {
              sum(v.data$id %in% x$fix, v.data$id %in% x$err) == 3
            }, logical(1L))

            time.fix <- time.fix[sel]

            delete <- lapply(time.fix, function(x) {
              tmp <- v.data[v.data$id %in% unique(unlist(x)), ]
              sz <- round(log10(tmp$size))
              three.different <- length(unique(sz)) == 3
              two.different <- sum(sz == max(sz)) == 2
              max.sz <- max(round(log10(tmp$size)))
              sm.pkg <- max.sz <= 5 & max.sz > 3
              if (three.different | (two.different & sm.pkg)) {
                as.numeric(row.names(tmp[tmp$size != max(tmp$size), ]))
              }
            })

            delete <- do.call(c, delete)

            if (!is.null(delete)) {
              v.data <- v.data[row.names(v.data) %in% delete == FALSE, ]
            }
          }
        } else v.data
      }

      v.data[, c("machine", "id")] <- NULL
      v.data
    }
  })

  out
}
