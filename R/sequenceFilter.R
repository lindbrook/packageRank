#' Filter downloads of full-sized sequential versions (prototype).
#'
#' @param dat Object.
#' @param max.delta.time Numeric. Maximum time interval between downloads (seconds).
#' @export

sequenceFilter <- function(dat, max.delta.time = 30) {
  t0 <- strptime(paste(dat$date, dat$time), "%Y-%m-%d %T", tz = "Europe/Vienna")
  dat <- dat[order(t0), ]
  rle.data <- rle(dat$ver)
  rle.out <- data.frame(lengths = rle.data$lengths, values = rle.data$values)

  candidates <- rle(rle.out$lengths == 1)
  candidates <- data.frame(lengths = candidates$lengths,
    values = candidates$values)
  candidates$id <- cumsum(candidates$lengths)

  c.id <- lapply(seq_len(nrow(candidates)), function(i) {
    if (i == 1) {
      if (candidates[i, "values"] == TRUE) {
        seq(from = 1, to = candidates[i, "id"], by = 1)
      }
    } else {
      if (candidates[i, "values"] == TRUE) {
        from <- candidates[i, "id"] - candidates[i, "lengths"] + 1
        seq(from = from, to = candidates[i, "id"], by = 1)
      }
    }
  })

  candidates.sel <- c.id[vapply(c.id, function(x) !is.null(x), logical(1L))]

  if (length(unlist(candidates.sel)) > 1) {
    rows.delete <- unlist(lapply(candidates.sel, function(i) {
      sel <-  cumsum(rle.out$lengths)[i]
      tmp <- dat[sel, ]
      tmp <- tmp[!duplicated(tmp$version), ]
      t2 <- strptime(paste(tmp$date, tmp$time), "%Y-%m-%d %T",
        tz = "Europe/Vienna")

      delta <- vapply(seq_along(t2[-1]), function(i) {
        difftime(t2[i + 1], t2[i], units = "secs")
      }, numeric(1L))

      if (any(delta < max.delta.time)) {
        delta.obs <- which(delta < max.delta.time)
        delta.exp <- seq(delta.obs[1], length(delta.obs) + delta.obs[1] - 1)
        seq.test <- identical(delta.obs, delta.exp)

        if (seq.test) {
          tmp.sel <- c(delta.obs, delta.obs[length(delta.obs)] + 1)
          tmp <- tmp[tmp.sel, ]
          t2.range <- range(t2[tmp.sel])
          max.time.window <- max.delta.time * length(t2[tmp.sel])
          obs.time.window <- difftime(t2.range[2], t2.range[1], units = "sec")
          if (obs.time.window < max.time.window) {
            row.names(dat[row.names(dat) %in% row.names(tmp), ])
          }
        } else NULL
      } else NULL
    }))

    dat[!row.names(dat) %in% rows.delete, ]
  } else dat
}
