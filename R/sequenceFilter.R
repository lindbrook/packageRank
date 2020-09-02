#' Filter Downloads of sequential Versions (prototype).
#'
#' @param dat Object.
#' @param timeframe Numeric. minutes
#' @param one.timeframe Logical.
#' @param one.country Logical.
#' @param one.ip Logical.
#' @export

sequenceFilter <- function(dat, timeframe = 15, one.timeframe = TRUE,
  one.country = TRUE, one.ip = TRUE) {

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

  c.id <- c.id[vapply(c.id, function(x) !is.null(x), logical(1L))]

  c.sel <- vapply(c.id, function(id) {
    if (length(id) > 1) {
      vers <- sort(unique(rle.out[id, "values"]))
      identical(sort(rle.out[id, "values"]), vers)
    } else FALSE
  }, logical(1L))

  out <- lapply(c.id[c.sel], function(x) {
    seq.data <- dat[cumsum(rle.out$lengths)[x], ]
    seq.data$t2 <- strptime(paste(seq.data$date, seq.data$time),
      "%Y-%m-%d %T", tz = "Europe/Vienna")
    # seq.data$stamp <- as.POSIXlt(paste(seq.data$date, seq.data$time),
    #   tz = "Europe/Vienna")

    delta <- difftime(max(seq.data$t2), min(seq.data$t2), units = "mins")

    if ((delta < timeframe) == TRUE) {
      time.sel <- rep(TRUE, nrow(seq.data))
    } else {
      time.sel <- seq.data$t2 >= min(seq.data$t2) &
                  seq.data$t2 <= min(seq.data$t2) + timeframe
      if (sum(time.sel) == 1) {
        time.sel <- rep(FALSE, nrow(seq.data))
      }
    }

    ip.sel <- ifelse(length(unique(seq.data$ip_id)) == 1, TRUE, FALSE)
    country.sel <- ifelse(length(unique(seq.data$country)) == 1, TRUE, FALSE)

    if (one.timeframe & !one.country & !one.ip) {
      row.names(seq.data[time.sel, ])
    } else if (one.timeframe & one.country & !one.ip) {
      if (country.sel) {
        row.names(seq.data[time.sel, ])
      }
    } else if (one.timeframe & !one.country & one.ip) {
      if (ip.sel) {
        row.names(seq.data[time.sel, ])
      }
    } else if (one.timeframe & one.country & one.ip) {
      if (country.sel & ip.sel) {
        row.names(seq.data[time.sel, ])
      }
    } else NULL
  })

  if (!is.null(unlist(out))) {
    dat[!row.names(dat) %in% unlist(out), ]
  } else dat
}
