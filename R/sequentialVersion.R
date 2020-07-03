#' Download of sequential versions
#'
#' @param package Character. Vector of package name(s).
#' @param date Character. Date. "yyyy-mm-dd".
#' @param time.frame Numeric. seconds.
#' @param one.country Logical.
#' @param one.ip Logical.
#' @export

sequentialVersion <- function(package = "cholera", date = Sys.Date() - 1,
  time.frame = 60, one.country = FALSE, one.ip = FALSE) {

  dat <- packageLog(packages = package, date = date)
  ver.tab <- table(dat$ver)
  dat$ver[dat$ver != names(which.max(ver.tab))]
  rle.data <- rle(dat$ver[dat$ver != names(which.max(ver.tab))])
  rle.data <- rle(dat$ver)
  out <- data.frame(lengths = rle.data$lengths, values = rle.data$values)

  candidates <- rle(out$lengths == 1)
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
        from <-  candidates[i, "id"] - candidates[i, "lengths"] + 1
        seq(from = from, to = candidates[i, "id"], by = 1)
      }
    }
  })

  c.id <- c.id[vapply(c.id, function(x) !is.null(x), logical(1L))]

  sel <- vapply(c.id, function(id) {
    if (length(id) > 1) {
      vers <- sort(unique(out[id, "values"]))
      identical(sort(out[id, "values"]), vers)
    } else FALSE
  }, logical(1L))

  out <- lapply(c.id[sel], function(x) {
    seq.data <- dat[x, ]
    seq.data$stamp <- strptime(paste(seq.data$date, seq.data$time),
      "%Y-%m-%d %T", tz = "GMT")

    idx <- as.data.frame(t(utils::combn(length(seq.data$stamp), 2)))
    delta <- difftime(seq.data$stamp[idx$V2], seq.data$stamp[idx$V1],
      units = "secs")

    sel <- unique(unlist(idx[delta < time.frame, ]))

    one.country <- length(unique(seq.data$country)) == 1
    one.ip <- length(unique(seq.data$ip_id)) == 1
    same.time.frame <- length(sel) > 0

    if (same.time.frame & !one.country & !one.ip) {
      row.names(seq.data[sel, ])
    } else if (same.time.frame & one.country & !one.ip) {
      row.names(seq.data[sel, ])
    } else if (same.time.frame & !one.country & one.ip) {
      row.names(seq.data[sel, ])
    } else if (same.time.frame & one.country & one.ip) {
      row.names(seq.data[sel, ])
    } else NULL
  })

  unlist(out)
}
