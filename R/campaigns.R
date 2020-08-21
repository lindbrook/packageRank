#' Filter out A-Z campaigns (protoype).
#'
#' Uses run length encoding rle().
#' @param tmp Object. Package log entries.
#' @param min.obs Numeric. Threshold number of unique packages downloaded.
#' @export
#' @note For use with ipFilter3().

campaigns <- function(tmp, min.obs = 5) {
  tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "Europe/Vienna")
  tmp <- tmp[order(tmp$t2, tmp$package), ]

  rle.all <- runLengthEncoding(tmp)
  start.data <- rle.all[rle.all$letter == "a" & rle.all$lengths >= min.obs, ]
  start.row <- as.numeric(row.names(start.data))
  stop.row <- start.row + length(letters) - 1

  delete.id <- unlist(lapply(seq_along(start.row), function(i) {
    dat <- rle.all[start.row[i]:stop.row[i], ]
    letter.vec <- rle.all$letter[start.row[i]:stop.row[i]]

    if (any(is.na(dat$letter))) {
      dat <- dat[!is.na(dat$letter), ]
      letter.vec <- letter.vec[!is.na(letter.vec)]
    }

    idx <- seq_along(letter.vec)[-length(letter.vec)]
    order.test <- vapply(idx, function(i) {
      letter.vec[i] < letter.vec[i + 1]
    }, logical(1L))

    if (any(!order.test)) {
      out <- dat[1:which(!order.test)[1] - 1, ]
    } else {
      out <- dat
    }

    unlist(lapply(seq_len(nrow(out)), function(i) {
      out[i, "start"]:out[i, "end"]
    }))
  }))

  tmp[-delete.id, ]
}

firstLetter <- function(x) tolower(substring(x, 1, 1))

runLengthEncoding <- function(x) {
  dat <- rle(firstLetter(x$package))
  data.frame(letter = dat$values,
             lengths = dat$lengths,
             start = cumsum(c(1, dat$lengths[-length(dat$lengths)])),
             end = cumsum(dat$lengths))
}

