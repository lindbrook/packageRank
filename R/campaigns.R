#' Filter out A-Z campaigns (protoype).
#'
#' Uses run length encoding rle().
#' @param ip Numeric. Nominal IP address.
#' @param cran_log Object. Package log entries.
#' @param min.obs Numeric. Threshold number of unique packages downloaded.
#' @param output Character. "df" or "rownames".
#' @export
#' @note For use with ipFilter3().

campaigns <- function(ip, cran_log, min.obs = 5, output = "rownames") {
  tmp <- cran_log[cran_log$ip_id == ip, ]
  tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "Europe/Vienna")
  tmp <- tmp[order(tmp$t2, tmp$package), ]

  rle.data <- runLengthEncoding(tmp)
  start.data <- rle.data[rle.data$letter == "a" & rle.data$lengths >= min.obs, ]
  start.row <- as.numeric(row.names(start.data))
  stop.row <- start.row + length(letters) - 1

  delete.id <- unlist(lapply(seq_along(start.row), function(i) {
    dat <- rle.data[start.row[i]:stop.row[i], ]
    letter.vec <- rle.data$letter[start.row[i]:stop.row[i]]

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

if (output == "df") {
  tmp[delete.id, ]
} else if (output == "rownames")
  row.names(tmp[delete.id, ])
}

firstLetter <- function(x, case.sensitive = FALSE) {
  if (case.sensitive) tolower(substring(x, 1, 1))
  else substring(x, 1, 1)
}

runLengthEncoding <- function(x) {
  dat <- rle(firstLetter(x$package))
  data.frame(letter = dat$values,
             lengths = dat$lengths,
             start = cumsum(c(1, dat$lengths[-length(dat$lengths)])),
             end = cumsum(dat$lengths))
}
