#' Filter Out A-Z Campaigns (protoype).
#'
#' Uses run length encoding rle().
#' @param ip Numeric. Nominal IP address (ip_id).
#' @param cran_log Object. Package log entries.
#' @param case.sensitive Logical.
#' @param min.obs Numeric. Threshold number of downloads.
#' @param output Character. "df" or "rownames".
#' @export
#' @note For use with ipFilter3().

campaigns <- function(ip, cran_log, case.sensitive = FALSE, min.obs = 5,
  output = "rownames") {

  tmp <- cleanLog(cran_log[cran_log$ip_id == ip, ])
  tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "Europe/Vienna")
  tmp <- tmp[order(tmp$t2, tmp$package), ]

  rle.data <- runLengthEncoding(tmp, case.sensitive = case.sensitive)

  if (case.sensitive) {
    start.data <- rle.data[(rle.data$letter == "A" | rle.data$letter == "a") &
                            rle.data$lengths >= min.obs, ]
  } else {
    start.data <- rle.data[rle.data$letter == "a" &
                           rle.data$lengths >= min.obs, ]
  }

  start.row <- as.numeric(row.names(start.data))
  stop.row <- start.row + length(letters) - 1

  delete.id <- unlist(lapply(seq_along(start.row), function(i) {
    dat <- rle.data[start.row[i]:stop.row[i], ]
    letter.vec <- rle.data$letter[start.row[i]:stop.row[i]]

    idx <- seq_along(letter.vec)[-length(letter.vec)]
    order.test <- vapply(idx, function(i) {
      letter.vec[i] < letter.vec[i + 1]
    }, logical(1L))

    if (any(!order.test)) {
      out <- dat[1:which(!order.test)[1], ]
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

#' Run Length Encoding of First Letter of Packages Downloaded.
#'
#' Uses rle().
#' @param ip Numeric. Nominal IP address.
#' @param cran_log Object. Package log entries.
#' @param case.sensitive Logical.
#' @export
#' @examples
#' \dontrun{
#' campaignRLE(ip = 24851, cran_log = july01)
#' }

campaignRLE <- function(ip, cran_log, case.sensitive = FALSE) {
  cran_log <- cleanLog(cran_log)
  cran_log <- cran_log[cran_log$ip_id == ip, ]
  cran_log$t2 <- as.POSIXlt(paste(cran_log$date, cran_log$time),
    tz = "Europe/Vienna")
  cran_log <- cran_log[order(cran_log$t2, cran_log$package), ]
  rle.data <- runLengthEncoding(cran_log, case.sensitive = case.sensitive)
  paste(rle.data$letter, collapse = "")
}

#' Filter Out A-Z Campaigns (protoype).
#'
#' Uses run length encoding rle().
#' @param cran_log Object. Package log entries.
#' @param no.of.ips Numeric. Number of IP addresses to check.
#' @param rle.depth s Numeric. Ceiling for number of rows of run length encoding.
#' @param case.sensitive Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

campaigns2 <- function(cran_log, no.of.ips = 50, rle.depth = 100,
  case.sensitive = FALSE, multi.core = TRUE) {

  cores <- multiCore(multi.core)
  cran_log <- smallFilter0(cran_log)
  ip.df <- ipFilter3(cran_log, "df")

  rle.data <- parallel::mclapply(ip.df$ip[1:no.of.ips], function(ip) {
    tmp <- cran_log[cran_log$ip_id == ip, ]
    tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "Europe/Vienna")
    tmp <- tmp[order(tmp$t2, tmp$package), ]
    rle.data <- runLengthEncoding(tmp, case.sensitive = case.sensitive)
    data.frame(ip = ip, nrow = nrow(rle.data))
  }, mc.cores = cores)

  rle.data <- do.call(rbind, rle.data)
  ips <- as.numeric(rle.data[rle.data$nrow <= rle.depth, "ip"])

  rows.delete <- parallel::mclapply(ips, function(ip) {
    tmp <- cran_log[cran_log$ip_id == ip, ]
    tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "Europe/Vienna")
    tmp <- tmp[order(tmp$t2, tmp$package), ]
    rle.data <- runLengthEncoding(tmp, case.sensitive = case.sensitive)

    A <- rle.data[rle.data$letter == "a" & rle.data$lengths >= 10, ]

    start <- as.numeric(row.names(A))
    end <- as.numeric(row.names(A)) + length(letters) - 1

    data.select <- lapply(seq_along(start), function(i) {
      audit.data <- rle.data[start[i]:end[i], ]
      if (all(!is.na(audit.data))) {
        data.frame(ip = ip, start = audit.data[1, "start"],
          end = audit.data[nrow(audit.data), "end"])
      }
    })

    data.select <- do.call(rbind, data.select)

    if (!is.null(data.select)) {
      unlist(lapply(seq_len(nrow(data.select)), function(i) {
        row.names(tmp[data.select[i, "start"]:data.select[i, "end"], ])
      }))
    } else NULL
  }, mc.cores = cores)
  unlist(rows.delete)
}

firstLetter <- function(x, case.sensitive = FALSE) {
  if (case.sensitive) substring(x, 1, 1)
  else tolower(substring(x, 1, 1))
}

runLengthEncoding <- function(x, case.sensitive = FALSE) {
  dat <- rle(firstLetter(x$package, case.sensitive = case.sensitive))
  data.frame(letter = dat$values,
             lengths = dat$lengths,
             start = cumsum(c(1, dat$lengths[-length(dat$lengths)])),
             end = cumsum(dat$lengths))
}
