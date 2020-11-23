#' Filter Out A-Z Campaigns from IPs with many unique package downloads.
#'
#' Uses run length encoding rle().
#' @param cran_log Object. Package log entries.

#' @param rle.depth s Numeric. Ceiling for number of rows of run length encoding. Fewer rows means longer runs
#' @param case.sensitive Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

ipFilter <- function(cran_log, rle.depth = 100, case.sensitive = FALSE,
  multi.core = TRUE) {

  cores <- multiCore(multi.core)
  cran_log <- smallFilter0(cran_log)
  greedy.ips <- ip_filter(cran_log, "ip") # IPs w/ >= 10K unique packages

  # candidate data #

  candidate.data <- parallel::mclapply(greedy.ips, function(ip) {
    tmp <- cran_log[cran_log$ip_id == ip, ]
    tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "Europe/Vienna")
    tmp[order(tmp$t2, tmp$package), ]
  }, mc.cores = cores)

  rle.data <- parallel::mclapply(candidate.data, function(x) {
    runLengthEncoding(x, case.sensitive = case.sensitive)
  }, mc.cores = cores)

  rle.ct <- vapply(rle.data, nrow, integer(1L))
  candidate.ids <- which(rle.ct <= rle.depth)

  # check for campaigns #

  rows.delete <- parallel::mclapply(seq_along(candidate.ids), function(i) {
    tmp <- rle.data[[candidate.ids[i]]]
    A <- tmp[tmp$letter == "a" & tmp$lengths >= 10, ]
    start <- as.numeric(row.names(A))
    end <- as.numeric(row.names(A)) + length(letters) - 1

    data.select <- lapply(seq_along(start), function(i) {
      audit.data <- tmp[start[i]:end[i], ]
      if (all(!is.na(audit.data))) {
        data.frame(ip = greedy.ips[candidate.ids[i]],
                   start = audit.data[1, "start"],
                   end = audit.data[nrow(audit.data), "end"])
      }
    })

    data.select <- do.call(rbind, data.select)
    c.data <- candidate.data[[candidate.ids[i]]]

    if (!is.null(data.select)) {
      unlist(lapply(seq_len(nrow(data.select)), function(i) {
        row.names(c.data[data.select[i, "start"]:data.select[i, "end"], ])
      }))
    } else NULL
  }, mc.cores = cores)

  unlist(rows.delete)
}

#' Identify IP's that are mirroring CRAN (k-means helper prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param cran_log Object. cran log.
#' @param output Character. "ip" vector of ip address; "df" data frame.
#' @param floor Numeric. If specified, the minimum number of unique packages downloaded by an IP address to use k-means clustering.
#' @param centers Numeric. Number of k's for k-means clustering.
#' @param nstart Numeric. Number of random sets.
#' @export

ip_filter <- function(cran_log, output = "ip", floor = 10000L, centers = 2L,
  nstart = 25L) {

  if (!output %in% c("df", "ip")) stop('"output" must be "ip" or "df".')

  freqtab <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  df <- data.frame(ip = names(freqtab), count = c(freqtab), row.names = NULL)

  if (is.null(floor)) {
    df <- df[!duplicated(df$count), ]
    cran.max <- nrow(utils::available.packages()) / 2
    # cran.max <- 8056L  # 2020-08-16

    if (max(freqtab) >= cran.max) {
      km <- stats::kmeans(stats::dist(df$count), centers = centers,
        nstart = nstart)
      out <- data.frame(ip = df$ip, packages = df$count, group = km$cluster)
    } else {
      out <- data.frame(ip = df$ip, packages = df$count, group = 1)
    }

    ip.country <- cran_log[!duplicated(cran_log$ip), c("ip_id", "country")]
    out <- merge(out, ip.country, by.x = "ip", by.y = "ip_id")
    out <- out[, c("ip", "country", "packages", "group")]

    if (output == "ip") {
      grp <- as.numeric(names(which.min(table(out$group))))
      out <- out[out$group == grp, ]
      out <- out[order(out$packages, decreasing = TRUE), ]
      out <- as.numeric(out$ip)
    } else if (output == "df") {
      out <- out[order(out$packages, decreasing = TRUE), ]
      row.names(out) <- NULL
    }

  } else {
    if (!is.numeric(floor)) stop('"floor" must be a number.')
    else if (floor < 0) stop('"floor" must be a positive number.')

    if (output == "ip") {
      out <- names(sort(freqtab[freqtab >= floor], decreasing = TRUE))
      out <- as.numeric(out)
    } else if (output == "df") {
      out <- data.frame(ip = names(freqtab), packages = c(freqtab),
        row.names = NULL)
      out$group <- ifelse(out$packages >= floor, 1, 2)
      ip.country <- cran_log[!duplicated(cran_log$ip), c("ip_id", "country")]
      out <- merge(out, ip.country, by.x = "ip", by.y = "ip_id")
      out <- out[, c("ip", "country", "packages", "group")]
      out <- out[order(out$packages, decreasing = TRUE), ]
      row.names(out) <- NULL
    }
  }

  out
}

runLengthEncoding <- function(x, case.sensitive = FALSE) {
  dat <- rle(firstLetter(x$package, case.sensitive = case.sensitive))
  data.frame(letter = dat$values,
             lengths = dat$lengths,
             start = cumsum(c(1, dat$lengths[-length(dat$lengths)])),
             end = cumsum(dat$lengths))
}

firstLetter <- function(x, case.sensitive = FALSE) {
  if (case.sensitive) substring(x, 1, 1)
  else tolower(substring(x, 1, 1))
}

#' Run Length Encoding of First Letter of Packages Downloaded.
#'
#' Uses rle().
#' @param ip Numeric. Nominal IP address.
#' @param cran_log Object. Package log entries.
#' @param case.sensitive Logical.
#' @param concatenate Logical.
#' @export
#' @examples
#' \dontrun{
#' campaignRLE(ip = 24851, cran_log = july01)
#' }

campaignRLE <- function(ip, cran_log, case.sensitive = FALSE,
  concatenate = TRUE) {
  cran_log <- cleanLog(cran_log)
  cran_log <- cran_log[cran_log$ip_id == ip, ]
  cran_log$t2 <- as.POSIXlt(paste(cran_log$date, cran_log$time),
    tz = "Europe/Vienna")
  cran_log <- cran_log[order(cran_log$t2, cran_log$package), ]
  rle.data <- runLengthEncoding(cran_log, case.sensitive = case.sensitive)
  if (concatenate) {
    paste(rle.data$letter, collapse = "")
  } else {
    rle.data$letter
  }
}
