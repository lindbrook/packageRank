#' Filter Out A-Z Campaigns from IPs with many unique package downloads.
#'
#' Uses run length encoding, rle(), and k-means clustering, stats::kmeans().
#' @param cran_log Object. Package log entries.
#' @param campaigns Logical. Filter A-Z campaigns when checking IPs with high unique package download counts.
#' @param rle.depth Numeric. Ceiling for number of rows of run length encoding. Fewer rows means longer runs: more compact, high count run length encoding sign of campaign.
#' @param case.sensitive Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @noRd

ipFilter <- function(cran_log, campaigns = TRUE, rle.depth = 100,
  case.sensitive = FALSE, multi.core = FALSE) {

  cores <- multiCore(multi.core)
  if (.Platform$OS.type == "windows" & cores > 1) cores <- 1L

  greedy.ips <- greedyIP(cran_log)

  if (campaigns) {
    rle.data <- parallel::mclapply(greedy.ips$package.ip, function(ip) {
      tmp <- cran_log[cran_log$ip_id == ip, ]
      tmp$date.time <- dateTime(tmp$date, tmp$time)
      tmp <- tmp[order(tmp$date.time, tmp$package), ]
      list(rle = runLengthEncoding(tmp, case.sensitive = case.sensitive),
           candidate.data = tmp)
    }, mc.cores = cores)
    
    rle.ct <- vapply(rle.data, function(x) nrow(x$rle), integer(1L))
    candidate.ids <- which(rle.ct <= rle.depth)

    # check for campaigns #

    campaign.row.delete <- lapply(candidate.ids, function(i) {
      tmp <- rle.data[[i]]$rle
      A <- tmp[tmp$letter == "a" & tmp$lengths >= 5, ]
      start <- as.numeric(row.names(A))
      end <- as.numeric(row.names(A)) + length(letters) - 1
      data.select <- do.call(rbind, lapply(seq_along(start), function(i) {
        audit.data <- tmp[start[i]:end[i], ]
        if (all(!is.na(audit.data))) {
          data.frame(ip = greedy.ips$package.ip[i],
                     start = audit.data[1, "start"],
                     end = audit.data[nrow(audit.data), "end"])
        }
      }))
      c.data <- rle.data[[i]]$candidate.data
      if (!is.null(data.select)) {
        unlist(lapply(seq_len(nrow(data.select)), function(i) {
          row.names(c.data[data.select[i, "start"]:data.select[i, "end"], ])
        }))
      } else NULL
    })
    rows.delete <- unlist(campaign.row.delete)
  } else {
    sel <- cran_log$ip_id %in% unlist(greedy.ips)
    rows.delete <- row.names(cran_log[sel, ])
  }
  cran_log[!row.names(cran_log) %in% rows.delete, ]
}

#' Identify IP's that are mirroring CRAN (via k-means clutering).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param cran_log Object. cran log.
#' @param centers Numeric. Number of k's for k-means clustering.
#' @param nstart Numeric. Number of random sets.
#' @noRd

greedyIP <- function(cran_log, centers = 2L, nstart = 25L) {
  pkgs <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })
  
  pkgs <- data.frame(ip = as.integer(names(pkgs)), packages = pkgs,
    row.names = NULL)

  p.classified <- kmeanClassifier("packages", pkgs, centers, nstart)
  p.class.id <- tapply(p.classified$packages, p.classified$group, mean)
  p.data <- p.classified[p.classified$group == which.max(p.class.id), ]
  # p.ip <- ip.dwnld.ratio[ip.dwnld.ratio$packages %in% p.data$packages, "ip"]
  list(package.ip = p.data$ip)
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

kmeanClassifier <- function(var = "packages", ip.dwnld.ratio, centers = centers,
  nstart = nstart, tol = 0.001) {

  # methodological convenience for stats::kmeans()
  dat <- ip.dwnld.ratio[!duplicated(ip.dwnld.ratio[, var]), ]

  # outlier test
  orders.magnitude <- trunc(log10(dat[, var]))

  # remove extreme outlier
  extreme.outlier <- mean(orders.magnitude == max(orders.magnitude)) <= tol

  if (extreme.outlier) {
    dat.extreme <- dat[orders.magnitude == max(orders.magnitude), ]
    dat <- dat[orders.magnitude != max(orders.magnitude), ]
  }

  km <- stats::kmeans(stats::dist(dat[, var]), centers = centers,
    nstart = nstart)

  tmp <- data.frame(dat[, var], group = km$cluster)
  names(tmp)[1] <- var
  out <- merge(ip.dwnld.ratio, tmp, by = var)

  if (extreme.outlier) {
    max.grp.id <- which.max(tapply(out[, var], out$group, mean))
    extreme.tmp <- data.frame(dat.extreme, group = max.grp.id)
    out <- rbind(extreme.tmp, out)
    row.names(out) <- NULL
  }
  out
}

#' Run Length Encoding of First Letter of Packages Downloaded.
#'
#' Uses rle().
#' @param ip Numeric. Nominal IP address.
#' @param cran_log Object. Package log entries.
#' @param case.sensitive Logical.
#' @param concatenate Logical.
#' @noRd
#' @examples
#' \dontrun{
#' campaignRLE(ip = 24851, cran_log = july01)
#' }

campaignRLE <- function(ip, cran_log, case.sensitive = FALSE,
  concatenate = TRUE) {
  cran_log <- cleanLog(cran_log)
  cran_log <- cran_log[cran_log$ip_id == ip, ]
  cran_log$date.time <- dateTime(cran_log$date, cran_log$time)
  cran_log <- cran_log[order(cran_log$date.time, cran_log$package), ]
  rle.data <- runLengthEncoding(cran_log, case.sensitive = case.sensitive)
  if (concatenate) paste(rle.data$letter, collapse = "")
  else rle.data$letter
}
