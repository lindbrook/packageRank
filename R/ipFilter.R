#' Filter Out A-Z Campaigns from IPs with many unique package downloads.
#'
#' Uses run length encoding, rle(), and k-means clustering, stats::kmeans().
#' @param cran_log Object. Package log entries.
#' @param campaigns Logical. Filter A-Z campaigns when checking IPs with high unique package download counts.
#' @param rle.depth s Numeric. Ceiling for number of rows of run length encoding. Fewer rows means longer runs.
#' @param case.sensitive Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export

ipFilter <- function(cran_log, campaigns = TRUE, rle.depth = 100,
  case.sensitive = FALSE, multi.core = TRUE, dev.mode = dev.mode) {

  cores <- multiCore(multi.core)
  # win.exception <- .Platform$OS.type == "windows" & cores > 1

  greedy.ips <- ip_filter(cran_log)

  if (campaigns) {
    # if (dev.mode | win.exception) {
    if (dev.mode) {
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl = cl, envir = environment(),
        varlist = c("greedy.ips", "cran_log"))
      candidate.data <- parallel::parLapply(cl, greedy.ips$package.ip,
        function(ip) {
          tmp <- cran_log[cran_log$ip_id == ip, ]
          tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "GMT")
          tmp[order(tmp$t2, tmp$package), ]
      })
      parallel::stopCluster(cl)

    } else {
      if (.Platform$OS.type == "windows") cores <- 1L
      candidate.data <- parallel::mclapply(greedy.ips$package.ip, function(ip) {
        tmp <- cran_log[cran_log$ip_id == ip, ]
        tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "GMT")
        tmp[order(tmp$t2, tmp$package), ]
      }, mc.cores = cores)
    }

    rle.data <- lapply(candidate.data, function(x) {
      runLengthEncoding(x, case.sensitive = case.sensitive)
    })

    rle.ct <- vapply(rle.data, nrow, integer(1L))
    candidate.ids <- which(rle.ct <= rle.depth)

    # check for campaigns #

    # if (dev.mode | win.exception) {
    if (dev.mode) {
      cl <- parallel::makeCluster(cores)

      parallel::clusterExport(cl = cl, envir = environment(),
        varlist = c("candidate.ids", "rle.data", "candidate.data"))

      campaign.row.delete <- parallel::parLapply(cl, candidate.ids,
        function(x) {
        tmp <- rle.data[[x]]
        A <- tmp[tmp$letter == "a" & tmp$lengths >= 10, ]
        start <- as.numeric(row.names(A))
        end <- as.numeric(row.names(A)) + length(letters) - 1
        data.select <- lapply(seq_along(start), function(i) {
          audit.data <- tmp[start[i]:end[i], ]
          if (all(!is.na(audit.data))) {
            data.frame(ip = greedy.ips$package.ip[x],
                       start = audit.data[1, "start"],
                       end = audit.data[nrow(audit.data), "end"])
          }
        })
        data.select <- do.call(rbind, data.select)
        c.data <- candidate.data[[x]]
        if (!is.null(data.select)) {
          unlist(lapply(seq_len(nrow(data.select)), function(i) {
            row.names(c.data[data.select[i, "start"]:data.select[i, "end"], ])
          }))
        } else NULL
      })

      parallel::stopCluster(cl)

    } else {
      if (.Platform$OS.type == "windows") cores <- 1L
      campaign.row.delete <- parallel::mclapply(candidate.ids, function(x) {
        tmp <- rle.data[[x]]
        A <- tmp[tmp$letter == "a" & tmp$lengths >= 10, ]
        start <- as.numeric(row.names(A))
        end <- as.numeric(row.names(A)) + length(letters) - 1
        data.select <- lapply(seq_along(start), function(i) {
          audit.data <- tmp[start[i]:end[i], ]
          if (all(!is.na(audit.data))) {
            data.frame(ip = greedy.ips$package.ip[x],
                       start = audit.data[1, "start"],
                       end = audit.data[nrow(audit.data), "end"])
          }
        })
        data.select <- do.call(rbind, data.select)
        c.data <- candidate.data[[x]]
        if (!is.null(data.select)) {
          unlist(lapply(seq_len(nrow(data.select)), function(i) {
            row.names(c.data[data.select[i, "start"]:data.select[i, "end"], ])
          }))
        } else NULL
      }, mc.cores = cores)
    }
    sel <- cran_log$ip_id %in% greedy.ips$ratio.ip
    ratio.row.delete <- row.names(cran_log[sel, ])
    rows.delete <- c(unlist(campaign.row.delete), ratio.row.delete)
  } else {
    sel <- cran_log$ip_id %in% unlist(greedy.ips)
    rows.delete <- row.names(cran_log[sel, ])
  }
  cran_log[!row.names(cran_log) %in% rows.delete, ]
}

#' Identify IP's that are mirroring CRAN (k-means helper prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param cran_log Object. cran log.
#' @param centers Numeric. Number of k's for k-means clustering.
#' @param nstart Numeric. Number of random sets.
#' @noRd

ip_filter <- function(cran_log, centers = 2L, nstart = 25L) {
  pkgs <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  pkgs <- data.frame(ip = as.integer(names(pkgs)), packages = pkgs,
    row.names = NULL)

  dwnlds <- as.data.frame(table(cran_log$ip_id), stringsAsFactors = FALSE)
  names(dwnlds) <- c("ip", "downloads")
  dwnlds$ip <- as.integer(dwnlds$ip)

  idp <- merge(dwnlds, pkgs, by = "ip")
  idp$ratio <- idp$downloads / idp$packages

  p.classified <- kmeanClassifier("packages", idp, centers, nstart)
  r.classified <- kmeanClassifier("ratio", idp, centers, nstart)

  pkg.tests <- idp[idp$packages == 1 & idp$downloads != 1, ]
  km <- stats::kmeans(stats::dist(pkg.tests$downloads), centers = centers,
    nstart = nstart)
  t.classified <- data.frame(ip = pkg.tests$ip, downloads = pkg.tests$downloads,
    group = km$cluster)

  p.class.id <- tapply(p.classified$packages, p.classified$group, mean)
  r.class.id <- tapply(r.classified$ratio, r.classified$group, mean)
  t.class.id <- tapply(t.classified$downloads, t.classified$group, mean)

  p.data <- p.classified[p.classified$group == which.max(p.class.id), ]
  r.data <- r.classified[r.classified$group == which.max(r.class.id), ]
  t.data <- t.classified[t.classified$group == which.max(t.class.id), ]

  p.ip <- idp[idp$packages %in% p.data$packages, "ip"]
  r.ip <- idp[idp$ratio %in% r.data$ratio, "ip"]

  list(package.ip = p.ip, ratio.ip = union(r.ip, t.data$ip))
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

kmeanClassifier <- function(var = "packages", idp, centers = centers,
  nstart = nstart, tol = 0.001) {

  # methodological convenience for stats::kmeans()
  dat <- idp[!duplicated(idp[, var]), ]

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
  out <- merge(idp, tmp, by = var)

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
  cran_log$t2 <- dateTime(cran_log$date, cran_log$time)
  cran_log <- cran_log[order(cran_log$t2, cran_log$package), ]
  rle.data <- runLengthEncoding(cran_log, case.sensitive = case.sensitive)
  if (concatenate) paste(rle.data$letter, collapse = "")
  else rle.data$letter
}
