#' Daily package downloads from Bioconductor.
#'
#' @param pkg A character vector.
#' @param obs Unit of observation: "month" or "year"
#' @export

bioconductor_downloads <- function(pkg = "clusterProfiler", obs = "year") {
  url <- paste0("https://bioconductor.org/packages/stats/bioc/", pkg, "/", pkg,
    "_stats.tab", collapse = "")
  pkg.data <- as.data.frame(data.table::fread(url))

  if (obs == "year") {
    dat <- lapply(unique(pkg.data$Year), function(yr) {
      pkg.data[pkg.data$Year == yr & pkg.data$Month == "all",]
    })

    dat <- do.call(rbind, dat)

  } else if (obs == "month") {
    dat <- pkg.data[pkg.data$Month != "all", ]
    dat$month <- NA

    for (i in seq_along(month.abb)) {
      if (i < 10) {
        dat[dat$Month == month.abb[i], "month"] <- paste0(0, i)
      } else {
        dat[dat$Month == month.abb[i], "month"] <- paste(i)
      }
    }

    dat$date <- as.Date(paste0(dat$Year, "-", dat$month, "-01"))
    dat <- dat[order(dat$date), ]

  } else stop('obs must be "year" or "month".')

  out <- list(pkg = pkg, obs = obs, data = dat)
  class(out) <- "bioconductor"
  out
}

#' Plot method for cran_downloads2().
#'
#' @param x object.
#' @param count Character. "download" or "ip".
#' @param smooth Logical. Add stats::lowess smoother.
#' @param ... Additional plotting parameters.
#' @export


plot.bioconductor <- function(x, count = "download", smooth = TRUE, ...) {
  if (x$obs == "year") {
    if (count == "download") {
      plot(x$data$Year, x$data$Nb_of_downloads, type = "o", xlab = "Year",
        ylab = "Downloads")
      if (smooth) {
        lines(stats::lowess(x$data$Year, x$data$Nb_of_downloads), col = "red")
      }
    } else if (count == "ip") {
      plot(x$data$Year, x$data$Nb_of_distinct_IPs, type = "o", xlab = "Year",
        ylab = "Unique IP Addresses")
      if (smooth) {
        lines(stats::lowess(x$data$Year, x$data$Nb_of_distinct_IPs),
          col = "red")
      }
    }
  } else if (x$obs == "month") {
    if (count == "download") {
      plot(x$data$date, x$data$Nb_of_downloads, type = "o", xlab = "Year",
        ylab = "Downloads")
      if (smooth) {
        lines(stats::lowess(x$data$date, x$data$Nb_of_downloads), col = "red")
      }
    } else if (count == "ip") {
      plot(x$data$date, x$data$Nb_of_distinct_IPs, type = "o", xlab = "Year",
        ylab = "Unique IP Addresses")
      if (smooth) {
        lines(stats::lowess(x$data$date, x$data$Nb_of_distinct_IPs),
          col = "red")
      }
    }
  } else stop('count must be "download" or "ip".')

  title(main = x$pkg)
}
