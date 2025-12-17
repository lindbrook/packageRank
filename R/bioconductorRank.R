#' Package download counts and rank percentiles.
#'
#' From bioconductor
#' @param package Character. Vector of package name(s).
#' @param date Character. Date. yyyy-mm
#' @param count Character. "ip" or "download".
#' @return An R data frame.
#' @export
#' @examples
#' \dontrun{
#' bioconductorRank(package = "cicero", date = "2019-09")
#' }

bioconductorRank <- function(package = "monocle", date = "2019-01",
  count = "download") {
  
  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  
  pkg.url <- "https://bioconductor.org/packages/stats/bioc/bioc_pkg_stats.tab"
  package.stats <-  as.data.frame(mfetchLog(pkg.url))
  
  dat <- package.stats[package.stats$Month != "all", ]
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
  
  sel.data <- dat[dat$date == as.Date(paste0(date, "-01")), ]
  
  if (count == "ip") {
    ct <- sel.data$Nb_of_distinct_IPs
  } else if (count == "download") {
    ct <- sel.data$Nb_of_downloads
  }
  
  names(ct) <- sel.data$Package
  
  freqtab <- sort(ct, decreasing = TRUE)
  
  # packages in bin
  package.bin <- lapply(package, function(nm) {
    freqtab[freqtab %in% freqtab[nm]]
  })
  
  # offset: ties arbitrarily broken by alphabetical order
  package.bin.delta <- vapply(seq_along(package.bin), function(i) {
    which(names(package.bin[[i]]) %in% package[i])
  }, numeric(1L))
  
  nominal.rank <- lapply(seq_along(package), function(i) {
    sum(freqtab > freqtab[package[i]]) + package.bin.delta[i]
  })
  
  tot.package <- length(freqtab)
  
  package.percentile <- vapply(package, function(x) {
    round(100 * mean(freqtab < freqtab[x]), 1)
  }, numeric(1L))
  
  dat <- data.frame(date = date,
                    package = package,
                    downloads = c(freqtab[package]),
                    rank = unlist(nominal.rank),
                    percentile = package.percentile,
                    total.downloads = sum(ct),
                    total.package = tot.package,
                    stringsAsFactors = FALSE,
                    row.names = NULL)
  
  out <- list(package = package, date = date, package.data = dat,
    freqtab = freqtab)
  class(out) <- "bioconductorRank"
  out
}

#' Plot method for bioconductorRank().
#' @param x An object of class "bioconductor_rank" created by \code{bioconductorRank()}.
#' @param graphics Character. "base" or "ggplot2".
#' @param log.y Logical. Logarithm of package downloads.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @export


plot.bioconductorRank <- function(x, graphics = NULL, log.y = TRUE, ...) {
  if (is.logical(log.y) == FALSE) stop("log.y must be TRUE or FALSE.")
  freqtab <- x$freqtab + 1
  package.data <- x$package.data
  package <- x$package
  date <- x$date
  y.max <- freqtab[1]
  q <- stats::quantile(freqtab)[2:4]
  
  iqr <- vapply(c("75%", "50%", "25%"), function(id) {
    dat <- which(freqtab > q[[id]])
    dat[length(dat)]
  }, numeric(1L))
  
  if (is.null(graphics)) {
    if (length(package) == 1) {
      basePlot(package, log.y, freqtab, iqr, package.data, y.max, date)
    } else if (length(package) > 1) {
      ggPlot(x, log.y, freqtab, iqr, package.data, y.max, date)
    } else stop("Error.")
  } else if (graphics == "base") {
    if (length(package) > 1) {
      invisible(lapply(package, function(pkg) {
        basePlot(pkg, log.y, freqtab, iqr, package.data, y.max, date)
      }))
    } else {
      basePlot(package, log.y, freqtab, iqr, package.data, y.max, date)
    }
  } else if (graphics == "ggplot2") {
    ggPlot(x, log.y, freqtab, iqr, package.data, y.max, date)
  } else stop('graphics must be "base" or "ggplot2"')
}

#' Print method for bioconductorRank().
#' @param x An object of class "bioconductor_rank" created by \code{bioconductorRank()}
#' @param ... Additional parameters.
#' @export

print.bioconductorRank <- function(x, ...) {
  dat <- x$package.data
  rank <- paste(format(dat$rank, big.mark = ","), "of",
                format(dat$total.package, big.mark = ","))
  out <- data.frame(dat[, c("date", "package", "downloads")], rank,
    percentile = dat[, "percentile"], stringsAsFactors = FALSE,
    row.names = NULL)
  print(out)
}

#' Summary method for bioconductorRank().
#' @param object Object. An object of class "bioconductor_rank" created by \code{bioconductorRank()}
#' @param ... Additional parameters.
#' @export
#' @note This is useful for directly accessing the data frame.

summary.bioconductorRank <- function(object, ...) {
  object$package.data
}
