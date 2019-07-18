#' Package download counts and rank percentiles (cross-sectional) (prototype).
#'
#' From bioconductor
#' @param pkg Character. Vector of package name(s).
#' @param yr.mo Character. Date.
#' @param count Character. "ip" or "download".
#' @return An R data frame.
#' @import data.table
#' @export

bioconductorRank <- function(pkg = "clusterProfiler", yr.mo = "2019-06",
  count = "download") {

  pkg.url <- "https://bioconductor.org/packages/stats/bioc/bioc_pkg_stats.tab"
  pkg.stats <-  as.data.frame(mfetchLog(pkg.url))

  dat <- pkg.stats[pkg.stats$Month != "all", ]
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

  sel.data <- dat[dat$date == paste0(yr.mo, "-01"), ]

  if (count == "ip") ct <- sel.data$Nb_of_distinct_IPs
  else if (count == "download") ct <- sel.data$Nb_of_downloads

  names(ct) <- sel.data$Package

  crosstab <- sort(ct, decreasing = TRUE)

  # packages in bin
  pkg.bin <- lapply(pkg, function(nm) {
    crosstab[crosstab %in% crosstab[nm]]
  })

  # offset: ties arbitrarily broken by alphabetical order
  pkg.bin.delta <- vapply(seq_along(pkg.bin), function(i) {
    which(names(pkg.bin[[i]]) %in% pkg[i])
  }, numeric(1L))

  nominal.rank <- lapply(seq_along(pkg), function(i) {
    sum(crosstab > crosstab[pkg[i]]) + pkg.bin.delta[i]
  })

  tot.pkgs <- length(crosstab)

  pkg.percentile <- vapply(pkg, function(x) {
    round(100 * mean(crosstab < crosstab[x]), 1)
  }, numeric(1L))

  dat <- data.frame(date = yr.mo,
                    packages = pkg,
                    downloads = c(crosstab[pkg]),
                    rank = unlist(nominal.rank),
                    percentile = pkg.percentile,
                    total.downloads = sum(ct),
                    total.packages = tot.pkgs,
                    stringsAsFactors = FALSE,
                    row.names = NULL)

  out <- list(packages = pkg, date = yr.mo, package.data = dat,
    crosstab = crosstab)

  class(out) <- "bioconductor_rank"
  out
}

#' Plot method for bioconductorRank().
#' @param x An object of class "bioconductor_rank" created by \code{bioconductorRank()}.
#' @param graphics Character. "base" or "ggplot2".
#' @param log_count Logical. Logarithm of package downloads.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @import graphics ggplot2
#' @importFrom ggplot2 ggplot aes_string scale_y_log10 geom_point geom_line facet_wrap theme
#' @export

plot.bioconductor_rank <- function(x, graphics = "base", log_count = TRUE,
  ...) {

  if (is.logical(log_count) == FALSE) stop("log_count must be TRUE or FALSE.")

  crosstab <- x$crosstab + 1
  package.data <- x$package.data
  packages <- x$packages
  date <- paste0(x$date, "-01")
  y.max <- crosstab[1]
  q <- stats::quantile(crosstab)[2:4]

  iqr <- vapply(c("75%", "50%", "25%"), function(id) {
    dat <- which(crosstab > q[[id]])
    dat[length(dat)]
  }, numeric(1L))

  if (is.null(graphics)) {
    if (length(packages) == 1) {
      basePlot(packages, log_count, crosstab, iqr, package.data, y.max, date)
    } else if (length(packages) > 1) {
      ggPlot(x, log_count, crosstab, iqr, package.data, y.max, date)
    } else stop("Error.")
  } else if (graphics == "base") {
    if (length(packages) > 1) {
      invisible(lapply(packages, function(pkg) {
        basePlot(pkg, log_count, crosstab, iqr, package.data, y.max, date)
      }))
    } else {
      basePlot(packages, log_count, crosstab, iqr, package.data, y.max, date)
    }
  } else if (graphics == "ggplot2") {
    ggPlot(x, log_count, crosstab, iqr, package.data, y.max, date)
  } else stop('graphics must be "base" or "ggplot2"')
}


#' Print method for bioconductorRank().
#' @param x An object of class "bioconductor_rank" created by \code{bioconductorRank()}
#' @param ... Additional parameters.
#' @export

print.bioconductor_rank <- function(x, ...) {
  dat <- x$package.data
  rank <- paste(format(dat$rank, big.mark = ","), "of",
                format(dat$total.packages, big.mark = ","))
  out <- data.frame(dat[, c("date", "packages", "downloads", "percentile")],
    rank, stringsAsFactors = FALSE, row.names = NULL)
  print(out)
}

#' Summary method for bioconductorRank().
#' @param object Object. An object of class "bioconductor_rank" created by \code{bioconductorRank()}
#' @param ... Additional parameters.
#' @export
#' @note This is useful for directly accessing the data frame.

summary.bioconductor_rank <- function(object, ...) {
  object$package.data
}
