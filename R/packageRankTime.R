#' Package download counts and rank percentiles (longitudinal).
#'
#' Temporal pattern over last week or month.
#' @param packages Character. Character. Vector of package name(s).
#' @param when Character. "last-month" or "last-week".
#' @param sample.pct Numeric.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @import cranlogs
#' @export
#' @note Most useful with plot() method. packageRankTime() takes longer to run because it replicates cranlogs::cran_downloads(when = "last-week" or "last-month") with additional computation for ranks and cohort.
#' @examples
#' \donttest{
#'
#' packageRankTime(packages = "HistData", when = "last-week")
#' packageRankTime(packages = c("Rcpp", "rlang"), when = "last-month")
#' }

packageRankTime <- function(packages = "HistData", when = "last-month",
  sample.pct = 5, multi.core = FALSE) {

  if (when %in% c("last-month", "last-week") == FALSE) {
    stop('when must either be "last-month" or "last-week".')
  }

  cores <- multiCore(multi.core)

  pkg.data <- cranlogs::cran_downloads(packages = packages, when = when)
  start.date <- pkg.data$date[1]
  end.date <- pkg.data$date[nrow(pkg.data)]

  top10 <- cranlogs::cran_top_downloads(when = when)
  top10.max <- lapply(1:5, function(i) {
    cranlogs::cran_downloads(packages = top10$package[i], when = when)$count
  })

  year <- as.POSIXlt(start.date)$year + 1900
  rstudio.url <- "http://cran-logs.rstudio.com/"

  url <- paste0(rstudio.url, year, '/', start.date, ".csv.gz")
  cran_log <- mfetchLog(url)
  init.pkgs <- unique(cran_log$package)
  init.pkgs <- stats::na.omit(init.pkgs)

  pkgs <- cran_log[cran_log$package %in% init.pkgs, ]
  crosstab <- table(pkgs$package)

  rank.percentile <- parallel::mclapply(names(crosstab), function(nm) {
    mean(crosstab < crosstab[nm])
  }, mc.cores = cores)

  rank.percentile <- unlist(rank.percentile)

  pct <- data.frame(pkg = names(crosstab), percentile = rank.percentile,
    stringsAsFactors = FALSE)
  pct <- pct[order(pct$percentile, decreasing = TRUE), ]
  row.names(pct) <- NULL

  # bins #

  breaks <- seq(1, 0, -0.05)

  bin.id <- lapply(2:length(breaks), function(i) {
    which(pct$percentile > breaks[i] & pct$percentile <= breaks[i - 1])
  })

  # set seed for random sampling
  set.seed(as.numeric(Sys.Date()))
  sample.id <- lapply(seq_along(bin.id), function(i) {
     sample(bin.id[[i]], round(sample.pct / 100 * length(bin.id[[i]])))
  })

  names(sample.id) <- paste(round(breaks[-1], 2))

  cohort <- pct[unlist(sample.id), "pkg"]
  out <- list(data = cranlogs::cran_downloads(cohort, when = when),
              pkg.data = pkg.data,
              packages = packages,
              when = when,
              y.max =  max(unlist(top10.max)))

  class(out) <- "package_rank_time"
  out
}

#' Plot method for timeSeriesRank().
#' @param x Object. An object of class "time_series" created by \code{packageRankTime()}.
#' @param graphics_pkg Character. "base" or "ggplot2".
#' @param log_count Logical. Logarithm of package downloads.
#' @param pkg_smooth Logical. Add smoother.
#' @param sample_smooth Logical. lowess background.
#' @param f Numeric. stats::lowess() smoother window. For use with graphics_pkg = "base" only.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @import graphics ggplot2
#' @importFrom ggplot2 ggplot aes_string scale_y_log10 geom_point geom_line facet_wrap theme
#' @export
#' @examples
#' \donttest{
#'
#' plot(packageRankTime(packages = "HistData", when = "last-week"))
#' plot(packageRankTime(packages = c("Rcpp", "rlang", "data.table"), when = "last-month"))
#' }

plot.package_rank_time <- function(x, graphics_pkg = "ggplot2",
  log_count = TRUE, pkg_smooth = TRUE, sample_smooth = TRUE, f = 1/3, ...) {

  if (is.logical(log_count) == FALSE) stop("log_count must be TRUE or FALSE.")
  if (is.logical(pkg_smooth) == FALSE) stop("pkg_smooth must be TRUE or FALSE.")
  if (is.logical(sample_smooth) == FALSE) {
    stop("sample_smooth must be TRUE or FALSE.")
  }
  if (is.numeric(f) == FALSE) stop("f must be numeric.")

  cran_smpl <- x$data
  pkg.data <- x$pkg.data
  packages <- x$packages

  if (log_count) {
    if (any(cran_smpl$count == 0)) cran_smpl$count <- cran_smpl$count + 1
    if (any(pkg.data$count == 0)) pkg.data$count <- pkg.data$count + 1
  }

  if (graphics_pkg == "base") {
    if (length(packages) > 1) {
      invisible(lapply(packages, function(pkg) {
        pkg.data.sel <- pkg.data[pkg.data$packages == pkg, ]
        basePlotTime(x, log_count, cran_smpl, pkg.data.sel, sample_smooth, f)
        title(main = pkg)
      }))
    } else if (length(packages) == 1) {
      basePlotTime(x, log_count, cran_smpl, pkg.data, sample_smooth, f)
      title(main = packages)
    }

  } else if (graphics_pkg == "ggplot2") {
    p <- ggplot(data = pkg.data, aes_string("date", "count")) +
           theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank()) +
           facet_wrap(~ package, ncol = 2)

    cran_smpl.lst <- rep(list(cran_smpl), length(packages))

    for (i in seq_along(packages)) {
      cran_smpl.data <- cran_smpl.lst[[i]]
      for (pkg in unique(cran_smpl.data$package)) {
        sel <- cran_smpl.data$package == pkg
        if (sample_smooth) {
          p <- p + geom_smooth(data = cran_smpl.data[sel, c("date", "count")],
                               method = "loess",
                               se = FALSE,
                               size = 0.25,
                               colour = "lightgray")
        } else {
          p <- p + geom_line(data = cran_smpl[sel, c("date", "count")],
                             colour = "lightgray")
        }
      }
    }

    p <- p + geom_line(colour = "red", size = 0.75) +
             geom_point(shape = 1, colour = "red", size = 2)

    if (pkg_smooth) p <- p + geom_smooth(colour = "blue",
                                         method = "loess",
                                         se = FALSE)

    if (log_count) p + scale_y_log10() else p

  } else stop('graphics_pkg must be "base" or "ggplot2"')
}

#' Print method for timeSeriesRank().
#' @param x An object of class "time_series" created by \code{packageRankTime()}.
#' @param ... Additional parameters.
#' @export

print.package_rank_time <- function(x, ...) {
  print(x[c("packages", "when")])
}

#' Summary method for timeSeriesRank().
#' @param object Object. An object of class "time_series" created by \code{packageRankTime()}.
#' @param ... Additional parameters.
#' @export
#' @note This is useful for directly accessing the data frame.

summary.package_rank_time <- function(object, ...) {
  object$pkg.data
}

#' Base R Graphics Plot (Longitudinal).
#' @param x Object.
#' @param log_count Logical. Logarithm of package downloads.
#' @param cran_smpl Object.
#' @param pkg.data Object.
#' @param sample_smooth Logical. lowess background.
#' @param f Numeric. stats::lowess() smoother window.
#' @noRd

basePlotTime <- function(x, log_count, cran_smpl, pkg.data, sample_smooth,
  f) {

  if (log_count) {
    plot(cran_smpl$date, log10(cran_smpl$count), pch = NA,
      ylim = c(0, max(log10(x$y.max))), xlab = "Date", ylab = "log10(Count)")

    if (sample_smooth) {
      for (nm in unique(cran_smpl$package)) {
        lines(stats::lowess(cran_smpl[cran_smpl$package == nm, "date"],
              log10(cran_smpl[cran_smpl$package == nm, "count"]), f = f),
              col = "lightgray")
      }
    } else {
      for (nm in unique(cran_smpl$package)) {
        lines(cran_smpl[cran_smpl$package == nm, "date"],
              log10(cran_smpl[cran_smpl$package == nm, "count"]),
              col = "lightgray")
      }
    }

    lines(pkg.data$date, log10(pkg.data$count), lwd = 2, col = "red",
      type = "o")
    lines(stats::lowess(pkg.data$date, log10(pkg.data$count), f = f),
      col = "blue", lwd = 2)

  } else {
    plot(cran_smpl$date, cran_smpl$count, pch = NA, ylim = c(0, max(x$y.max)),
      xlab = "Date", ylab = "Count")

    if (sample_smooth) {
      for (nm in unique(cran_smpl$package)) {
        lines(stats::lowess(cran_smpl[cran_smpl$package == nm, "date"],
              cran_smpl[cran_smpl$package == nm, "count"], f = f),
              col = "lightgray")
      }
    } else {
      for (nm in unique(cran_smpl$package)) {
        lines(cran_smpl[cran_smpl$package == nm, "date"],
              cran_smpl[cran_smpl$package == nm, "count"],
              col = "lightgray")
      }
    }

    lines(pkg.data$date, pkg.data$count, lwd = 2, col = "red")
    lines(stats::lowess(pkg.data$date, pkg.data$count, f = f), col = "blue",
      lwd = 2)
  }
}
