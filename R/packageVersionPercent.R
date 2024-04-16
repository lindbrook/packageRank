#' Compute data for versionPlot().
#'
#' packageRank::blog.data or recompute random sample of packages.
#' @param lst Object. List of CRAN download logs data frames. Use monthlyLog().
#' @param yr.mo Character. "yyyy-mo". packageVersionsPercent(NULL, yr.mo)
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @examples
#' \dontrun{
#' # To resample and recompute, set lst to NULL, specify a yr.mo:
#' packageVersionPercent(NULL, yr.mo = "2020-07")
#'
#' Otherwise, you must provide a pre-computed lst of logs.
#' }
#' @export

packageVersionPercent <- function(lst, yr.mo = "2020-07", multi.core = FALSE) {
  cores <- multiCore(multi.core)

  if (is.null(lst)) {
    lst <- monthlyLog(yr.mo = yr.mo)
    cran.pkgs <- packageSample2(lst, multi.core = cores)
    arch.pkgs <- packageSample2(lst, "arch", multi.core = cores)
  } else {
     mo <- months(as.Date(names(lst)[1]))
    if (mo == "October") {
      cran.pkgs <- packageRank::blog.data$cran.pkgs.oct
      arch.pkgs <- packageRank::blog.data$arch.pkgs.oct
    } else if (mo == "July") {
      cran.pkgs <- packageRank::blog.data$cran.pkgs.jul
      arch.pkgs <- packageRank::blog.data$arch.pkgs.jul
    }
  }
  
  pkgs <- c(cran.pkgs, arch.pkgs)
  histories <- lapply(pkgs, packageHistory)
  names(histories) <- pkgs
  
  pkg.data <- lapply(pkgs, function(x) {
    computeVersionPercents(lst, x, histories)
  })
  names(pkg.data) <- pkgs
  
  out <- list(cran.pkgs = cran.pkgs, arch.pkgs = arch.pkgs, pkg.data = pkg.data)
  class(out) <- "packageVersionPercent"
  out
}

#' Get CRAN logs for selected month.
#'
#' Compute list of log files, 'lst', for packageVersionPercent().
#' @param yr.mo Character. "yyyy-mm".
#' @note This is computationally intensive; you're downloading 30 odd files that are each around 50 MB in size (and creating a ~1.5 GB file)! Parallelization not practical; multiple attempts to connect to website causes problems. Truncates in-progress/future dates to yesterday's date. Automatically takes care of leap days (e.g., monthlyLog("2020-02").
#' @export

monthlyLog <- function(yr.mo = "2020-07") {
  start.date <- resolveDate(yr.mo)
  end.date <- resolveDate(yr.mo, "to")
  start.date <- fixDate_2012(start.date)
  end.date <- fixDate_2012(end.date)
  days <- seq(start.date, end.date, by = "day")
  out <- lapply(days, fetchCranLog)
  names(out) <- days
  out
}

computeVersionPercents <- function(lst, pkg, histories) {
  dates <- as.Date(names(lst))
  end.date <- dates[length(dates)]
  obs.ver.ct <- observedVersionCount(lst, pkg)
  exp.ver.ct <- expectedVersionCount(histories, pkg, dates)
  out <- data.frame(date = dates, package = pkg, obs = obs.ver.ct, 
    exp = exp.ver.ct, row.names = NULL)
  out$pct.of.versions <- 100 * out$obs / out$exp
  out
}

observedVersionCount <- function(lst, pkg) {
  vapply(lst, function(x) {
      length(unique(x[!is.na(x$package) & x$package == pkg, "version"]))
  }, integer(1L))
}

expectedVersionCount <- function(histories, pkg, dates) {
  vapply(seq_along(dates), function(i) {
    sum(histories[[paste(pkg)]]$Date <= dates[i])
  }, integer(1L))
}

#' Plot method for packageVersionPercent().
#' @param x An object of class "packageVersionPercent".
#' @param ... Additional plotting parameters.
#' @export

plot.packageVersionPercent <- function(x, ...) {
  pkg.data <- do.call(rbind, x$pkg.data)
  pkg.data$id <- seq_along(c(x$cran.pkgs, x$arch.pkgs))

  titleA <- "Percent of Versions Downloaded:"
  titleB <- 'CRAN (1-100) & Archive (101-200)'
  # titleC <- paste0(':', month(pkg.data[1, "date"]), year(pkg.data[1, "date"]))

  ggplot2::ggplot(data = pkg.data, 
                  ggplot2::aes(x = .data$id, y = .data$pct.of.versions)) +
    ggplot2::geom_point(color = "gray") +
    ggplot2::geom_smooth(method = "loess", formula = "y ~ x", se = FALSE,
      color = "red", linewidth = 1.5) +

    # https://github.com/tidyverse/ggplot2/issues/2963
    # geom_vline(xintercept) --> geom_vline(aes(xintercept))
    ggplot2::geom_vline(aes(xintercept = 99.5), col = "black", 
      linetype = "dashed") +

    ggplot2::xlab("Package ID") +
    ggplot2::ylab("Percent") +
    sugrrants::facet_calendar(~ as.Date(date), week_start = 7) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_y_continuous(breaks = c(0, 50, 100), limits = c(-10, 110)) +
    ggplot2::scale_x_continuous(breaks = 100) +
    # ggtitle(paste(titleA, titleB, titleC))
    ggplot2::ggtitle(paste(titleB))
}
