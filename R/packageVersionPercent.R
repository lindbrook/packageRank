#' Compute data for versionPlot().
#'
#' packageRank::blog.data or recompute random sample of packages.
#' @param lst Object. List of CRAN download logs data frames. Use monthlyLogs().
#' @param resample Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

packageVersionPercent <- function(lst, resample = FALSE, multi.core = TRUE) {
  cores <- multiCore(multi.core)
  mo <- months(as.Date(names(lst)[1]))

  if (resample) {
    lst <- monthlyLogs(month = mo)
    cran.pkgs <- packageSample2(lst, multi.core = cores)
    arch.pkgs <- packageSample2(lst, "arch", multi.core = cores)
  } else {
    if (mo == "October") {
      cran.pkgs <- packageRank::blog.data$cran.pkgs.oct
      arch.pkgs <- packageRank::blog.data$arch.pkgs.oct
    } else if (mo == "July") {
      cran.pkgs <- packageRank::blog.data$cran.pkgs.jul
      arch.pkgs <- packageRank::blog.data$arch.pkgs.jul
    }
  }

  pkgs <- c(cran.pkgs, arch.pkgs)
  pkg.data <- lapply(pkgs, function(x) computeVersionPercents(lst, x, cores))
  #      user  system elapsed
  # 596.149 236.667 711.117

  names(pkg.data) <- pkgs
  out <- list(cran.pkgs = cran.pkgs, arch.pkgs = arch.pkgs, pkg.data = pkg.data)
  class(out) <- "packageVersionPercent"

  out
}

#' Get CRAN logs for October 2019 or July 2020.
#'
#' Compute 'lst' for packageVersions().
#' @param month Character. "October" or "July".
#' @note This is an intensive task since you're downloading 30 odd log files that are each around 50 MB in size.
#' @export

monthlyLogs <- function(month = "October") {
  if (month == "October") {
    days <- seq(as.Date("2019-10-01"), as.Date("2019-10-31"), by = "day")
  } else if (month == "July") {
    days <- seq(as.Date("2020-07-01"), as.Date("2019-07-31"), by = "day")
  } else stop('"month" must be "October" or "July".')
  lapply(days, function(d) packageLog0(d, memoization = FALSE))
}

computeVersionPercents <- function(lst, pkgs, cores) {
  dates <- as.Date(names(lst))
  end.date <- dates[length(dates)]
  obs.ver.ct <- observedVersionCount(lst, pkgs, cores)
  exp.ver.ct <- expectedVersionCount(pkgs, end.date + 1)
  out <- lapply(seq_along(obs.ver.ct), function(i) {
    data.frame(date = dates[i], package = names(obs.ver.ct[[i]]),
               obs = obs.ver.ct[[i]], row.names = NULL)
  })
  out <- do.call(rbind, out)
  out$exp <- exp.ver.ct
  out$pct.of.versions <- 100 * out$obs / out$exp
  out
}

observedVersionCount <- function(lst, pkgs, cores) {
  parallel::mclapply(lst, function(x) {
    vapply(pkgs, function(p) {
      length(unique(x[!is.na(x$package) & x$package == p, "version"]))
    }, integer(1L))
  }, mc.cores = cores)
}

expectedVersionCount <- function(pkgs, end.date) {
  h <- lapply(pkgs, packageHistory0)
  vapply(h, function(x) sum(x$date < end.date), integer(1L))
}

#' Plot method for packageVersionPercent().
#' @param x An object of class "packageVersions" created by \code{packageVersions()}.
#' @param ... Additional plotting parameters.
#' @export

plot.packageVersionPercent <- function(x, ...) {
  pkg.data <- do.call(rbind, x$pkg.data)
  pkg.data$id <- seq_along(c(x$cran.pkgs, x$arch.pkgs))

  titleA <- "Percent of Versions Downloaded:"
  titleB <- 'CRAN (1-100) & Archive "retired" (101-200)'

  ggplot(data = pkg.data, aes_string(x = "id", y = "pct.of.versions")) +
    geom_point(color = "gray") +
    geom_smooth(method = "loess", formula = "y ~ x", se = FALSE,
      color = "red", size = 1.5) +

    # https://github.com/tidyverse/ggplot2/issues/2963
    # geom_vline(xintercept) --> geom_vline(aes(xintercept))
    geom_vline(aes(xintercept = 99.5), col = "black", linetype = "dashed") +

    xlab("Package ID") +
    ylab("Percent") +
    sugrrants::facet_calendar(~ as.Date(date), week_start = 7) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(breaks = c(0, 50, 100), limits = c(-10, 110)) +
    scale_x_continuous(breaks = 100) +
    ggtitle(paste(titleA, titleB))
}
