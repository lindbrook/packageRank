#' Visualize a Package's Downloads Relative to "All" CRAN packages over Time.
#'
#' Uses a stratified random sample cohort of packages plus top ten packages.
#' @param x object.
#' @param graphics Character. NULL, "base" or "ggplot2".
#' @param log.count Logical. Logarithm of package downloads.
#' @param smooth Logical. Add smoother.
#' @param sample.smooth Logical. Add smoother.
#' @param f Numeric. smoother window for stats::lowess(). For graphics = "base" only; c.f. stats::lowess(f)
#' @param span Numeric. Smoothing parameter for geom_smooth(); c.f. stats::loess(span).
#' @param sample.pct Numeric. Percent of packages to sample.
#' @param population.seed Numeric. Seed for sample.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores to use. Note that due to performance considerations, the number of cores defaults to one on Windows.
#' @noRd

populationPlot <- function(x, graphics = NULL, log.count = TRUE, smooth = TRUE,
  sample.smooth = TRUE, f = 1/3, span = 3/4, sample.pct = 2.5,
  population.seed = as.numeric(Sys.Date()), multi.core = TRUE) {

  pkg.data <- x$cranlogs.data
  start.date <- pkg.data$date[1]
  end.date <- pkg.data$date[nrow(pkg.data)]

  cran_log <- fetchCranLog(start.date)
  init.pkgs <- unique(cran_log$package) # remove duplicated pkgs (diff versions)
  init.pkgs <- stats::na.omit(init.pkgs)

  pkgs <- cran_log[cran_log$package %in% init.pkgs, ]
  freqtab <- table(pkgs$package)
  cores <- multiCore(multi.core)

  rank.percentile <- parallel::mclapply(names(freqtab), function(nm) {
    mean(freqtab < freqtab[nm])
  }, mc.cores = cores)

  rank.percentile <- unlist(rank.percentile)

  pct <- data.frame(pkg = names(freqtab), percentile = rank.percentile,
    stringsAsFactors = FALSE)
  pct <- pct[order(pct$percentile, decreasing = TRUE), ]
  row.names(pct) <- NULL

  # bins #

  breaks <- seq(1, 0, -0.05)

  bin.id <- lapply(2:length(breaks), function(i) {
    which(pct$percentile > breaks[i] & pct$percentile <= breaks[i - 1])
  })

  # set seed for random sampling
  set.seed(population.seed)
  sample.id <- lapply(seq_along(bin.id), function(i) {
     sample(bin.id[[i]], round(sample.pct / 100 * length(bin.id[[i]])))
  })

  names(sample.id) <- paste(round(breaks[-1], 2))

  top10 <- pct[1:10, "pkg"]
  y.max <- max(cranlogs::cran_downloads(top10, from = start.date,
    to = end.date)$count)

  # cohort + top 10
  cohort <- pct[unlist(sample.id), "pkg"]
  if (any(top10 %in% cohort)) cohort <- unique(c(cohort, top10))
  if (any(x$packages %in% cohort)) {
    cohort <- cohort[cohort %in% x$packages == FALSE]
  }

  cohort.data <- cranlogs::cran_downloads(cohort, from = start.date,
    to = end.date)

  out <- list(data = cohort.data,
              pkg.data = pkg.data,
              packages = x$packages,
              y.max =  y.max)

  cran.smpl <- out$data
  pkg.data <- out$pkg.data
  packages <- out$packages

  if (log.count) {
    zero.test <- any(cran.smpl$count == 0) | any(pkg.data$count == 0)
    if (zero.test) {
      cran.smpl$count <- cran.smpl$count + 1
      pkg.data$count <- pkg.data$count + 1
    }
  }

  if (is.null(graphics)) {
    if (length(packages) == 1) graphics <- "base"
    else if (length(packages) > 1) graphics <- "ggplot2"
  } else {
    if (all(graphics %in% c("base", "ggplot2") == FALSE))
    stop('graphics must be "base" or "ggplot2"')
  }

  if (graphics == "base") {
    if (length(packages) > 1) {
      invisible(lapply(packages, function(pkg) {
        pkg.data.sel <- pkg.data[pkg.data$package == pkg, ]
        basePlotTime(out, log.count, cran.smpl, pkg.data.sel, smooth,
          sample.smooth, f)
        title(main = pkg)
      }))
    } else if (length(packages) == 1) {
      basePlotTime(out, log.count, cran.smpl, pkg.data, smooth, sample.smooth,
        f)
      title(main = packages)
    }

  } else if (graphics == "ggplot2") {
    p <- ggplot(data = pkg.data, aes_string("date", "count")) +
           theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank()) +
           facet_wrap(~ package, nrow = 2)

    cran.smpl.lst <- rep(list(cran.smpl), length(packages))

    for (i in seq_along(packages)) {
      cran.smpl.data <- cran.smpl.lst[[i]]
      for (pkg in unique(cran.smpl.data$package)) {
        sel <- cran.smpl.data$package == pkg
        if (sample.smooth) {
          p <- p + geom_smooth(data = cran.smpl.data[sel, c("date", "count")],
                               method = "loess",
                               formula = "y ~ x",
                               se = FALSE,
                               span = span,
                               size = 0.25,
                               colour = "lightgray")
        } else {
          p <- p + geom_line(data = cran.smpl[sel, c("date", "count")],
                             colour = "lightgray")
        }
      }
    }

    p <- p + geom_line(colour = "red", size = 1/3) +
             geom_point(shape = 1, size = 2, colour = "red")

    if (smooth) p <- p + geom_smooth(colour = "blue", method = "loess",
      formula = "y ~ x", se = FALSE, size = 0.75, span = span)

    if (log.count) p <- p + scale_y_log10()

    p
  } else stop('graphics must be "base" or "ggplot2"')
}


#' Base R Graphics Plot (Longitudinal).
#' @param x Object.
#' @param log.count Logical. Logarithm of package downloads.
#' @param cran.smpl Object.
#' @param pkg.data Object.
#' @param smooth Logical. Add smoother for selected package.
#' @param sample.smooth Logical. lowess background.
#' @param f Numeric. stats::lowess() smoother window.
#' @noRd

basePlotTime <- function(x, log.count, cran.smpl, pkg.data, smooth,
  sample.smooth, f) {

  if (log.count) {
    plot(cran.smpl$date, log10(cran.smpl$count), pch = NA,
      ylim = c(0, max(log10(x$y.max))), xlab = "Date", ylab = "log10(Count)")

    if (sample.smooth) {
      for (nm in unique(cran.smpl$package)) {
        lines(stats::lowess(cran.smpl[cran.smpl$package == nm, "date"],
              log10(cran.smpl[cran.smpl$package == nm, "count"]), f = f),
              col = "lightgray")
      }
    } else {
      for (nm in unique(cran.smpl$package)) {
        lines(cran.smpl[cran.smpl$package == nm, "date"],
              log10(cran.smpl[cran.smpl$package == nm, "count"]),
              col = "lightgray")
      }
    }

    lines(pkg.data$date, log10(pkg.data$count), lwd = 1, col = "red",
      type = "o")
    if (smooth) {
      lines(stats::lowess(pkg.data$date, log10(pkg.data$count), f = f),
        col = "blue", lwd = 1.5)
    }
  } else {
    plot(cran.smpl$date, cran.smpl$count, pch = NA, ylim = c(0, max(x$y.max)),
      xlab = "Date", ylab = "Count")

    if (sample.smooth) {
      for (nm in unique(cran.smpl$package)) {
        lines(stats::lowess(cran.smpl[cran.smpl$package == nm, "date"],
              cran.smpl[cran.smpl$package == nm, "count"], f = f),
              col = "lightgray")
      }
    } else {
      for (nm in unique(cran.smpl$package)) {
        lines(cran.smpl[cran.smpl$package == nm, "date"],
              cran.smpl[cran.smpl$package == nm, "count"],
              col = "lightgray")
      }
    }

    lines(pkg.data$date, pkg.data$count, lwd = 1, col = "red")
    lines(stats::lowess(pkg.data$date, pkg.data$count, f = f), col = "blue",
      lwd = 1.5)
  }
}
