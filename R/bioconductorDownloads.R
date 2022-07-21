#' Annual/monthly package downloads from Bioconductor.
#'
#' @param packages Character. Vector of package names.
#' @param when \code{"last-year"}, or \code{"year-to-date"} or \code{"ytd"}.
#' @param from Start date as \code{yyyy-mm} or \code{yyyy}.
#' @param to End date as \code{yyyy-mm} or \code{yyyy}.
#' @param unit.observation "year" or "month".
#' @export
#' @examples
#' \dontrun{
#' # all packages
#' bioconductorDownloads()
#'
#' # entire history
#' bioconductorDownloads(packages = "clusterProfiler")
#'
#' # year-to-date
#' bioconductorDownloads(packages = "clusterProfiler", when = "ytd")
#' bioconductorDownloads(packages = "clusterProfiler", when = "year-to-date")
#'
#' # last 12 months
#' bioconductorDownloads(packages = "clusterProfiler", when = "last-year")
#'
#' # from 2015 to current year
#' bioconductorDownloads(packages = "clusterProfiler", from = 2015)
#'
#' # 2010 through 2015 (yearly)
#' bioconductorDownloads(packages = "clusterProfiler", from = 2010, to = 2015,
#'   unit.observation = "year")
#'
#' # selected year (yearly)
#' bioconductorDownloads(packages = "clusterProfiler", from = 2015, to = 2015)
#'
#' # selected year (monthly)
#' bioconductorDownloads(packages = "clusterProfiler", from = "2015-01", to = "2015-12")
#'
#' # June 2014 through March 2015
#' bioconductorDownloads(packages = "clusterProfiler", from = "2014-06", to = "2015-03")
#' }

bioconductorDownloads <- function(packages = NULL, from = NULL, to = NULL,
  when = NULL, unit.observation = "month") {

  # January 2009
  if (unit.observation %in% c("month", "year") == FALSE) {
    stop('unit.observation must be "month" or "year".', call. = FALSE)
  }

  current.date <- Sys.Date()
  current.yr <- as.numeric(format(current.date, "%Y"))
  current.mo <- as.numeric(format(current.date, "%m"))

  if (is.null(packages)) {
    dat <- list(bioc_download(packages, from, to, when, current.date,
      current.yr, current.mo, unit.observation))
  } else {
    if (length(packages) > 1) {
      dat <- lapply(packages, function(p) {
        bioc_download(p, from, to, when, current.date, current.yr, current.mo,
          unit.observation)
      })
      names(dat) <- packages

    } else if (length(packages) == 1) {
      dat <- list(bioc_download(packages, from, to, when, current.date,
        current.yr, current.mo, unit.observation))
    }
  }

  dat <- lapply(dat, function(x) {
    x$cumulative_Nb_of_distinct_IPs <- cumsum(x$Nb_of_distinct_IPs)
    x$cumulative_Nb_of_downloads <- cumsum(x$Nb_of_distinct_IPs)
    x
  })

  out <- list(data = dat, packages = packages, current.date = current.date,
    current.yr = current.yr, current.mo = current.mo,
    unit.observation = unit.observation)
  class(out) <- "bioconductorDownloads"
  out
}

#' Plot method for bioconductorDownloads().
#'
#' @param x object.
#' @param graphics Character. NULL, "base" or "ggplot2".
#' @param count Character. "download" or "ip".
#' @param cumulative Logical. Use cumulative counts.
#' @param points Character of Logical. Plot points. "auto", TRUE, FALSE. "auto" for bioconductorDownloads(unit.observation = "month") with 24 or fewer months, points are plotted.
#' @param smooth Logical. Add stats::lowess smoother.
#' @param f Numeric. smoother window for stats::lowess(). For graphics = "base" only; c.f. stats::lowess(f)
#' @param span Numeric. Smoothing parameter for geom_smooth(); c.f. stats::loess(span).
#' @param se Logical. Works only with graphics = "ggplot2".
#' @param log.y Logical. Logarithm of package downloads.
#' @param r.version Logical. Add R release dates.
#' @param same.xy Logical.  Use same scale for multiple packages when graphics = "base".
#' @param multi.plot Logical. Plot all data in a single window frame.
#' @param legend.loc Character.
#' @param ... Additional plotting parameters.
#' @export
#' @examples
#' \dontrun{
#' plot(bioconductorDownloads())
#' plot(bioconductorDownloads(packages = "graph"))
#' plot(bioconductorDownloads(packages = "graph", from = 2010, to = 2015))
#' plot(bioconductorDownloads(packages = "graph", from = "2014-06", to = "2015-03"))
#' plot(bioconductorDownloads(packages = c("graph", "IRanges", "S4Vectors"), from = 2018))
#' }

plot.bioconductorDownloads <- function(x, graphics = NULL, count = "download",
  cumulative = FALSE, points = "auto", smooth = FALSE, f = 2/3, span = 3/4,
  se = FALSE, log.y = FALSE, r.version = FALSE, same.xy = TRUE,
  multi.plot = FALSE, legend.loc = "topleft", ...) {

  if(x$unit.observation == "month") {
    if (points == "auto") {
      if (length(unique(do.call(rbind, x$data)$date)) <= 24) {
        points <- TRUE
      } else {
        points <- FALSE
      }
    } else if (is.logical(points) == FALSE) {
      stop('points must be "auto", TRUE, or FALSE.', call. = FALSE)
    }
  } else if (x$unit.observation == "year") points <- TRUE

  if (x$unit.observation == "year") {
    obs.in.progress <- x$current.yr == max(x$data[[1]]$Year)
  } else if (x$unit.observation == "month") {
    start.obs <- x$data[[1]]$date[1]
    stop.obs <- rev(x$data[[1]]$date)[1]
    obs.in.progress <- x$current.yr == as.numeric(format(stop.obs, "%Y")) &
                       x$current.mo == as.numeric(format(stop.obs, "%m"))
  }

  if (is.null(graphics)) {
    if (is.null(x$packages)) {
      graphics <- "base"
    } else if (length(x$packages) == 1) {
      graphics <- "base"
    } else graphics <- "ggplot2"
  } else {
    if (all(graphics %in% c("base", "ggplot2") == FALSE))
    stop('graphics must be "base" or "ggplot2".', call. = FALSE)
  }

  if (graphics == "base") {
    if (is.null(x$packages) | length(x$packages) == 1) {
      bioc_plot(x, graphics, count, points, smooth, f, log.y,
        obs.in.progress, r.version, same.xy, cumulative)
    } else if (length(x$packages) > 1) {
      if (multi.plot) {
        bioc_plot_multi(x, count, points, smooth, f, log.y,
          obs.in.progress, r.version, legend.loc, cumulative)
      } else {
        grDevices::devAskNewPage(ask = TRUE)
        bioc_plot(x, graphics, count, points, smooth, f, log.y,
          obs.in.progress, r.version, same.xy, cumulative)
        grDevices::devAskNewPage(ask = FALSE)
      }
    }
  } else if (graphics == "ggplot2") {
    gg_bioc_plot(x, graphics, count, points, smooth, span, se,
      log.y, obs.in.progress, multi.plot, cumulative)
  }
}

#' Print method for bioconductorDownloads().
#' @param x object.
#' @param ... Additional parameters.
#' @export

print.bioconductorDownloads <- function(x, ...) {
  if (is.data.frame(x$data)) print(x$data)
  else if (is.list(x$data)) {
    out <- do.call(rbind, x$data)
    row.names(out) <- NULL
    out$date <- NULL
    print(out)
  }
}

#' Summary method for bioconductorDownloads().
#' @param object Object.
#' @param ... Additional parameters.
#' @export

summary.bioconductorDownloads <- function(object, ...) {
  if (is.data.frame(object$data)) object$data
  else if (is.list(object$data)) {
    out <- do.call(rbind, object$data)
    row.names(out) <- NULL
    out
  }
}

bioc_download <- function(packages, from, to, when, current.date, current.yr,
  current.mo, unit.observation) {

  if (is.null(packages)) {
    url <- "https://bioconductor.org/packages/stats/bioc/bioc_stats.tab"
  } else {
    url <- paste0("https://bioconductor.org/packages/stats/bioc/", packages,
      "/", packages, "_stats.tab", collapse = "")
  }

  bioc.data <- as.data.frame(mfetchLog(url))

  if (!is.null(when)) {
    if (when == "last-year") {
      if (unit.observation == "month") {
        log.data <- bioc.data[bioc.data$Month != "all", ]
        month.num <- vapply(log.data$Month, function(x) {
          which(x == month.abb)
        }, integer(1L))
        month <- ifelse(nchar(month.num) == 1, paste0(0, month.num), month.num)
        log.data$date <- as.Date(paste0(log.data$Year, "-", month, "-01"))
        mo <- ifelse(nchar(current.mo) == 1, paste0(0, current.mo), current.mo)
        then <- as.Date(paste0(current.yr - 1, "-", mo, "-01"))
        dat <- log.data[log.data$date >= then & log.data$date <= current.date, ]
      } else if (unit.observation == "year") {
        log.data <- bioc.data[bioc.data$Month == "all", ]
        dat <- log.data[log.data$Year %in% c(current.yr, current.yr - 1), ]
        dat$date <- as.Date(paste0(dat$Year, "-01-01"))
      } else stop('"unit.observation must be "month" or "year".', call. = FALSE)

    } else if (when == "year-to-date" | when == "ytd") {
      if (unit.observation == "month") {
        log.data <- bioc.data[bioc.data$Month != "all", ]
        month.num <- vapply(log.data$Month, function(x) {
          which(x == month.abb)
        }, integer(1L))
        month <- ifelse(nchar(month.num) == 1, paste0(0, month.num), month.num)
        log.data$date <- as.Date(paste0(log.data$Year, "-", month, "-01"))
        mo <- ifelse(nchar(current.mo) == 1, paste0(0, current.mo), current.mo)
        then <- as.Date(paste0(current.yr, "-01-01"))
        dat <- log.data[log.data$date >= then & log.data$date <= current.date, ]
      } else if (unit.observation == "year") {
        log.data <- bioc.data[bioc.data$Month == "all", ]
        dat <- log.data[log.data$Year == current.yr, ]
        dat$date <- as.Date(paste0(dat$Year, "-01-01"))
      } else stop('"unit.observation must be "month" or "year".', call. = FALSE)
    } else {
      stop('when must be "last-year", "year-to-date" or "ytd".', call. = FALSE)
    }

  } else {
    if (is.null(from) & is.null(to)) {
      if (unit.observation == "month") {
        log.data <- bioc.data[bioc.data$Month != "all", ]
        month.num <- vapply(log.data$Month, function(x) {
          which(x == month.abb)
        }, integer(1L))
        month <- ifelse(nchar(month.num) == 1, paste0(0, month.num), month.num)
        log.data$date <- as.Date(paste0(log.data$Year, "-", month, "-01"))
        dat <- log.data
      } else if (unit.observation == "year") {
        dat <- bioc.data[bioc.data$Month == "all", ]
        dat$date <- as.Date(paste0(dat$Year, "-01-01"))
      } else stop('"unit.observation must be "month" or "year".', call. = FALSE)

    } else if (all(c(from, to) %in% 2009:current.yr)) {
      if (unit.observation == "month") {
        log.data <- bioc.data[bioc.data$Month != "all", ]
        month.num <- vapply(log.data$Month, function(x) {
          which(x == month.abb)
        }, integer(1L))
        month <- ifelse(nchar(month.num) == 1, paste0(0, month.num), month.num)
        log.data$date <- as.Date(paste0(log.data$Year, "-", month, "-01"))
        dat <- log.data
        if (!is.null(from) & is.null(to)) {
          dat <- log.data[log.data$Year >= from, ]
        } else if (is.null(from) & !is.null(to)) {
          dat <- log.data[log.data$Year <= to, ]
        } else if (!is.null(from) & !is.null(to)) {
          dat <- log.data[log.data$Year >= from & log.data$Year <= to, ]
        } else dat <- log.data
      } else if (unit.observation == "year") {
        log.data <- bioc.data[bioc.data$Month == "all", ]
        if (!is.null(from) & is.null(to)) {
          dat <- log.data[log.data$Year >= from, ]
        } else if (is.null(from) & !is.null(to)) {
          dat <- log.data[log.data$Year <= to, ]
        } else if (!is.null(from) & !is.null(to)) {
          dat <- log.data[log.data$Year >= from & log.data$Year <= to, ]
        } else dat <- log.data
        dat$date <- as.Date(paste0(dat$Year, "-01-01"))
      }

    } else if (
      all(vapply(c(from, to), is.character, logical(1L))) &
      all(vapply(c(from, to), nchar, integer(1L)) == 7) &
      all(vapply(c(from, to), function(x) grepl("-", x), logical(1L)))) {

      log.data <- bioc.data[bioc.data$Month != "all", ]
      month.num <- vapply(log.data$Month, function(x) {
        which(x == month.abb)
      }, integer(1L))
      month <- ifelse(nchar(month.num) == 1, paste0(0, month.num), month.num)
      log.data$date <- as.Date(paste0(log.data$Year, "-", month, "-01"))

      if (!is.null(from) & is.null(to)) {
        dat <- log.data[log.data$date %in% checkDate(from):current.date, ]
      } else if (is.null(from) & !is.null(to)) {
        dat <- log.data[log.data$date <= checkDate(to), ]
      } else if (!is.null(from) & !is.null(to)) {
        sel <- log.data$date >= checkDate(from) & log.data$date <= checkDate(to)
        dat <- log.data[sel, ]
      } else dat <- log.data
    } else {
      msg1 <- '"from" and "to" are formatted as "yyyy" or "yyyy-mm". '
      msg2 <- 'Logs begin January 2009.'
      stop(msg1, msg2, call. = FALSE)
    }
  }
  row.names(dat) <- NULL
  if (is.null(packages) == FALSE) dat$packages <- packages
  dat <- dat[order(dat$date), ]
  dat[dat$date <= current.date, ]
}

bioc_plot <- function(x, graphics, count, points, smooth, f, log.y,
  obs.in.progress, r.version, same.xy, cumulative) {

  obs <- x$unit.observation
  type <- ifelse(points, "o", "l")

  if (count == "download") {
    ylab <- "Downloads"
    if (cumulative) {
      y.var <- "cumulative_Nb_of_downloads"
    } else {
      y.var <- "Nb_of_downloads"
    }
  } else if (count == "ip") {
    ylab <- "Unique IP Addresses"
    if (cumulative) {
      y.var <- "cumulative_Nb_of_distinct_IPs"
    } else {
      y.var <- "Nb_of_distinct_IPs"
    }
  }

  x.var <- tools::toTitleCase(obs)

  if (obs.in.progress) {
    today <- x$current.date
    end.of.month <- lastDayMonth(today)$date

    est.ct <- vapply(x$data, function(dat) {
      ip.data <- dat[nrow(dat), ]
      complete.data <- dat[-nrow(dat), ]
      obs.days <- as.numeric(format(today, "%d"))
      exp.days <- as.numeric(format(end.of.month, "%d"))
      if (cumulative) {
        if (count == "download") {
          delta <- ip.data[, "Nb_of_downloads"] * exp.days / obs.days
        } else if (count == "ip") {
          delta <- ip.data[, "Nb_of_distinct_IPs"] * exp.days / obs.days
        }
        round(complete.data[nrow(complete.data), y.var] + delta)
      } else {
        round(ip.data[, y.var] * exp.days / obs.days)
      }
    }, numeric(1L))

    if (same.xy) {
      xlim <- range(do.call(rbind, x$data)$date)
      ylim <- range(c(do.call(rbind, x$data)[, y.var], est.ct))
    } else {
      xlim <- NULL
      ylim <- NULL
    }

    invisible(lapply(seq_along(x$data), function(i) {
      dat <- x$data[[i]]
      ip.data <- dat[nrow(dat), ]
      complete.data <- dat[1:(nrow(dat) - 1), ]
      last.obs <- nrow(complete.data)
      est.data <- ip.data
      est.data[, y.var] <- est.ct[i]

      if (log.y) {
        plot(complete.data$date, complete.data[, y.var], type = type,
          xlab = "Date", ylab = paste0("log10 ", ylab), xlim = xlim,
          ylim = ylim, log = "y")
      } else {
        plot(complete.data$date, complete.data[, y.var], type = type,
          xlab = "Date", ylab = ylab, xlim = xlim, ylim = ylim)
      }

      points(ip.data[, "date"], ip.data[, y.var], col = "gray")
      points(est.data[, "date"], est.data[, y.var], col = "red")

      segments(complete.data[last.obs, "date"],
               complete.data[last.obs, y.var],
               ip.data$date,
               ip.data[, y.var],
               lty = "dotted")
      segments(complete.data[last.obs, "date"],
               complete.data[last.obs, y.var],
               est.data$date,
               est.data[, y.var],
               col = "red")

      axis(4, at = ip.data[, y.var], labels = "obs")
      axis(4, at = est.data[, y.var], labels = "est", col.axis = "red",
        col.ticks = "red")
      title(main = x$packages[i])

      if (smooth) {
        smooth.data <- rbind(complete.data, est.data)
        lines(stats::lowess(smooth.data$date, smooth.data[, y.var], f = f),
          col = "blue")
      }

      if (r.version) {
        r_v <- rversions::r_versions()
        axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
          cex.axis = 2/3, padj = 0.9)
      }
    }))

  } else {
    if (same.xy) {
      xlim <- range(do.call(rbind, x$data)$date)
      ylim <- range(do.call(rbind, x$data)[, y.var])
    } else {
      xlim <- NULL
      ylim <- NULL
    }

    invisible(lapply(seq_along(x$data), function(i) {
      dat <- x$data[[i]]

      if (log.y) {
        plot(dat[, x.var], dat[, y.var], type = type, xlab = "Year",
          ylab = paste0("log10(", ylab, ")"), log = "y")
      } else {
        plot(dat[, x.var], dat[, y.var], type = type, xlab = "Year",
          ylab = ylab)
      }

      if (points) points(dat$date, dat[, y.var])

      if (smooth) {
        lines(stats::lowess(dat$date, dat[, y.var], f = f), col = "blue")
      }
      if (r.version) {
        r_v <- rversions::r_versions()
        axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
          cex.axis = 2/3, padj = 0.9)
      }

      title(main = x$packages[i])
    }))
  }

  if (is.null(x$packages)) title(main = "All Packages")
}

bioc_plot_multi <- function(x, count, points, smooth, f, log.y,
  obs.in.progress, r.version, legend.loc, cumulative) {

  obs <- x$unit.observation
  type <- ifelse(points, "o", "l")

  if (count == "download") {
    ylab <- "Downloads"
    if (cumulative) {
      y.var <- "cumulative_Nb_of_downloads"
    } else {
      y.var <- "Nb_of_downloads"
    }
  } else if (count == "ip") {
    ylab <- "Unique IP Addresses"
    if (cumulative) {
      y.var <- "cumulative_Nb_of_distinct_IPs"
    } else {
      y.var <- "Nb_of_distinct_IPs"
    }
  }

  dat <- do.call(rbind, x$data)
  vars <- c("date", y.var)
  xlim <- range(dat$date)
  ylim <- range(dat[, y.var])

  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  # http://jfly.iam.u-tokyo.ac.jp/color/
  # The palette with grey:
  # cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
  #   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7")
  token <- c(1, 0,  2:7)

  if (obs.in.progress) {
    today <- x$current.date
    end.of.month <- lastDayMonth(today)$date

    est.ct <- vapply(x$data, function(dat) {
      ip.data <- dat[nrow(dat), ]
      complete.data <- dat[-nrow(dat), ]
      obs.days <- as.numeric(format(today, "%d"))
      exp.days <- as.numeric(format(end.of.month, "%d"))
      if (cumulative) {
        if (count == "download") {
          delta <- ip.data[, "Nb_of_downloads"] * exp.days / obs.days
        } else if (count == "ip") {
          delta <- ip.data[, "Nb_of_distinct_IPs"] * exp.days / obs.days
        }
        round(complete.data[nrow(complete.data), y.var] + delta)
      } else {
        round(ip.data[, y.var] * exp.days / obs.days)
      }
    }, numeric(1L))

    ylim <- range(c(ylim, est.ct))

    if (log.y) {
      plot(x$data[[1]]$date, x$data[[1]][, y.var], pch = NA, xlab = "Date",
        ylab = paste0("log10 ", ylab), xlim = xlim, ylim = ylim, log = "y")
    } else {
      plot(x$data[[1]]$date, x$data[[1]][, y.var], pch = NA,
        xlab = "Date", ylab = ylab, xlim = xlim, ylim = ylim)
    }

    invisible(lapply(seq_along(x$data), function(i) {
      tmp <- x$data[[i]]
      complete.data <- tmp[1:(nrow(tmp) - 1), ]
      ip.data <- tmp[nrow(tmp), ]
      lines(complete.data[, vars], col = cbPalette[i], type = type,
        pch = token[i])
      segments(complete.data[nrow(complete.data), "date"],
               complete.data[nrow(complete.data), y.var],
               ip.data$date,
               ip.data[, y.var],
               lty = "dotted",
               col = "gray")
       segments(complete.data[nrow(complete.data), "date"],
                complete.data[nrow(complete.data), y.var],
                ip.data$date,
                est.ct[i],
                col = cbPalette[i])
      points(ip.data[, "date"], ip.data[, y.var], col = "gray", pch = token[i])
      points(ip.data[, "date"], est.ct[i], col = "red", pch = token[i])
      if (smooth) {
        est.data <- ip.data
        est.data[, y.var] <- est.ct[i]
        smooth.data <- rbind(complete.data, est.data)
        lines(stats::lowess(smooth.data$date, smooth.data[, y.var], f = f),
          col = cbPalette[i], lwd = 1.5)
      }
    }))

  } else {
    if (log.y) {
      plot(x$data[[1]]$date, x$data[[1]][, y.var], pch = NA, xlab = "Date",
        ylab = paste0("log10 ", ylab), xlim = xlim, ylim = ylim, log = "y")
    } else {
      plot(x$data[[1]]$date, x$data[[1]][, y.var], pch = NA, xlab = "Date",
        ylab = ylab, xlim = xlim, ylim = ylim)
    }

    invisible(lapply(seq_along(x$packages), function(i) {
      tmp <- dat[dat$package == x$packages[i], ]
      lines(tmp$date, tmp[, y.var], type = type, col = cbPalette[i])
      if (smooth) {
        lines(stats::lowess(dat[dat$package == x$packages[i], vars],
          f = f), col = cbPalette[i])
      }
    }))
  }

  if (r.version) {
    r_v <- rversions::r_versions()
    axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
      cex.axis = 2/3, padj = 0.9)
  }

  id <- seq_along(x$packages)
  legend(x = legend.loc,
         legend = x$packages,
         col = cbPalette[id],
         pch = c(1, token[id]),
         bg = "white",
         cex = 2/3,
         title = NULL,
         lwd = 1)
}

gg_bioc_plot <- function(x, graphics, count, points, smooth, span, se,
  log.y, obs.in.progress, multi.plot, cumulative) {

  obs <- x$unit.observation
  date <- x$current.date
  dat <- summary(x)

  if (count == "download") {
    ylab <- "Downloads"
    if (cumulative) {
      y.var <- "cumulative_Nb_of_downloads"
    } else {
      y.var <- "Nb_of_downloads"
    }
  } else if (count == "ip") {
    ylab <- "Unique IP Addresses"
    if (cumulative) {
      y.var <- "cumulative_Nb_of_distinct_IPs"
    } else {
      y.var <- "Nb_of_distinct_IPs"
    }
  }

  if (obs.in.progress) {
    today <- x$current.date
    end.of.month <- lastDayMonth(today)$date
    ip.data <- dat[dat$date == max(dat$date), ]
    complete.data <- dat[dat$date != max(dat$date), ]
    obs.days <- as.numeric(format(today, "%d"))
    exp.days <- as.numeric(format(end.of.month, "%d"))

    if (cumulative) {
      if (count == "download") {
        delta <- ip.data[, "Nb_of_downloads"] * exp.days / obs.days
      } else if (count == "ip") {
        delta <- ip.data[, "Nb_of_distinct_IPs"] * exp.days / obs.days
      }
      sel <- complete.data$date == max(complete.data$date)
      est.ct <- round(complete.data[sel, y.var] + delta)
    } else {
      est.ct <- round(ip.data[, y.var] * exp.days / obs.days)
    }

    est.data <- ip.data
    est.data[, y.var] <- est.ct
    last.obs <- complete.data$date == max(complete.data$date)
    est.seg <- rbind(complete.data[last.obs, ], est.data)
    obs.seg <- rbind(complete.data[last.obs, ], ip.data)

    if (multi.plot) {
      if (cumulative) {
        if (count == "download") {
          p <- ggplot(data = dat, aes_string("date",
            "cumulative_Nb_of_downloads", colour = "packages")) + ylab(ylab)
        } else if (count == "ip") {
          p <- ggplot(data = dat, aes_string("date",
            "cumulative_Nb_of_distinct_IPs", colour = "packages")) + ylab(ylab)
        }
      } else {
        if (count == "download") {
          p <- ggplot(data = dat, aes_string("date", "Nb_of_downloads",
            colour = "packages")) + ylab(ylab)
        } else if (count == "ip") {
          p <- ggplot(data = dat, aes_string("date", "Nb_of_distinct_IPs",
            colour = "packages")) + ylab(ylab)
        }
      }

      p <- p + geom_line(data = complete.data, size = 1/3) +
               geom_line(data = est.seg, size = 1/3) +
               geom_line(data = obs.seg, size = 1/3, linetype = "dotted") +
               geom_point(data = est.data, shape = 1) +
               geom_point(data = ip.data, shape = 0) +
               xlab("Date") +
               theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())

    } else {
      if (cumulative) {
        if (count == "download") {
          p <- ggplot(data = dat,
            aes_string("date", "cumulative_Nb_of_downloads")) + ylab(ylab)
        } else if (count == "ip") {
          p <- ggplot(data = dat,
            aes_string("date", "cumulative_Nb_of_distinct_IPs")) + ylab(ylab)
        }
      } else {
        if (count == "download") {
          p <- ggplot(data = dat, aes_string("date", "Nb_of_downloads")) +
               ylab(ylab)
        } else if (count == "ip") {
          p <- ggplot(data = dat, aes_string("date", "Nb_of_distinct_IPs")) +
               ylab(ylab)
        }
      }

      p <- p + geom_line(data = complete.data, size = 1/3) +
               geom_line(data = est.seg, size = 1/3, col = "red") +
               geom_line(data = obs.seg,  size = 1/3, linetype = "dotted") +
               geom_point(data = est.data, col = "red") +
               geom_point(data = ip.data, shape = 0) +
               facet_wrap(~ packages, ncol = 2) +
               xlab("Date") +
               theme_bw() +
               theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())
    }

    if (points) p <- p + geom_point(data = complete.data)
    if (log.y) p <- p + scale_y_log10()
    if (smooth) {
      smooth.data <- rbind(complete.data, est.data)
      p <- p + geom_smooth(data = smooth.data, method = "loess",
        formula = "y ~ x", se = se, span = span)
    }

  } else {
    if (cumulative) {
      if (count == "download") {
        p <- ggplot(data = dat,
          aes_string("date", "cumulative_Nb_of_downloads")) + ylab(ylab)
      } else if (count == "ip") {
        p <- ggplot(data = dat,
          aes_string("date", "cumulative_Nb_of_distinct_IPs")) + ylab(ylab)
      }
    } else {
      if (count == "download") {
        p <- ggplot(data = dat, aes_string("date", "Nb_of_downloads")) +
             ylab(ylab)
      } else if (count == "ip") {
        p <- ggplot(data = dat, aes_string("date", "Nb_of_distinct_IPs")) +
             ylab(ylab)
      }
    }

    if (multi.plot) {
      p <- p + geom_line(size = 0.5) +
        xlab("Date") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    } else {
      p <- p + geom_line(size = 0.5) +
        facet_wrap(~ packages, ncol = 2) +
        xlab("Date") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }

    if (points) p <- p + geom_point()
    if (log.y) p <- p + scale_y_log10()
    if (smooth) {
      p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se,
        span = span)
    }
  }

  suppressWarnings(print(p))
}

checkDate <- function(string, end.date = FALSE) {
  if (!is.character(string)) {
    stop("date must a character string.", call. = FALSE)
  }
  if (nchar(string) != 7 | (grepl("-", string) == FALSE)) {
    stop('Format must be "yyyy-mm".', call. = FALSE)
  } else {
    date.parts <- unlist(strsplit(string, "-"))
    if (date.parts[2] %in% c(paste0(0, 1:9), paste(10:12)) == FALSE) {
      stop("Month must be between 01 and 12.", call. = FALSE)
    }
    if (date.parts[1] < 2009) {
      msg <- paste0('Bioconductor logs begin ', "January 2009", ".")
      warning(msg, call. = FALSE)
    }
  }

  date.candidate <- as.Date(paste0(string, "-01"), optional = TRUE)
  if (is.na(date.candidate)) stop("No such date.", call. = FALSE)
  else if (date.candidate > Sys.Date()) stop("Date in future!", call. = FALSE)
  else date.candidate
}
