#' Annual/monthly package downloads from Bioconductor (beta).
#'
#' @param pkg Character. Vector of package names.
#' @param year Numeric.
#' @param month Numeric.
#' @param end.year Numeric.
#' @param end.month Numeric.
#' @param observation Character. "year" or "month"
#' @export
#' @examples
#' # all packages
#' bioconductorDownloads()
#'
#' # entire history
#' bioconductorDownloads(pkg = "clusterProfiler")
#'
#' # year-to-date
#' bioconductorDownloads(pkg = "clusterProfiler", year = 2019)
#'
#' # June 2014 througg March 2018
#' bioconductorDownloads(pkg = "clusterProfiler", year = 2014,
#'   end.year = 2018, month = 6, end.month = 3)
#'
#' # last 12 months
#' bioconductorDownloads(pkg = "clusterProfiler", year = "last-year")

bioconductorDownloads <- function(pkg = NULL, year = NULL, month = NULL,
  end.year = NULL, end.month = NULL, observation = "month") {

  cal.date <- Sys.Date()
  current.yr <- data.table::year(cal.date)
  current.mo <- data.table::month(cal.date)

  if (is.null(pkg)) {
    dat <- list(bioc_download(pkg, year, month, end.year, end.month,
      observation, current.yr, current.mo, cal.date))
  } else {
    if (length(pkg) > 1) {
      dat <- lapply(pkg, function(p) {
        bioc_download(p, year, month, end.year, end.month, observation,
          current.yr, current.mo, cal.date)
      })
      names(dat) <- pkg

    } else if (length(pkg) == 1) {
      dat <- list(bioc_download(pkg, year, month, end.year, end.month,
        observation, current.yr, current.mo, cal.date))
    }
  }

  if (observation == "year") {
    if (is.null(end.year)) {
      end.date <- cal.date
    } else if (is.null(end.year) == FALSE) {
      end.date <- as.Date(paste0(end.year, "-12-01"))
    }
  } else if (observation == "month") {
    if (is.null(end.year) & is.null(end.month)) {
      end.date <- cal.date
    } else if (is.null(end.year) == FALSE & is.null(end.month)) {
      end.date <- as.Date(paste0(end.year, "-12-01"))
    } else if (is.null(end.year) == FALSE & is.null(end.month) == FALSE) {
      end.date <- as.Date(paste0(end.year, "-", end.month ,"-01"))
    }
  }

  out <- list(data = dat, pkg = pkg, obs = observation, date = cal.date,
    current.yr = current.yr, current.mo = current.mo, end.date = end.date)
  class(out) <- "bioconductor"
  out
}

#' Plot method for bioconductor_downloads().
#'
#' @param x object.
#' @param graphics Character. NULL, "base" or "ggplot2".
#' @param count Character. "download" or "ip".
#' @param add.points Logical. Add points.
#' @param smooth Logical. Add stats::lowess smoother.
#' @param smooth.f Numeric. smoother span.
#' @param se Logical. Works only with graphics = "ggplot2".
#' @param log_count Logical. Logarithm of package downloads.
#' @param ... Additional plotting parameters.
#' @export
#' @examples
#' plot(bioconductorDownloads())
#' plot(bioconductorDownloads(pkg = "graph"))
#' plot(bioconductorDownloads(pkg = "graph", year = 2019))
#' plot(bioconductorDownloads(pkg = "graph", year = 2014, end.year = 2018, month = 6, end.month = 3))

plot.bioconductor <- function(x, graphics = NULL, count = "download",
  add.points = TRUE, smooth = FALSE, smooth.f = 2/3, se = FALSE,
  log_count = FALSE, ...) {

  if (x$obs == "year") {
    date.test <- data.table::year(x$date) == data.table::year(x$end.date)
    obs.in.progress <- ifelse(date.test, TRUE, FALSE)

  } else if (x$obs == "month") {
    current.yr <- data.table::year(x$date)
    start.obs <- as.Date(paste0(current.yr, "-", data.table::month(x$date),
      "-01"))
    stop.obs <- as.Date(paste0(current.yr, "-", data.table::month(x$date) + 1,
      "-01"))
    obs.in.progress <- ifelse(x$end.date >= start.obs & x$end.date < stop.obs,
      TRUE, FALSE)
  }

  if (is.null(graphics)) graphics <- "base"
  else {
    if (all(graphics %in% c("base", "ggplot2") == FALSE))
    stop('graphics must be "base" or "ggplot2"')
  }

  if (graphics == "base") {
    bioc_plot(x, graphics, count, add.points, smooth, smooth.f, log_count,
      obs.in.progress)
  } else if (graphics == "ggplot2") {
    gg_bioc_plot(x, graphics, count, add.points, smooth, smooth.f, se,
      log_count, obs.in.progress)
  }
}

#' Print method for bioconductorDownloads().
#' @param x object.
#' @param ... Additional parameters.
#' @export

print.bioconductor <- function(x, ...) {
  if (is.data.frame(x$data)) print(x$data)
  else if (is.list(x$data)) {
    out <- do.call(rbind, x$data)
    row.names(out) <- NULL
    print(out)
  }
}

#' Summary method for bioconductorDownloads().
#' @param object Object.
#' @param ... Additional parameters.
#' @export

summary.bioconductor <- function(object, ...) {
  if (is.data.frame(object$data)) object$data
  else if (is.list(object$data)) {
    out <- do.call(rbind, object$data)
    row.names(out) <- NULL
    out
  }
}

bioc_download <- function(pkg, year, month, end.year, end.month, observation,
  current.yr, current.mo, cal.date) {

  if (is.null(pkg)) {
    url <- "https://bioconductor.org/packages/stats/bioc/bioc_stats.tab"
  } else {
    url <- paste0("https://bioconductor.org/packages/stats/bioc/", pkg, "/",
      pkg, "_stats.tab", collapse = "")
  }

  pkg.data <- as.data.frame(mfetchLog(url))

  if (observation == "year") {
    if (is.null(year) & is.null(end.year)) {
      dat <- lapply(unique(pkg.data$Year), function(yr) {
        pkg.data[pkg.data$Year == yr & pkg.data$Month == "all", ]
      })
      dat <- do.call(rbind, dat)
    } else if (!is.null(year) & is.null(end.year)) {
      dat <- pkg.data[pkg.data$Year == year & pkg.data$Month == "all", ]
    } else if (!is.null(year) & !is.null(end.year)) {
      dat <- pkg.data[pkg.data$Year %in% year:end.year &
                      pkg.data$Month == "all", ]
    } else stop('If you provide "end.year", you must also provide "year".')

    dat <- dat[order(dat$Year), ]
    row.names(dat) <- NULL
    if (is.null(pkg) == FALSE) dat$pkg <- pkg

    dat <- dat[order(dat$Year), ]

  } else if (observation == "month") {
    pkg.data <- pkg.data[pkg.data$Month != "all", ]

    if (is.null(year) & is.null(end.year) &
        is.null(month) & is.null(end.month)) {
      dat <- pkg.data

    } else if (!is.null(year) & !is.character(year) & is.null(end.year) &
               is.null(month) & is.null(end.month)) {
      dat <- pkg.data[pkg.data$Year == year, ]

    } else if (!is.null(year) & !is.null(end.year) &
               is.null(month) & is.null(end.month)) {
      dat <- pkg.data[pkg.data$Year %in% year:end.year, ]

    } else if (!is.null(year) & is.null(end.year) &
               !is.null(month) & is.null(end.month)) {
      sel <- pkg.data$Year == year & pkg.data$Month == month.abb[month]
      dat <- pkg.data[sel, ]

    } else if (!is.null(year) & is.null(end.year) &
               !is.null(month) & is.null(end.month)) {
      sel <- pkg.data$Year == year & pkg.data$Month == month.abb[month]
      dat <- pkg.data[sel, ]

    } else if (!is.null(year) & is.null(end.year) &
               !is.null(month) & !is.null(end.month)) {
      sel <- pkg.data$Year == year &
             pkg.data$Month %in% month.abb[month:end.month]
      dat <- pkg.data[sel, ]

    } else if (!is.null(year) & !is.null(end.year) &
               !is.null(month) & is.null(end.month)) {
      sel.endpts <- pkg.data$Year == year &
                   pkg.data$Month %in% month.abb[month:12] |
                   pkg.data$Year == end.year

      if (end.year - year == 1) {
         sel <- sel.endpts
      } else {
         yrs <- seq(year, end.year)
         sel.yrs <- pkg.data$Year %in% yrs[yrs %in% c(year, end.year) == FALSE]
         sel <- sel.endpts | sel.yrs
      }

      dat <- pkg.data[sel, ]

    } else if (!is.null(year) & !is.null(end.year) &
               !is.null(month) & !is.null(end.month)) {

      sel.endpts <- pkg.data$Year == year &
                    pkg.data$Month %in% month.abb[month:12] |
                    pkg.data$Year == end.year &
                    pkg.data$Month %in% month.abb[1:end.month]

      if (end.year - year == 1) {
        sel <- sel.endpts
      } else {
        yrs <- seq(year, end.year)
        sel.yrs <- pkg.data$Year %in% yrs[yrs %in% c(year, end.year) == FALSE]
        sel <- sel.endpts | sel.yrs
      }

       dat <- pkg.data[sel, ]

     } else if (is.character(year) & is.null(end.year) &
                is.null(month) & is.null(end.month)) {
       if (year == "last-year") {
         sel.start <- pkg.data$Year == current.yr - 1 &
                      pkg.data$Month %in% month.abb[current.mo:12]
         sel.end <- pkg.data$Year == current.yr &
                    pkg.data$Month %in% month.abb[1:current.mo]
         dat <- pkg.data[sel.start | sel.end, ]
       }
    } else stop('error.')

    dat$mo <- NA

    for (i in seq_along(month.abb)) {
      dat[dat$Month == month.abb[i], "mo"] <- i
    }

    if (any(dat$Year == current.yr & dat$mo > current.mo)) {
      dat0 <- dat[dat$Year != current.yr, ]
      dat <- rbind(dat0, dat[dat$Year == current.yr & dat$mo <= current.mo, ])
    }

    dat <- dat[order(dat$Year, dat$mo), ]
    dat$mo <- NULL
    row.names(dat) <- NULL
    if (is.null(pkg) == FALSE) dat$pkg <- pkg
    dat
  }

  dat
}

bioc_plot <- function(x, graphics, count, add.points, smooth, smooth.f,
  log_count, obs.in.progress) {

  obs <- x$obs
  date <- x$date

  if (count == "download") {
    y.var <- "Nb_of_downloads"
    y.lab <- "Downloads"
  } else if (count == "ip") {
    y.var <- "Nb_of_distinct_IPs"
    y.lab <- "Unique IP Addresses"
  }

  invisible(lapply(x$data, function(dat) {
    if (obs == "month") {
      mo <- vapply(dat$Month, function(mo) which(mo == month.abb), numeric(1L))
      dat$date <- as.Date(paste0(dat$Year, "-", mo, "-01"))

      if (any(dat$date < x$date)) {
        dat <- dat[dat$date < x$date, ]
      }

      if (log_count) {
        plot(dat$date, dat[, y.var], type = "l", xlab = "Year",
          ylab = paste0("log10(", y.lab, ")"), log = "y")
      } else {
        plot(dat$date, dat[, y.var], type = "l", xlab = "Year", ylab = y.lab)
      }

       if (add.points) {
         if (obs.in.progress) {
           points(dat[1:(nrow(dat) - 1), "date"], dat[1:(nrow(dat) - 1), y.var],
             pch = 1)
           points(dat[nrow(dat), "date"], dat[nrow(dat), y.var], pch = 15,
             col = "red")
         } else points(dat$date, dat[, y.var])
       }

       if (smooth) {
         lines(stats::lowess(dat$date, dat[, y.var], f = smooth.f),
           col = "blue")
       }
     } else if (obs == "year") {
       if (log_count) {
         plot(dat$Year, dat[, y.var], type = "l", xlab = "Year",
           ylab = paste0("log10(", y.lab, ")"), log = "y")
       } else {
         plot(dat$Year, dat[, y.var], type = "l", xlab = "Year", ylab = y.lab)
       }

       if (add.points) {
         if (obs.in.progress) {
           points(dat[1:(nrow(dat) - 1), "Year"], dat[1:(nrow(dat) - 1), y.var],
             pch = 1)
           points(dat[nrow(dat), "Year"], dat[nrow(dat), y.var], pch = 15,
             col = "red")
         } else points(dat$Year, dat[, y.var])
       }
     }

    if (is.null(dat$pkg)) {
       title(main = "All Packages")
     } else {
       title(main = unique(dat$pkg))
     }
  }))
}

gg_bioc_plot <- function(x, graphics, count, add.points, smooth, smooth.f, se,
  log_count, obs.in.progress) {

  obs <- x$obs
  date <- x$date
  dat <- summary(x)

  mo <- vapply(dat$Month, function(mo) which(mo == month.abb), numeric(1L))
  dat$date <- as.Date(paste0(dat$Year, "-", mo, "-01"))

  if (count == "download") {
    p <- ggplot(data = dat, aes_string("date", "Nb_of_downloads")) +
         ylab("Downloads")
  } else if (count == "ip") {
    p <- ggplot(data = dat, aes_string("date", "Nb_of_distinct_IPs")) +
         ylab("Unique IP Addresses")
  }

  p <- p + geom_line(size = 0.5) + facet_wrap(~ pkg, ncol = 2) + xlab("Date") +
    theme_bw() + theme(panel.grid.minor = element_blank())

  if (add.points & log_count & smooth) {
    p + geom_point() + scale_y_log10() + geom_smooth(method = "loess", se = se)
  } else if (add.points & log_count & !smooth) {
    p + geom_point() + scale_y_log10()
  } else if (add.points & !log_count & smooth) {
    p +  geom_point() + geom_smooth(method = "loess", se = se)
  } else if (!add.points & log_count & smooth) {
    p + scale_y_log10() + geom_smooth(method = "loess", se = se)
  } else if (!add.points & !log_count & smooth) {
    p + geom_smooth(method = "loess", se = se)
  } else if (add.points & !log_count & !smooth) {
    p + geom_point()
  } else if (!add.points & log_count & !smooth) {
    p + scale_y_log10()
  } else p
}
