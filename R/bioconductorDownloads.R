#' Annual/monthly package downloads from Bioconductor (beta).
#'
#' @param packages Character. Vector of package names.
#' @param when \code{"last-year"}, or \code{"year-to-date"} or \code{"ytd"}.
#' @param from Start date as \code{yyyy-mm} or \code{yyyy}.
#' @param to End date as \code{yyyy-mm} or \code{yyyy}.
#' @param observation "year" or "month".
#' @export
#' @examples
#' \donttest{
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
#' # 2015 through 2018 (yearly)
#' bioconductorDownloads(packages = "clusterProfiler", from = 2010, to = 2015, observation = "year")
#'
#' # selected year (yearly)
#' bioconductorDownloads(packages = "clusterProfiler", from = 2015, to = 2015)
#'
#' # selected year (monthly)
#' bioconductorDownloads(packages = "clusterProfiler", from = "2015-01", to = "2015-12")
#'
#' # June 2014 through March 2018
#' bioconductorDownloads(packages = "clusterProfiler", from = "2014-06", to = "2015-03")
#' }

bioconductorDownloads <- function(packages = NULL, from = NULL, to = NULL,
  when = NULL, observation = "month") {

  # January 2009
  if (observation %in% c("month", "year") == FALSE) {
    stop('observation must be "month" or "year".')
  }

  current.date <- Sys.Date()
  current.yr <- data.table::year(current.date)
  current.mo <- data.table::month(current.date)

  if (is.null(packages)) {
    dat <- list(bioc_download(packages, from, to, when, current.date,
      current.yr, current.mo, observation))
  } else {
    if (length(packages) > 1) {
      dat <- lapply(packages, function(p) {
        bioc_download(p, from, to, when, current.date, current.yr, current.mo,
          observation)
      })
      names(dat) <- packages

    } else if (length(packages) == 1) {
      dat <- list(bioc_download(packages, from, to, when, current.date,
        current.yr, current.mo, observation))
    }
  }

  out <- list(data = dat, packages = packages, current.date = current.date,
    current.yr = current.yr, current.mo = current.mo, observation = observation)
  class(out) <- "bioconductor"
  out
}

#' Plot method for bioconductor_downloads().
#'
#' @param x object.
#' @param graphics Character. NULL, "base" or "ggplot2".
#' @param count Character. "download" or "ip".
#' @param points Character of Logical. Plot points. "auto", TRUE, FALSE. "auto" for bioconductorDownloads(observation = "month") with 24 or fewer months, points are plotted.
#' @param smooth Logical. Add stats::lowess smoother.
#' @param smooth.f Numeric. smoother span.
#' @param se Logical. Works only with graphics = "ggplot2".
#' @param log_count Logical. Logarithm of package downloads.
#' @param ... Additional plotting parameters.
#' @export
#' @examples
#' \donttest{
#' plot(bioconductorDownloads())
#' plot(bioconductorDownloads(packages = "graph"))
#' plot(bioconductorDownloads(packages = "graph", from = 2010, to = 2015))
#' plot(bioconductorDownloads(packages = "graph", from = "2014-06", to = "2015-03"))
#' plot(bioconductorDownloads(packages = c("graph", "IRanges", "S4Vectors"), from = 2018))
#' }

plot.bioconductor <- function(x, graphics = NULL, count = "download",
  points = "auto", smooth = FALSE, smooth.f = 2/3, se = FALSE,
  log_count = FALSE, ...) {

  if( x$observation == "month") {
    if (points == "auto") {
      if (length(unique(do.call(rbind, x$data)$date)) <= 24) {
        points <- TRUE
      } else {
        points <- FALSE
      }
    } else if (is.logical(points) == FALSE) {
      stop('points must be "auto", TRUE, or FALSE.')
    }
  } else if (x$observation == "year") points <- TRUE

  if (x$observation == "year") {
    obs.in.progress <- x$current.yr == max(x$data[[1]]$Year)
  } else if (x$observation == "month") {
    start.obs <- x$data[[1]]$date[1]
    stop.obs <- rev(x$data[[1]]$date)[1]
    obs.in.progress <- x$current.yr == data.table::year(stop.obs) &
                       x$current.mo == data.table::month(stop.obs)
  }

  if (is.null(graphics)) {
    if (is.null(x$packages)) {
      graphics <- "base"
    } else if (length(x$packages) == 1) {
      graphics <- "base"
    } else graphics <- "ggplot2"
  } else {
    if (all(graphics %in% c("base", "ggplot2") == FALSE))
    stop('graphics must be "base" or "ggplot2"')
  }

  if (graphics == "base") {
    if (is.null(x$packages) | length(x$packages) == 1) {
      bioc_plot(x, graphics, count, points, smooth, smooth.f, log_count,
        obs.in.progress)
    } else if (length(x$packages) > 1) {
      grDevices::devAskNewPage(ask = TRUE)
      bioc_plot(x, graphics, count, points, smooth, smooth.f, log_count,
        obs.in.progress)
      grDevices::devAskNewPage(ask = FALSE)
    }
  } else if (graphics == "ggplot2") {
    gg_bioc_plot(x, graphics, count, points, smooth, smooth.f, se,
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
    out$date <- NULL
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

bioc_download <- function(packages, from, to, when, current.date, current.yr,
  current.mo, observation) {

  if (is.null(packages)) {
    url <- "https://bioconductor.org/packages/stats/bioc/bioc_stats.tab"
  } else {
    url <- paste0("https://bioconductor.org/packages/stats/bioc/", packages,
      "/", packages, "_stats.tab", collapse = "")
  }

  bioc.data <- as.data.frame(mfetchLog(url))

  if (!is.null(when)) {
    if (when == "last-year") {
      if (observation == "month") {
        log.data <- bioc.data[bioc.data$Month != "all", ]
        month.num <- vapply(log.data$Month, function(x) {
          which(x == month.abb)
        }, integer(1L))
        month <- ifelse(nchar(month.num) == 1, paste0(0, month.num), month.num)
        log.data$date <- as.Date(paste0(log.data$Year, "-", month, "-01"))
        mo <- ifelse(nchar(current.mo) == 1, paste0(0, current.mo), current.mo)
        then <- as.Date(paste0(current.yr - 1, "-", mo, "-01"))
        dat <- log.data[log.data$date >= then & log.data$date <= current.date, ]
      } else if (observation == "year") {
        log.data <- bioc.data[bioc.data$Month == "all", ]
        dat <- log.data[log.data$Year %in% c(current.yr, current.yr - 1), ]
        dat$date <- as.Date(paste0(dat$Year, "-01-01"))
      } else stop('"observation must be "month" or "year"')

    } else if (when == "year-to-date" | when == "ytd") {
      if (observation == "month") {
        log.data <- bioc.data[bioc.data$Month != "all", ]
        month.num <- vapply(log.data$Month, function(x) {
          which(x == month.abb)
        }, integer(1L))
        month <- ifelse(nchar(month.num) == 1, paste0(0, month.num), month.num)
        log.data$date <- as.Date(paste0(log.data$Year, "-", month, "-01"))
        mo <- ifelse(nchar(current.mo) == 1, paste0(0, current.mo), current.mo)
        then <- as.Date(paste0(current.yr, "-01-01"))
        dat <- log.data[log.data$date >= then & log.data$date <= current.date, ]
      } else if (observation == "year") {
        log.data <- bioc.data[bioc.data$Month == "all", ]
        dat <- log.data[log.data$Year == current.yr, ]
        dat$date <- as.Date(paste0(dat$Year, "-01-01"))
      } else stop('"observation must be "month" or "year"')
    } else stop('when must be "last-year", "year-to-date" or "ytd".')

  } else {
    if (is.null(from) & is.null(to)) {
      if (observation == "month") {
        log.data <- bioc.data[bioc.data$Month != "all", ]
        month.num <- vapply(log.data$Month, function(x) {
          which(x == month.abb)
        }, integer(1L))
        month <- ifelse(nchar(month.num) == 1, paste0(0, month.num), month.num)
        log.data$date <- as.Date(paste0(log.data$Year, "-", month, "-01"))
        dat <- log.data
      } else if (observation == "year") {
        dat <- bioc.data[bioc.data$Month == "all", ]
        dat$date <- as.Date(paste0(dat$Year, "-01-01"))
      } else stop('"observation must be "month" or "year"')

    } else if (all(c(from, to) %in% 2009:current.yr)) {
      if (observation == "month") {
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
      } else if (observation == "year") {
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
      stop(msg1, msg2)
    }
  }
  row.names(dat) <- NULL
  if (is.null(packages) == FALSE) dat$packages <- packages
  dat <- dat[order(dat$date), ]
  dat[dat$date <= current.date, ]
}

bioc_plot <- function(x, graphics, count, points, smooth, smooth.f,
  log_count, obs.in.progress) {

  obs <- x$observation

  if (count == "download") {
    y.var <- "Nb_of_downloads"
    y.lab <- "Downloads"
  } else if (count == "ip") {
    y.var <- "Nb_of_distinct_IPs"
    y.lab <- "Unique IP Addresses"
  }

  if (obs == "month") {
    x.var <- "date"
  } else if (obs == "year") {
    x.var <- "Year"
  }

  invisible(lapply(x$data, function(dat) {
    if (obs == "month") {
      if (log_count) {
        plot(dat[, x.var], dat[, y.var], type = "l", xlab = "Year",
          ylab = paste0("log10(", y.lab, ")"), log = "y")
      } else {
        plot(dat[, x.var], dat[, y.var], type = "l", xlab = "Year",
          ylab = y.lab)
      }

      if (points) {
        if (obs.in.progress) {
          points(dat[1:(nrow(dat) - 1), "date"], dat[1:(nrow(dat) - 1), y.var],
            pch = 1)
        } else points(dat$date, dat[, y.var])
      }

      if (obs.in.progress) {
        points(dat[nrow(dat), "date"], dat[nrow(dat), y.var], pch = 15,
          col = "red")
      }

      if (smooth) {
        lines(stats::lowess(dat$date, dat[, y.var], f = smooth.f), col = "blue")
      }

     } else if (obs == "year") {
       if (log_count) {
         plot(dat$Year, dat[, y.var], type = "l", xlab = "Year",
           ylab = paste0("log10(", y.lab, ")"), log = "y")
       } else {
         plot(dat$Year, dat[, y.var], type = "l", xlab = "Year", ylab = y.lab)
       }

       if (points) {
         if (obs.in.progress) {
           points(dat[1:(nrow(dat) - 1), "Year"], dat[1:(nrow(dat) - 1), y.var],
             pch = 1)
         } else points(dat$Year, dat[, y.var])
       }

       if (obs.in.progress) {
         points(dat[nrow(dat), "Year"], dat[nrow(dat), y.var], pch = 15,
           col = "red")
       }
     }

    if (is.null(dat$packages)) {
       title(main = "All Packages")
     } else {
       title(main = unique(dat$packages))
     }
  }))
}

gg_bioc_plot <- function(x, graphics, count, points, smooth, smooth.f, se,
  log_count, obs.in.progress) {

  obs <- x$observation
  date <- x$date
  dat <- summary(x)
  oip <- rev(unique(dat$date))[1]

  mo <- vapply(dat$Month, function(mo) which(mo == month.abb), numeric(1L))
  dat$date <- as.Date(paste0(dat$Year, "-", mo, "-01"))

  if (count == "download") {
    p <- ggplot(data = dat, aes_string("date", "Nb_of_downloads")) +
         ylab("Downloads")
  } else if (count == "ip") {
    p <- ggplot(data = dat, aes_string("date", "Nb_of_distinct_IPs")) +
         ylab("Unique IP Addresses")
  }

  p <- p + geom_line(size = 0.5) + facet_wrap(~ packages, ncol = 2) +
    xlab("Date") + theme_bw() + theme(panel.grid.minor = element_blank())

  if (points & log_count & smooth) {
    p <- p + geom_point(data = dat[!dat$date %in% oip, ]) + scale_y_log10() +
      geom_smooth(method = "loess", se = se)
  } else if (points & log_count & !smooth) {
    p <- p + geom_point(data = dat[!dat$date %in% oip, ]) + scale_y_log10()
  } else if (points & !log_count & smooth) {
    p <- p +  geom_point(data = dat[!dat$date %in% oip, ]) +
      geom_smooth(method = "loess", se = se)
  } else if (!points & log_count & smooth) {
    p <- p + scale_y_log10() + geom_smooth(method = "loess", se = se)
  } else if (!points & !log_count & smooth) {
    p <- p + geom_smooth(method = "loess", se = se)
  } else if (points & !log_count & !smooth) {
    p <- p + geom_point(data = dat[!dat$date %in% oip, ])
  } else if (!points & log_count & !smooth) {
    p + scale_y_log10()
  }

  if (obs.in.progress) {
    p + geom_point(data = dat[dat$date %in% oip, ], color = "red", shape = 15)
  } else p
}

checkDate <- function(string, end.date = FALSE) {
  if (!is.character(string)) stop("date must a character string.")
  if (nchar(string) != 7 | (grepl("-", string) == FALSE)) {
    stop('Format must be "yyyy-mm".')
  } else {
    date.parts <- unlist(strsplit(string, "-"))
    if (date.parts[2] %in% c(paste0(0, 1:9), paste(10:12)) == FALSE) {
      stop("Month must be between 01 and 12.")
    }
    if (date.parts[1] < 2009) {
      warning(paste0('Bioconductor logs begin ', "January 2009", "."))
    }
  }

  date.candidate <- as.Date(paste0(string, "-01"), optional = TRUE)
  if (is.na(date.candidate)) stop("No such date.")
  else if (date.candidate > Sys.Date()) stop("Date in future!")
  else date.candidate
}
