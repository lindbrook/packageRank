#' Daily package downloads from the RStudio CRAN mirror.
#'
#' Enhanced implementation of cranlogs::cran_downloads().
#' @param packages A character vector, the packages to query,
#'   or \code{NULL} for a sum of downloads for all packages.
#'   Alternatively, it can also be \code{"R"}, to query downloads
#'   of R itself. \code{"R"} cannot be mixed with packages.
#' @param when \code{last-day}, \code{last-week} or \code{last-month}.
#'   If this is given, then \code{from} and \code{to} are ignored.
#' @param from Start date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
#' @param to End date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @param fix.cranlogs Logical. Use RStudio logs to fix 8 dates with duplicated data in 'cranlogs' results.
#' @export
#' @examples
#' \dontrun{
#' cranDownloads(packages = "HistData")
#' cranDownloads(packages = "HistData", when = "last-week")
#' cranDownloads(packages = "HistData", when = "last-month")
#'
#' # January 7 - 31, 2019
#' cranDownloads(packages = "HistData", from = "2019-01-07", to = "2019-01-31")
#'
#' # February through March 2019
#' cranDownloads(packages = "HistData", from = "2019-02", to = "2019-03")
#'
#' # 2020 year-to-date
#' cranDownloads(packages = "HistData", from = 2020)
#' }

cranDownloads <- function(packages = NULL, when = NULL, from = NULL,
  to = NULL, check.package = TRUE, dev.mode = FALSE, fix.cranlogs = TRUE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  
  utc.date.time <- utc()
  utc.date <- as.Date(format(utc.date.time, "%Y-%m-%d"))
  upload.utc <- dateTime(utc.date, time = "17:00", tz = "GMT")
  today.log <- as.Date(format(upload.utc, "%Y-%m-%d")) - 1
  clogs <- try(cranlogs::cran_downloads(from = today.log, to = today.log),
    silent = TRUE)

  if (any(class(clogs) == "try-error")) {
    stop("'cranlogs' service not available.", call. = FALSE)
  }

  if (length(packages) > 1) {
    if ("R" %in% packages) {
      stop("R downloads cannot be mixed with package downloads.", call. = FALSE)
    }
  }

  first.log <- as.Date("2012-10-01") # first RStudio CRAN mirror log.
  # first.rlog <- as.Date("2014-05-23") # first non-problematic candidate
  first.r_log <- as.Date("2015-01-01") # match 'cranlogs'

  if (!is.null(packages)) {
    if (!"R" %in% packages) {
      if (check.package) packages <- checkPackage(packages, dev.mode)
      first.published <- do.call(c, lapply(packages, function(pkg) {
        packageHistory(pkg)[1, "Date"]
      }))
      if (any(first.published < first.log)) {
        first.published[first.published < first.log] <- first.log
      }
    } else {
      first.published <- packageHistory("R")[1, "Date"]
      if (first.r_log >= first.published) first.published <- first.r_log
    }
  } else first.published <- first.log
  
  available.log <- logDate(warning.msg = FALSE, fix.date = FALSE)

  if (is.null(when) & is.null(from) & is.null(to)) {
    argmnts <- list(packages = packages, when = "last-day")
  } else if (!is.null(when) & is.null(from) & is.null(to)) {
    if (when %in% c("last-day", "last-week", "last-month")) {
      argmnts <- list(packages = packages, when = when)
    } else {
      stop('"when" must be "last-day", "last-week" or "last-month".',
        call. = FALSE)
      }
  } else if (is.null(when) & !is.null(from)) {
    start.date <- resolveDate(from, type = "from")
    if ("R" %in% packages) {
      if (start.date < first.r_log) {
        message("Logs for R download begin ", first.r_log, ".")
      }
    }
    if (!is.null(to)) {
      end.date <- resolveDate(to, type = "to")
      end.date.test <- first.published > end.date
      if (any(end.date.test)) {
        drop.pkgs <- paste(packages[end.date.test], collapse = ", ")
        message("Note: ", drop.pkgs, " not published by selected end date.")
        packages <- packages[!end.date.test]
        first.published <- first.published[!end.date.test]
      }
    } else end.date <- available.log
    if (start.date > end.date) stop('"from" must be <= "to".', call. = FALSE)
    argmnts <- list(packages = packages, from = start.date, to = end.date)
  } else if (is.null(when) & !is.null(to)) {
    end.date <- resolveDate(to, type = "to")
    if (is.null(packages)) {
      to.data <- cranlogs::cran_downloads(NULL, from = first.published,
        to = end.date)
    } else {
      end.date.test <- first.published > end.date
      if (any(end.date.test)) {
        drop.pkgs <- paste(packages[end.date.test], collapse = ", ")
        message("Note: ", drop.pkgs, " not published by selected end date.")
        packages <- packages[!end.date.test]
        first.published <- first.published[!end.date.test]
      }
      to.data <- lapply(seq_along(packages), function(i) {
        cranlogs::cran_downloads(packages[i], from = first.published[i],
          to = end.date)
      })
      if ("R" %in% packages) {
        if (end.date < first.r_log) {
          stop("Logs for R download begin ", first.r_log, ".", call. = FALSE)
        }
      }
    }
  }

  if (exists("argmnts")) {
    cranlogs.data <- do.call(cranlogs::cran_downloads, argmnts)
    if (is.null(argmnts$packages)) {
      cranlogs.data$cumulative <- cumsum(cranlogs.data$count)
    } else if ("R" %in% argmnts$packages) {
      cranlogs.data <- cranlogs.data[cranlogs.data$os != "NA", ]
      count <- tapply(cranlogs.data$count, list(cranlogs.data$date,
        cranlogs.data$os), sum)
      cumulative <- apply(count, 2, cumsum)
      dts <- rep(as.Date(row.names(count)), ncol(count))
      plt <- rep(colnames(count), each = nrow(count))
      cranlogs.data <- data.frame(date = dts, count = c(count),
        cumulative = c(cumulative), platform = plt, row.names = NULL)
    } else {
      cumulative <- unlist(lapply(packages, function(pkg) {
        cumsum(cranlogs.data[cranlogs.data$package == pkg, "count"])
      }))
      cranlogs.data <- cbind(cranlogs.data[, c("date", "count")],
        cumulative, cranlogs.data$package)
      sel <- (ncol(cranlogs.data) - 1):ncol(cranlogs.data)
      names(cranlogs.data)[sel] <- c("cumulative", "package")
    }
    out <- list(packages = packages, cranlogs.data = cranlogs.data,
      when = argmnts$when, from = argmnts$from, to = argmnts$to)
  } else {
    if (inherits(to.data, "list")) cranlogs.data <- do.call(rbind, to.data)
    else if (inherits(to.data, "data.frame")) cranlogs.data <- to.data

    if ("R" %in% packages) {
      cranlogs.data <- cranlogs.data[cranlogs.data$os != "NA", ]
      count <- tapply(cranlogs.data$count, list(cranlogs.data$date,
        cranlogs.data$os), sum)
      cumulative <- apply(count, 2, cumsum)
      dts <- rep(as.Date(row.names(count)), ncol(count))
      plt <- rep(colnames(count), each = nrow(count))
      cranlogs.data <- data.frame(date = dts, count = c(count),
        cumulative = c(cumulative), platform = plt, row.names = NULL)
    } else {
      if (is.null(packages)) {
        cranlogs.data$cumulative <- cumsum(cranlogs.data$count)
      } else {
        cumulative <- unlist(lapply(packages, function(p) {
          cumsum(cranlogs.data[cranlogs.data$package == p, "count"])
        }))
        cranlogs.data <- cbind(cranlogs.data[, c("date", "count")], cumulative,
          cranlogs.data$package)
        names(cranlogs.data)[ncol(cranlogs.data)] <- "package"
      }
    }

    id <- which.min(first.published)
    out <- list(packages = packages, cranlogs.data = cranlogs.data,
      when = NULL, from = first.published[id], to = end.date)
  }

  if (!is.null(packages)) {
    if (all(packages != "R")) {
      if (check.package) {
        out$cranlogs.data <- packageLifeFilter(out, packages, first.published) 
      }
    }
  }

  if ("R" %in% packages) {
    if (fix.cranlogs) out <- fixRCranlogs(out)
  } else {
    if (fix.cranlogs) out <- fixCranlogs(out)
  }
  class(out) <- "cranDownloads"
  out
}

#' Plot method for cranDownloads().
#'
#' @param x object.
#' @param statistic Character. "count" or "cumulative".
#' @param graphics Character. "auto", "base" or "ggplot2".
#' @param points Character of Logical. Plot points. "auto", TRUE, FALSE.
#' @param log.y Logical. Logarithm of package downloads.
#' @param smooth Logical. Add smoother.
#' @param se Logical. Works only with graphics = "ggplot2".
#' @param f Numeric. smoother window for stats::lowess(). For graphics = "base" only; c.f. stats::lowess(f)
#' @param span Numeric. Smoothing parameter for geom_smooth(); c.f. stats::loess(span).
#' @param package.version Logical. Add latest package release dates.
#' @param r.version Logical. Add R release dates.
#' @param population.plot Logical. Plot population plot.
#' @param population.seed Numeric. Seed for sample in population plot.
#' @param multi.plot Logical.
#' @param same.xy Logical. Use same scale for multiple packages when graphics = "base".
#' @param legend.location Character.
#' @param ip.legend.location Character. Location of in-progress legend.
#' @param r.total Logical.
#' @param dev.mode Logical. Use packageHistory0() to scrape CRAN.
#' @param unit.observation Character. "year", "month", "week", or "day".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @param ... Additional plotting parameters.
#' @return A base R or ggplot2 plot.
#' @export
#' @examples
#' \dontrun{
#' plot(cranDownloads(packages = c("Rcpp", "rlang", "data.table")))
#' plot(cranDownloads(packages = c("Rcpp", "rlang", "data.table"), when = "last-month"))
#' plot(cranDownloads(packages = "R", from = "2020-01-01", to = "2020-01-01"))
#' plot(cranDownloads(packages = "R", from = 2020))
#' }

plot.cranDownloads <- function(x, statistic = "count", graphics = "auto",
  points = "auto", log.y = FALSE, smooth = FALSE, se = FALSE, f = 1/3,
  span = 3/4, package.version = FALSE, r.version = FALSE,
  population.plot = FALSE, population.seed = as.numeric(Sys.Date()),
  multi.plot = FALSE, same.xy = TRUE, legend.location = "topleft",
  ip.legend.location = "topright", r.total = FALSE, dev.mode = FALSE,
  unit.observation = "day", multi.core = TRUE, ...) {

  cores <- multiCore(multi.core)

  if (graphics == "auto") {
    if (is.null(x$packages)) {
      graphics <- "base"
    } else if (length(x$packages) == 1) {
      graphics <- "base"
    } else if (length(x$package) > 1) {
      graphics <- "ggplot2"
    }
  }

  if (!is.null(x$when)) {
    if (x$when == "last-week" & unit.observation != "day") {
      stop('With when = "last-week", only unit.observation = "day" available.',
        call. = FALSE)
    }
    if (x$when == "last-month" & unit.observation == "month" &
      graphics == "ggplot2") {
        msg1 <- 'With when = "last-month" and graphics = "ggplot2", only'
        msg2 <- 'unit.observation = "month" is not available.'
      stop(paste(msg1, msg2), call. = FALSE)
    }
  }

  if (is.logical(log.y) == FALSE) {
    stop("log.y must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.logical(smooth) == FALSE) {
    stop("smooth must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.logical(se) == FALSE) {
    stop("se must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.numeric(f) == FALSE) {
    stop("f must be numeric.", call. = FALSE)
  }
  if (statistic %in% c("count", "cumulative") == FALSE) {
    stop('"statistic" must be "count" or "cumulative".', call. = FALSE)
  }
  if (!graphics %in% c("base", "ggplot2")) {
    stop('graphics must be "base" or "ggplot2"', call. = FALSE)
  }

  if (unit.observation %in% c("week", "month", "year")) {
    if (is.null(x$packages)) {
      x$first.obs.date <- x$cranlogs.data[1, "date"]
    } else {
      if ("R" %in% x$packages) {
        x$first.obs.date <- x$cranlogs.data[1, "date"]
      } else {
        x$first.obs.date <- do.call(c, lapply(x$packages, function(pkg) {
          tmp <- x$cranlogs.data[x$cranlogs.data$package == pkg, ]
          tmp[1, "date"]
        }))
      }
    }

    x$last.obs.date <- x$cranlogs.data[nrow(x$cranlogs.data), "date"]

    if (!is.null(x$when)) {
      x$from <- x$cranlogs.data$date[1]
      x$to <- x$cranlogs.data$date[length(x$cranlogs.data$date)]
    }

    x$cranlogs.data <- aggregateData(x, unit.observation, cores)
  }

  obs.ct <- length(unique(x$cranlogs.data$date))

  if (points == "auto") {
    if (obs.ct <= 45) points <- TRUE else points <- FALSE
  } else if (is.logical(points) == FALSE) {
    stop('points must be "auto", TRUE, or FALSE.', call. = FALSE)
  }

  if (population.plot) {
     populationPlot(x, graphics = graphics, f = f, span = span,
       population.seed = population.seed)
  } else if ("R" %in% x$packages) {
    if (r.total) {
      rTotPlot(x, statistic, graphics, obs.ct, legend.location, points,
        log.y, smooth, se, r.version, f, span)
    } else {
      rPlot(x, statistic, graphics, obs.ct, legend.location,
        ip.legend.location, points, log.y, smooth, se, r.version, f, span,
        multi.plot)
    }
  } else if (is.null(x$packages)) {
    cranPlot(x, statistic, graphics, obs.ct, points, log.y, smooth, se, f,
      span, r.version)
  } else {
    if (multi.plot) {
      multiPlot(x, statistic, graphics, obs.ct, log.y, legend.location,
        ip.legend.location, points, smooth, se, f, span)
    } else {
      singlePlot(x, statistic, graphics, obs.ct, points, smooth, se, f,
        span, log.y, package.version, dev.mode, r.version, same.xy)
    }
  }
}

#' Print method for cranDownloads().
#' @param x object.
#' @param ... Additional parameters.
#' @export

print.cranDownloads <- function(x, ...) {
  print(x$cranlogs.data)
}

#' Summary method for cranDownloads().
#' @param object Object.
#' @param ... Additional parameters.
#' @export
#' @note This is useful for directly accessing the data frame.

summary.cranDownloads <- function(object, ...) {
  object$cranlogs.data
}
