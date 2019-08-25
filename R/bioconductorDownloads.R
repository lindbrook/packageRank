#' Annual/monthly package downloads from Bioconductor (beta).
#'
#' @param packages Character. Vector of package names.
#' @param when \code{"last-year"}, or \code{"year-to-date"} or \code{"ytd"}.
#' @param from Start date as \code{yyyy-mm} or \code{yyyy}.
#' @param to End date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
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
#' # 2015 through 2018
#' bioconductorDownloads(packages = "clusterProfiler", from = 2010, to = 2015)
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
#' @param add.points Logical. Add points.
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
#' plot(bioconductorDownloads(packages = "graph", year = 2019))
#' plot(bioconductorDownloads(packages = "graph", year = 2014, end.year = 2018, 
#'   month = 6, end.month = 3))
#' }

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
      log.data <- bioc.data[bioc.data$Month == "all", ]
      if (!is.null(from) & is.null(to)) {
        dat <- log.data[log.data$Year >= from, ]
      } else if (is.null(from) & !is.null(to)) {
        dat <- log.data[log.data$Year <= to, ]
      } else if (!is.null(from) & !is.null(to)) {
        dat <- log.data[log.data$Year >= from & log.data$Year <= to, ]
      } else dat <- log.data
      dat$date <- as.Date(paste0(dat$Year, "-01-01"))

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
