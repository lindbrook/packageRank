#' Annual/monthly package downloads from Bioconductor (prototype).
#'
#' @param year Numeric.
#' @param month Numeric.
#' @param end.year Numeric.
#' @param end.month Numeric.
#' @param pkg Character.
#' @param observation Character. "year" or "month"
#' @export

bioconductor_downloads2 <- function(year = NULL, month = NULL, end.year = NULL,
  end.month = NULL, pkg = NULL, observation = "year") {

  cal.date <- Sys.Date()
  current.yr <- data.table::year(cal.date)

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
    dat

  } else if (observation == "month") {
    pkg.data <- pkg.data[pkg.data$Month != "all", ]

    if (is.null(year) & is.null(end.year) &
        is.null(month) & is.null(end.month)) {
      dat <- pkg.data

    } else if (!is.null(year) & is.null(end.year) &
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
                !is.null(month) & !is.null(end.month)) {
       sel <- pkg.data$Year == year &
              pkg.data$Month %in% month.abb[month:12] |
              pkg.data$Year == end.year &
              pkg.data$Month %in% month.abb[1:end.month]
       dat <- pkg.data[sel, ]

    } else stop('error.')

    dat <- dat[order(dat$Year), ]
    row.names(dat) <- NULL
    if (is.null(pkg) == FALSE) dat$pkg <- pkg
    dat
  }

  out <- list(obs = observation, data = dat, pkg = pkg)
  class(out) <- "bioconductor2"
  out
}

#' Plot method for bioconductor_downloads().
#'
#' @param x object.
#' @param count Character. "download" or "ip".
#' @param smooth Logical. Add stats::lowess smoother.
#' @param ... Additional plotting parameters.
#' @export

plot.bioconductor2 <- function(x, count = "download", smooth = TRUE, ...) {
  if (x$obs == "year") {
    if (count == "download") {
      plot(x$data$Year, x$data$Nb_of_downloads, type = "o", xlab = "Year",
        ylab = "Downloads")
      if (smooth) {
        lines(stats::lowess(x$data$Year, x$data$Nb_of_downloads), col = "red")
      }
    } else if (count == "ip") {
      plot(x$data$Year, x$data$Nb_of_distinct_IPs, type = "o", xlab = "Year",
        ylab = "Unique IP Addresses")
      if (smooth) {
        lines(stats::lowess(x$data$Year, x$data$Nb_of_distinct_IPs),
          col = "red")
      }
    }
  } else if (x$obs == "month") {
    mo <- vapply(x$data$Month, function(mo) which(mo == month.abb), numeric(1L))
    x$data$date <- as.Date(paste0(x$data$Year, "-", mo, "-01"))

    if (count == "download") {
      plot(x$data$date, x$data$Nb_of_downloads, type = "o", xlab = "Year",
        ylab = "Downloads")
      if (smooth) {
        lines(stats::lowess(x$data$date, x$data$Nb_of_downloads), col = "red")
      }
    } else if (count == "ip") {
      plot(x$data$date, x$data$Nb_of_distinct_IPs, type = "o", xlab = "Year",
        ylab = "Unique IP Addresses")
      if (smooth) {
        lines(stats::lowess(x$data$date, x$data$Nb_of_distinct_IPs),
          col = "red")
      }
    }
  } else stop('count must be "download" or "ip".')

  title(main = x$pkg)
}

#' @export
print.bioconductor2 <- function(x, ...) print(x$data)

#' @export
summary.bioconductor2 <- function(object, ...) object$data
