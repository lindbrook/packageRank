#' Annual/monthly package downloads from Bioconductor (prototype).
#'
#' @param year Numeric.
#' @param month Numeric.
#' @param end.year Numeric.
#' @param end.month Numeric.
#' @param pkg Character.
#' @param observation Character. "year" or "month"
#' @export

bioconductor_downloads2 <- function(pkg = NULL, year = NULL, month = NULL,
  end.year = NULL, end.month = NULL, observation = "month") {

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

  out <- list(obs = observation, data = dat, pkg = pkg, date = cal.date,
    year = current.yr)
  class(out) <- "bioconductor2"
  out
}

#' Plot method for bioconductor_downloads().
#'
#' @param x object.
#' @param count Character. "download" or "ip".
#' @param add.points Logical. Add points.
#' @param smooth Logical. Add stats::lowess smoother.
#' @param smooth.f Numeric. smoother span.
#' @param ... Additional plotting parameters.
#' @export
#' @examples
#' plot(bioconductor_downloads2(pkg = "graph"))
#' plot(bioconductor_downloads2(pkg = "graph", year = 2019))
#' plot(bioconductor_downloads2(pkg = "graph", year = 2014, end.year = 2018, month = 6, end.month = 3))

plot.bioconductor2 <- function(x, count = "download", add.points = TRUE,
  smooth = FALSE, smooth.f = 2/3, ...) {

  dat <- x$data

  if (x$obs == "year") {
    next.year <- as.Date(paste0(x$year + 1, "-01-01"))
    yr.in.progress <- ifelse(x$date < next.year, TRUE, FALSE)

    if (count == "download") {
      plot(dat$Year, dat$Nb_of_downloads, type = "l", xlab = "Year",
        ylab = "Downloads")

      if (add.points) {
        if (yr.in.progress) {
          points(dat[1:(nrow(dat) - 1), "Year"],
                 dat[1:(nrow(dat) - 1), "Nb_of_downloads"],
                 pch = 1)
          points(dat[nrow(dat), "Year"],
                 dat[nrow(dat), "Nb_of_downloads"],
                 pch = 15, col = "red")
        }
      }

      if (smooth) {
        lines(stats::lowess(dat$Year, dat$Nb_of_downloads, f = smooth.f),
          col = "blue")
      }
    } else if (count == "ip") {
      plot(dat$Year, dat$Nb_of_distinct_IPs, type = "l", xlab = "Year",
        ylab = "Unique IP Addresses")

      if (add.points) {
        if (yr.in.progress) {
          points(dat[1:(nrow(dat) - 1), "Year"],
                 dat[1:(nrow(dat) - 1), "Nb_of_distinct_IPs"],
                 pch = 1)
          points(dat[nrow(dat), "Year"],
                 dat[nrow(dat), "Nb_of_distinct_IPs"],
                 pch = 15, col = "red")
        }
      }

      if (smooth) {
        lines(stats::lowess(dat$Year, dat$Nb_of_distinct_IPs, f = smooth.f),
          col = "blue")
      }
    }
  } else if (x$obs == "month") {
    mo <- vapply(dat$Month, function(mo) which(mo == month.abb), numeric(1L))
    dat$date <- as.Date(paste0(dat$Year, "-", mo, "-01"))

    if ( any(dat$date < x$date) ) {
      dat <- dat[dat$date < x$date, ]
    }

    data.yr_mo <- extractYearMonth(dat[nrow(dat), "date"])
    current.yr_mo <- extractYearMonth(x$date)

    if (count == "download") {
      plot(dat$date, dat$Nb_of_downloads, type = "l", xlab = "Year",
        ylab = "Downloads")

      if (add.points) {
        if (identical(data.yr_mo, current.yr_mo)) {
          points(dat[1:(nrow(dat) - 1), "date"],
                 dat[1:(nrow(dat) - 1), "Nb_of_downloads"],
                 pch = 1)
          points(dat[nrow(dat), "date"],
                 dat[nrow(dat), "Nb_of_downloads"],
                 pch = 15, col = "red")
        } else points(dat[, c("date", "Nb_of_downloads")])
      }

      if (smooth) {
        lines(stats::lowess(dat$date, dat$Nb_of_downloads, f = smooth.f),
          col = "blue")
      }
    } else if (count == "ip") {
      plot(dat$date, dat$Nb_of_distinct_IPs, type = "l", xlab = "Year",
        ylab = "Unique IP Addresses")

      if (add.points) {
        if (identical(data.yr_mo, current.yr_mo)) {
          points(dat[1:(nrow(dat) - 1), "date"],
                 dat[1:(nrow(dat) - 1), "Nb_of_distinct_IPs"],
                 pch = 1)
          points(dat[nrow(dat), "date"],
                 dat[nrow(dat), "Nb_of_distinct_IPs"],
                 pch = 15, col = "red")
        } else points(dat[, c("date", "Nb_of_distinct_IPs")])
      }

      if (smooth) {
        lines(stats::lowess(dat$date, dat$Nb_of_distinct_IPs, f = smooth.f),
          col = "blue")
      }
    }
  } else stop('count must be "download" or "ip".')

  if (is.null(x$pkg)) title(main = "All Packages") else title(main = x$pkg)
}

#' @export
print.bioconductor2 <- function(x, ...) print(x$data)

#' @export
summary.bioconductor2 <- function(object, ...) object$data

extractYearMonth <- function(z) substr(z, 1, 7)

