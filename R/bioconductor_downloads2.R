#' Annual/monthly package downloads from Bioconductor (prototype).
#'
#' @param year Numeric.
#' @param end.year Numeric.
#' @param pkg Character.
#' @export
#' @examples
#' bio_yr(year = 2015)
#' bio_yr(pkg = "clusterProfiler")

bio_yr <- function(year = NULL, end.year = NULL, pkg = NULL) {
  cal.date <- Sys.Date()
  current.yr <- data.table::year(cal.date)

  if (is.null(pkg)) {
    url <- "https://bioconductor.org/packages/stats/bioc/bioc_stats.tab"
  } else {
    url <- paste0("https://bioconductor.org/packages/stats/bioc/", pkg, "/",
      pkg, "_stats.tab", collapse = "")
  }

  pkg.data <- as.data.frame(mfetchLog(url))

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
}

#' Monthly package downloads from Bioconductor (prototype).
#'
#' @param year Numeric.
#' @param month Numeric.
#' @param end.year Numeric.
#' @param end.month Numeric.
#' @param pkg Character.
#' @export
#' @examples
#' bio_mo(year = 2015)
#' bio_mo(year = 2016, pkg = "clusterProfiler")

bio_mo <- function(year = NULL, month = NULL, end.year = NULL,
  end.month = NULL, pkg = NULL) {

  cal.date <- Sys.Date()
  current.yr <- data.table::year(cal.date)

  if (is.null(pkg)) {
    url <- "https://bioconductor.org/packages/stats/bioc/bioc_stats.tab"
  } else {
    url <- paste0("https://bioconductor.org/packages/stats/bioc/", pkg, "/",
      pkg, "_stats.tab", collapse = "")
  }

  pkg.data <- as.data.frame(mfetchLog(url))
  pkg.data <- pkg.data[pkg.data$Month != "all", ]

  if (is.null(year) & is.null(end.year) &
      is.null(month) & is.null(end.month)) {
    dat <- pkg.data

  } else if (!is.null(year) & is.null(end.year) &
      is.null(month) & is.null(end.month)) {
    dat <- pkg.data[pkg.data$Year == year, ]

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
