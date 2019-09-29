#' CRAN statistics
#'
#' @param date Character. Date YYYY-MM-DD.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

cranStatistics <- function(date = Sys.Date() - 1,
  memoization = TRUE) {
    ymd <- as.Date(date)
    if (ymd > Sys.Date()) stop("Can't see into the future!")
    year <- as.POSIXlt(ymd)$year + 1900
    rstudio.url <- "http://cran-logs.rstudio.com/"
    url <- paste0(rstudio.url, year, '/', ymd, ".csv.gz")

    if (RCurl::url.exists(url)) {
      if (memoization) cran_log <- mfetchLog(url)
      else cran_log <- fetchLog(url)
    } else {
      msg <- "Check your internet connection or try the previous day."
      stop("Log for ", date, " not (yet) available. ", msg)
    }

    # NA for Package or R
    cran_log <- cran_log[!is.na(cran_log$package), ]

    # package audit
    # system.time(cran.pkgs0 <- cranPackages()) # 13.635
    # system.time(cran.pkgs1 <- availablePackages()) # 8.849

    cran.pkgs <- cranPackages()
    archive.pkgs <- archivePackages()
    rstudio.log.pkgs <- unique(cran_log$package)

    cran.archive <- intersect(cran.pkgs, archive.pkgs)
    cran.no_archive <- setdiff(cran.pkgs, archive.pkgs)
    archive.no_cran <- setdiff(archive.pkgs, cran.pkgs)

    # Available on CRAN but not in RStudio log = zero downloads (few?)
    cran.zero_downloads <- setdiff(cran.pkgs, rstudio.log.pkgs)
    # 2019-09-23: 'spate'
    # 2019-09-24: 'stationaRy'
    # 2019-09-26: 'timeperiodsR'

    # In RStudio log but not available on CRAN = archived
    # 224 @ "2019-09-23"
    # 409 @ "2019-09-24"
    # 2186 @ "2019-09-26"
    archive.zero_downloads <- sort(setdiff(rstudio.log.pkgs, cran.pkgs))
    # any(archive.downloads %in% cran.pkgs)
    # any(vapply(archive.audit, packageInArchive, logical(1L)))

    cran.zero.archive <- cran.zero_downloads[cran.zero_downloads %in%
      cran.archive]
    cran.zero.no_archive <- cran.zero_downloads[cran.zero_downloads %in%
      cran.no_archive]


    out <- list(cran.no_archive = cran.no_archive,
                cran.archive = cran.archive,
                archive.no_cran = archive.no_cran,
                # cran.zero_downloads = cran.zero_downloads,
                cran.zero.archive = cran.zero.archive,
                cran.zero.no_archive = cran.zero.no_archive,
                archive.zero_downloads = archive.zero_downloads)
    class(out) <- "cran_statistics"
    out
}

#' Summary method for cranStatistics().
#'
#' Return summary package counts.
#' @param object Object. An object of class "cran_statistics" created by \code{cranStatistics()}.
#' @param ... Additional parameters.
#' @return An R vector.
#' @export

summary.cran_statistics <- function(object, ...) {
  vapply(object, length, numeric(1L))
}
