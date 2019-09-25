#' Archive and zero downloads
#'
#' @param date Character. Date.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

archiveZeroDownloads <- function(date = Sys.Date() - 1,
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
    cran_log <- cran_log[-which(is.na(cran_log$package)), ]

    # package audit
    cran.pkgs <- cranPackages()
    rstudio.log.pkgs <- unique(cran_log$package)

    # Available on CRAN but not in RStudio log = zero downloads (few?)
    zero.downloads <- setdiff(cran.pkgs, rstudio.log.pkgs)
    # 2019-09-25: "spate"
    # 2019-09-25: "stationaRy"

    # In RStudio log but not available on CRAN = archived
    # 224 @ "2019-09-23"
    # 409 @ "2019-09-24"
    archive.downloads <- sort(setdiff(rstudio.log.pkgs, cran.pkgs))
    # any(archive.downloads %in% cran.pkgs)
    # any(vapply(archive.audit, packageInArchive, logical(1L)))

    list(zero.downloads = zero.downloads, archive.downloads = archive.downloads)
  }
