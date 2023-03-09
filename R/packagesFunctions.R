#' Packages currently on CRAN.
#'
#' Package name, version and date of publication.
#' @return An R data frame.
#' @export

packages_on_CRAN <- function() {
  pkg.data <- tools::CRAN_package_db()
  vars <- c("Package", "Version", "Published")
  pkg.data[!duplicated(pkg.data$Package), vars] # N.B. duplicated()!
}

mpackages_on_CRAN <- memoise::memoise(packages_on_CRAN)

#' Packages currently in Archive
#'
#' Include inactive (retired) packages and previous versions of active packages.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R vector.
#' @export

packages_in_Archive <- function(multi.core = TRUE) {
  cores <- multiCore(multi.core)
  root.url <- "https://cran.r-project.org/src/contrib/Archive/"
  web_page <- mreadLines(root.url)
  start <- which(grepString("PARENTDIR", web_page)) + 1
  stop <- which(grepString("<address>", web_page)) - 3
  unlist(parallel::mclapply(start:stop, function(i) {
    tmp <- gsub("<.*?>", "", web_page[i])
    unlist(strsplit(tmp, "/"))[1]
  }, mc.cores = cores))
}

#' Packages observed in download logs.
#'
#' @param date Character. Date. "yyyy-mm-dd". NULL uses latest available log.
#' @return An R vector.
#' @export

packages_observed_in_logs <- function(date = NULL) {
  file.url.date <- logDate(date)
  cran_log <- fetchCranLog(date = file.url.date, memoization = TRUE)
  cran_log <- cleanLog(cran_log) # remove NAs and size 0 byte downloads
  sort(unique(cran_log$package))
}

#' Partitioned CRAN and Archive Packages.
#'
#' CRAN, Archive, Observed, CRAN & Archive, CRAN only and Archive only.
#' @param observed.downloads Logical. Compute current observed package downloads.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R list.
#' @export

packages_partitioned <- function(observed.downloads = FALSE, 
  multi.core = TRUE) {
  
  cores <- multiCore(multi.core)
  
  cran <- packages_on_CRAN()
  archive <- packages_in_Archive(multi.core = cores)
  cran.archive <- intersect(cran$Package, archive)
  cran.only <- setdiff(cran$Package, archive)
  archive.only <- setdiff(archive, cran$Package)
  
  if (observed.downloads) {
    obs <- packages_observed_in_logs(date = NULL)
    out <- list(cran = cran, archive = archive, obs = obs, 
                cran.archive = cran.archive, cran.only = cran.only, 
                archive.only = archive.only)  
  } else {
    out <- list(cran = cran, archive = archive, cran.archive = cran.archive,
                cran.only = cran.only, archive.only = archive.only)
  }
  out
}