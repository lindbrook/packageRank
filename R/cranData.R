#' Scrape CRAN package information.
#'
#' Current version, date and size (source).
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @return An R data frame.
#' @export

cranData <- function(multi.core = TRUE) {
  cores <- multiCore(multi.core)
  url <- "https://cran.r-project.org/src/contrib/"
  web_page <- readLines(url)
  pkg.id <- grepString("compressed.gif", web_page)
  pkg.data <- gsub("<.*?>", "", web_page[pkg.id])
  cran <- parallel::mclapply(seq_along(pkg.data), function(i) {
    dat <- unlist(strsplit(pkg.data[i], '.tar.gz'))
    ptA <- unlist(strsplit(dat[1], "_"))
    ptB <- unlist(strsplit(dat[2], " "))
    data.frame(package = ptA[1],
               version = ptA[2],
               date = as.Date(ptB[1]),
               size = unlist(strsplit(ptB[length(ptB)], "&nbsp;")),
               repository = "CRAN",
               stringsAsFactors = FALSE)
  }, mc.cores = cores)
  do.call(rbind, cran)
}
