#' Check for empty folders CRAN's archive.
#'
#' Empty folders are errors that do not represent real packages.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores
#' @export

emptyFolderAudit <- function(multi.core = TRUE) {
  url <- "https://cran.r-project.org/src/contrib/Archive/"
  web_page <- readLines(url)
  cores <- multiCore(multi.core)

  archive.start <- parallel::mclapply(web_page, function(x) {
    grepl("Parent Directory", x)
  }, mc.cores = cores)

  archive.stop <- parallel::mclapply(web_page, function(x) {
    grepl("colspan=", x)
  }, mc.cores = cores)

  archive.start <- unlist(archive.start)
  archive.stop <- unlist(archive.stop)
  start <- which(archive.start) + 2
  stop <- which(archive.stop)[2] - 1

  web_page <- web_page[start:stop]

  # README file at directory root #
  README.test <- unlist(parallel::mclapply(web_page, function(x) {
    grepl(paste0("\\<", "README", "\\>"), x)
  }, mc.cores = cores))

  if (sum(README.test) == 1) {
    web_page <- web_page[!README.test]
  } else stop("No README file.")

  pkg.parsed.lst <- parallel::mclapply(web_page, function(x) {
    pkg.data <- gsub("<.*?>", "", x)
    unlist(strsplit(pkg.data, "/"))
  }, mc.cores = cores)

  # 2019-03-02 13:09 time stamp: CRAN fixed on 2019-10-05
  empty.folder1 <- c( "Archived", "as", "check", "corrected", "despite",
    "issues", "on", "reminder.", "X-CRAN-Comment:", "were")

  # 2019-03-12 18:23 time stamp
  empty.folder2 <- "app"

  # 2020-01-11 14:37 time stamp
  empty.folder3 <- c("problems", "reminders.")

  empty <- c(empty.folder1, empty.folder2, empty.folder3)

  if (any(empty %in% pkgs)) {
    time.stamp <- lapply(pkg.parsed.lst[which(pkgs %in% empty)], function(x) {
      trimws(unlist(strsplit(x[2], "- &")))[1]
    })

    stamp <- unique(unlist(time.stamp))

    empty.id <- unlist(parallel::mclapply(pkg.parsed.lst, function(x) {
      trimws(unlist(strsplit(x[2], "- &")))[1] %in% stamp
    }, mc.cores = cores))

    pkgs <- unlist(parallel::mclapply(pkg.parsed.lst, function(x) {
      unlist(strsplit(x, "/"))[1]
    }, mc.cores = cores))

    list(pkgs = pkgs[empty.id], stamp = unique(unlist(time.stamp)))

  } else stop("Apparently, no empty folders.")
}

