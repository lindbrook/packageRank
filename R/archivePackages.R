#' Packages in CRAN archive.
#'
#' Scrape https://cran.r-project.org/src/contrib/Archive/.
#' @param include.date Logical. Return data frame with package name and last publication date.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export

archivePackages <- function(include.date = FALSE, multi.core = TRUE,
  dev.mode = FALSE) {

  archive.url <- "https://cran.r-project.org/src/contrib/Archive/"
  web_page <- mreadLines(archive.url)
  cores <- multiCore(multi.core)

  if ((.Platform$OS.type == "windows" & cores > 1) | dev.mode) {
    cl <- parallel::makeCluster(cores)

    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = "web_page")

    archive.start <- parallel::parLapply(cl, web_page, function(x) {
      grepl("Parent Directory", x)
    })

    archive.stop <- parallel::parLapply(cl, web_page, function(x) {
      grepl("colspan=", x)
    })

    parallel::stopCluster(cl)

  } else {
    archive.start <- parallel::mclapply(web_page, function(x) {
      grepl("Parent Directory", x)
    }, mc.cores = cores)

    archive.stop <- parallel::mclapply(web_page, function(x) {
      grepl("colspan=", x)
    }, mc.cores = cores)
  }

  archive.start <- unlist(archive.start)
  archive.stop <- unlist(archive.stop)
  start <- which(archive.start) + 2
  stop <- which(archive.stop)[2] - 1

  web_page <- web_page[start:stop]

  # README file at directory root #

  if ((.Platform$OS.type == "windows" & cores > 1) | dev.mode) {
    cl <- parallel::makeCluster(cores)

    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = "web_page")

    README.test <- unlist(parallel::parLapply(cl, web_page, function(x) {
      grepl(paste0("\\<", "README", "\\>"), x)
    }))

    parallel::stopCluster(cl)

  } else {
    README.test <- unlist(parallel::mclapply(web_page, function(x) {
      grepl(paste0("\\<", "README", "\\>"), x)
    }, mc.cores = cores))
  }

  if (sum(README.test) == 1) {
    web_page <- web_page[!README.test]
  } else {
    stop("README.test error.")
  }

  if ((.Platform$OS.type == "windows" & cores > 1) | dev.mode) {
    cl <- parallel::makeCluster(cores)

    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = "web_page")

    pkg.parsed.lst <- parallel::parLapply(cl, web_page, function(x) {
      pkg.data <- gsub("<.*?>", "", x)
      unlist(strsplit(pkg.data, "/"))
    })

    pkgs <- unlist(parallel::parLapply(cl, pkg.parsed.lst, function(x) {
      unlist(strsplit(x, "/"))[1]
    }))

    parallel::stopCluster(cl)

  } else {
    pkg.parsed.lst <- parallel::mclapply(web_page, function(x) {
      pkg.data <- gsub("<.*?>", "", x)
      unlist(strsplit(pkg.data, "/"))
    }, mc.cores = cores)

    pkgs <- unlist(parallel::mclapply(pkg.parsed.lst, function(x) {
      unlist(strsplit(x, "/"))[1]
    }, mc.cores = cores))
  }

  ## Empty folders

  # 2019-03-02 13:09 time stamp: CRAN fixed on 2019-10-05
  empty.folder1 <- c( "Archived", "as", "check", "corrected", "despite",
    "issues", "on", "reminder.", "X-CRAN-Comment:", "were")

  # 2019-03-12 18:23 time stamp
  empty.folder2 <- "app"

  # 2020-01-11 14:37 time stamp
  empty.folder3 <- c("problems", "reminders.")

  # 2020-02-19 10:40 time stamp
  empty.folder4 <- c("2020-02-18", "in", "time.", "2015-09-01")

  # 2020-05-14 11:03; 2020-06-27 06:43; 2020-06-30 05:42; 2020-06-30 05:42;
  # 2020-05-14 11:03; 2020-04-20 12:08; 2020-05-14 11:03; 2020-05-14 11:03;
  # 2020-05-14 11:03; 2020-05-14 11:03; 2020-05-14 11:03; 2020-05-14 11:03;
  # 2020-05-14 11:03; 2020-06-30 05:42; 2020-06-30 05:42; 2020-05-14 11:03
  empty.folder5 <- c("and", "X-CRAN-History:", "ENmisc.", "archived", "been",
    "datasa", "depending", "given", "have", "it", "long", "notice.",
    "orphaned", "package", "requires", "those")

  empty <- c(empty.folder1, empty.folder2, empty.folder3, empty.folder4,
    empty.folder5)

  # 2020-01-11 14:37 time stamp
  # empties_2020.01.11 <- c("Archived", "as", "check", "corrected", "despite",
  #   "on", "were")

  if (any(empty %in% pkgs)) {
    pkgs <- pkgs[pkgs %in% empty == FALSE]
    pkg.parsed.lst <- pkg.parsed.lst[pkgs %in% empty == FALSE]
  }

  if (include.date) {
    out <- lapply(pkg.parsed.lst, function(x) {
      pkg.name <-x[1]
      date.data <- x[2]
      date <- unlist(strsplit(date.data, " "))[1]
      data.frame(package = pkg.name, date = date, stringsAsFactors = FALSE)
    })
    do.call(rbind, out)
  } else {
    pkgs
  }
}
