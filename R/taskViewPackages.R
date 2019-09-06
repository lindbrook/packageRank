#' Extract CRAN Task View packages
#'
#' @param url Character. Task view URL.
#' @param core.packages Logical. TRUE for core packages; FALSE for all packages.
#' @return A character vector of package names.
#' @export
#' @examples
#' \donttest{
#' # Bayesian Inference
#' url <- "https://CRAN.R-project.org/view=Bayesian"
#' taskViewPackages(url)
#'
#' # Chemometrics and Computational Physics
#' url <- "https://CRAN.R-project.org/view=ChemPhys"
#' taskViewPackages(url, core = FALSE)
#' }

taskViewPackages <- function(url = url, core.packages = TRUE) {
  web_page <- readLines(url)

  start <- which(vapply(seq_along(web_page), function(i) {
    grepl("CRAN packages:", web_page[i])
  }, logical(1L))) + 2

  stop <- which(vapply(seq_along(web_page), function(i) {
    grepl("Related links:", web_page[i])
  }, logical(1L))) - 3

  pkg.urls <- web_page[start:stop]

  if (core.packages) {
    id <- which(vapply(seq_along(pkg.urls), function(i) {
      grepl("core", pkg.urls[i] )
    }, logical(1L)))

    packages <- lapply(seq_along(pkg.urls[id]), function(i) {
      unlist(strsplit(pkg.urls[i], '[/]'))[3]
    })
  } else {
    packages <- lapply(pkg.urls, function(x) unlist(strsplit(x, '[/]'))[3])
  }

  unlist(packages)
}

#' Extract CRAN Task View Names and URLs
#'
#' @return R data frame
#' @export

taskViewNameURL <- function() {
  web_page <- readLines("https://cran.r-project.org/web/views/")

  start <- which(vapply(seq_along(web_page), function(i) {
    grepl("Topics", web_page[i])
  }, logical(1L))) + 2

  dat <- web_page[start:length(web_page)]

  id <- vapply(seq_along(dat), function(i) {
    grepl("<td>", dat[i])
  }, logical(1L))

  dat <- dat[id]
  even <- seq_along(dat) %% 2 == 0
  odd <- seq_along(dat) %% 2 == 1

  topic <- unlist(lapply(dat[even], function(x) {
    tmp <- unlist(strsplit(x, "<td>"))[2]
    unlist(strsplit(tmp, "</td>"))
  }))

  short.topic <- unlist(lapply(dat[odd], function(x) {
    tmp <- unlist(strsplit(x, "\">"))[2]
    unlist(strsplit(tmp, "</a></td>"))
  }))

  url <- unlist(lapply(short.topic, function(topic) {
    paste0("https://CRAN.R-project.org/view=", topic)
  }))

  data.frame(name = short.topic, url = url, Task.View = topic,
    stringsAsFactors = FALSE)
}

#' List of CRAN Task View Package Names
#'
#' @param core.packages Logical. TRUE for core packages; FALSE for all packages.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @note Documents \code{taskview.package.names}.
#' @export

taskViewPackageNames <- function(core.packages = FALSE, multi.core = TRUE) {
  tasks <- taskViewNameURL()

  pkgs <- parallel::mclapply(tasks$url, taskViewPackages,
    core.packages = core.packages, mc.cores = multiCore(multi.core))

  stats::setNames(pkgs, tasks$name)
  # usethis::use_data(taskview.package.names)
}

#' Get CRAN Task View data.
#' @param task.view Character. Short CRAN Task View.
#' @param when \code{last-day}, \code{last-week} or \code{last-month}.
#'   If this is given, then \code{from} and \code{to} are ignored.
#' @param from Start date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
#' @param to End date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.#'
#' @export
#' @examples
#' \donttest{
#' taskViewCountData("Bayesian")
#' }

taskViewCountData <- function(task.view, when = NULL, from = "2019-01",
  to = NULL) {
  nm <- packageRank::taskview.package.names
  task.lst <- lapply(nm[[task.view]], function(x) {
    dat <- packageRank::cranDownloads(x, when = when, from = from, to = to)
    dat$cranlogs.data
  })
  task.data <- do.call(rbind, task.lst)
  list(dat = tapply(task.data$count, task.data$date, sum),
    task.view = task.view)
}
