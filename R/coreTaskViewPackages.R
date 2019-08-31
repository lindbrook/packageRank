#' Extract core CRAN Task View packages
#'
#' @param url Character. Task view URL.
#' @return A character vector of package names.
#' @export
#' @examples
#' # Bayesian Inference
#' url <- "https://cran.r-project.org/web/views/Bayesian.html"
#' coreTaskViewPackages(url)
#'
#' # Chemometrics and Computational Physics
#' url <- "https://cran.r-project.org/web/views/ChemPhys.html"
#' coreTaskViewPackages(url)

coreTaskViewPackages <- function(url = url) {
  web_page <- readLines(url)

  start <- which(vapply(seq_along(web_page), function(i) {
    grepl("CRAN packages:", web_page[i])
  }, logical(1L))) + 2

  stop <- which(vapply(seq_along(web_page), function(i) {
    grepl("Related links:", web_page[i])
  }, logical(1L))) - 3

  pkgs <- web_page[start:stop]

  id <- which(vapply(seq_along(pkgs), function(i) {
    grepl("core", pkgs[i] )
  }, logical(1L)))

  core.packages <- lapply(seq_along(pkgs[id]), function(i) {
    unlist(strsplit(pkgs[i], '[/]'))[3]
  })

  unlist(core.packages)
}

#' Extractct Task View Names and URLs
#'
#' @return R data frame
#' @export

topicDataFrame <- function() {
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

  data.frame(short.topic = short.topic, url = url, topic = topic, stringsAsFactors = FALSE)
}

#' Get taskview data.
#' @param taskview Character. Short task view name
#' @export
taskviewData <- function(taskview) {
  task.lst <- lapply(packageRank::taskview.pkgs[[taskview]], function(x) {
    packageRank::cranDownloads(x, from = "2019-01")$cranlogs.data
  })
  task.data <- do.call(rbind, task.lst)
  list(dat = tapply(task.data$count, task.data$date, sum), taskview = taskview)
}

#' Plot taskview data.
#' @param taskview Character. Short task view name
#' @export
taskviewPlot <- function(taskview) {
  task.data <- taskviewData(taskview)
  dat <- task.data[["dat"]]
  plot(as.Date(names(dat)), dat, type = "l")
  lines(stats::lowess(as.Date(names(dat)), dat), col = "red")
  title(main =  task.data[["taskview"]])
}

# Documents taskview.pkgs.
taskViewPackages <- function() {
  taskview.data <- topicDataFrame()
  taskview.pkgs <- lapply(taskview.data$url, coreTaskViewPackages)
  names(taskview.pkgs) <- taskview.data$short.topic
  # usethis::use_data(taskview.pkgs)
}
