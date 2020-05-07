#' Extract package version history CRAN and Archive.
#'
#' Date and version of all publications.
#' @param package Character. Package name.
#' @param short.date Logical
#' @export

packageHistory <- function(package = "cholera", short.date = TRUE) {
  vars <- c("Package", "Version", "Date/Publication", "crandb_file_date",
    "date")
  history <- pkgsearch::cran_package_history(package)[vars]
  history <- data.frame(history)

  all.archive <- pkgsearch::cran_package(package, "all")$archived

  if (all.archive) {
    repository <- rep("Archive", nrow(history))
  } else {
    repository <- c(rep("Archive", nrow(history) - 1), "CRAN")
  }

  if (short.date) {
    date <- strsplit(history$Date.Publication, "[ ]")
    date <- as.Date(vapply(date, function(x) x[1], character(1L)))
    data.frame(history[, c("Package", "Version")], Date = date,
      Repository = repository, stringsAsFactors = FALSE)
  } else {
    data.frame(history, Repository = repository, stringsAsFactors = FALSE)
  }
}

#' Extract package version history CRAN and Archive (scrape CRAN).
#'
#' Date and version of most recent publication.
#' @param package Character. Package name.
#' @export

packageHistory0 <- function(package = "cholera") {
  # "2008-02-16" first package
  if (any(is.na(packageCRAN(package)))) {
    cran <- NULL
  } else {
    cran <- packageCRAN(package)
  }
  out <- rbind(packageArchive(package), cran)
  row.names(out) <- NULL
  out
}

#' Extract package version history from CRAN.
#'
#' Date and version of most recent publication.
#' @param package Character. Package name.
#' @return An R data frame or NULL.
#' @examples
#' \donttest{
#' packageCRAN(package = "HistData")
#' packageCRAN(package = "VR") # No version on CRAN (archived)
#' }
#' @export

packageCRAN <- function(package = "cholera") {
  root.url <- "https://CRAN.R-project.org/package"
  url <- paste0(root.url, "=", package)

  if (RCurl::url.exists(url)) {
    web_page <- readLines(url)

    # 'SoilHyP'
    removed <- any(vapply(seq_along(web_page), function(i) {
      grepl("removed from the CRAN repository", web_page[i])
    }, logical(1L)))

    if (!removed) {
      published.check <- grepString("Published:", web_page)
      version.check <- grepString("<td>Version:</td>", web_page)

      if (any(published.check)) {
        line.id <- which(published.check) + 1
        published <- gsubClean(web_page, line.id)
      } else published <- NA

      if (any(version.check)) {
        line.id <- which(version.check) + 1
        version <- gsubClean(web_page, line.id)
      } else version.date <- NA

      data.frame(package = package, version = version,
        date = as.Date(published), repository = "CRAN",
        stringsAsFactors = FALSE)
    } # else warning("Package removed from CRAN.")
  } # else warning("Package not on CRAN. Check spelling.")
}

#' Extract version history from Archive.
#'
#' @param package Character. Package name.
#' @return An R data frame or NULL.
#' @export
#' @examples
#' \donttest{
#' packageArchive(package = "HistData")
#' packageArchive(package = "adjustedcranlogs")  # No archived versions.
#' }

packageArchive <- function(package = "cholera") {
  root.url <- "https://cran.r-project.org/src/contrib/Archive/"
  url <- paste0(root.url, package)

  if (RCurl::url.exists(url)) {
    web_page <- readLines(url)
    ancestry.check <- grepString("Ancestry", web_page) # 'Rmosek'

    if (any(ancestry.check)) {
      ancestry.url <- paste0(url, "/", "Ancestry")
      ancestry_page <- readLines(ancestry.url)
      ancestry.id <- grepString("</td><td align=\"right\">", ancestry_page)
      line.id <- which(ancestry.id)[-1]

      ancestry.data <- lapply(line.id, function(i) {
        dat <- gsub("<.*?>", "", ancestry_page[i])
        dat <- unlist(strsplit(dat, '.tar.gz'))
        ptA <- unlist(strsplit(dat[1], "_"))
        ptB <- unlist(strsplit(dat[2], " "))
        data.frame(package = ptA[1],
                   version = ptA[2],
                   date = as.Date(ptB[1]),
                   size = unlist(strsplit(ptB[length(ptB)], "&nbsp;")),
                   repository = "Ancestry",
                   stringsAsFactors = FALSE)
      })

      ancestry.data <- do.call(rbind, ancestry.data)
    }

    archive.check <- grepString("</td><td align=\"right\">",  web_page)

    if (any(archive.check)) {
      line.id <- which(archive.check)[-1]
      archive.data <- lapply(line.id, function(i) {
        gsub("<.*?>", "", web_page[i])
      })

      ancestry.check2 <- grepString("Ancestry", archive.data, TRUE)

      if (any(ancestry.check2)) {
        version.date <- lapply(archive.data[!ancestry.check2], function(x) {
          unlist(strsplit(x, '.tar.gz'))
        })
      } else {
        version.date <- lapply(archive.data, function(x) {
          unlist(strsplit(x, '.tar.gz'))
        })
      }

      version.date <-lapply(version.date, function(x) {
        ptB <- unlist(strsplit(x[2], " "))
        data.frame(version = unlist(strsplit(x[1], "_"))[2],
                   date = as.Date(ptB[1]),
                   size = unlist(strsplit(ptB[length(ptB)], "&nbsp;")),
                   stringsAsFactors = FALSE)
      })

      out <- data.frame(package, do.call(rbind, version.date),
        repository = "Archive", stringsAsFactors = FALSE)
    }

    if (any(ancestry.check)) {
      out <- rbind(ancestry.data, out)
    }

    out <- out[order(out$date), ]
    row.names(out) <- NULL
    out
  }
}

grepString <- function(string, dat, reg.exp = FALSE) {
  if (reg.exp) {
    string <- paste0("\\<", string, "\\>")
  }
  vapply(seq_along(dat), function(i) {
    grepl(string, dat[i])
  }, logical(1L))
}

gsubClean <- function(dat, id) {
  gsub("<.*?>", "", dat[id])
}
