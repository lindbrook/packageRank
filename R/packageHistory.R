#' Extract package version history CRAN and Archive.
#'
#' Date and version of all publications.
#' @param package Character. Package name.
#' @param check.package Logical. Validate and "spell check" package.
#' @export

packageHistory <- function(package = "cholera", check.package = TRUE) {
  if (check.package) package <- checkPackage(package)

  # Use packageHistory0() for "missing" and latest packages.
  # e.g.,"VR" in cran_package() but not cran_package_history()
  history <- try(pkgsearch::cran_package_history(package), silent = TRUE)

  if (any(class(history) == "try-error")) {
    out <- packageHistory0(package)
  } else {
    # vars <- c("Package", "Version", "Date/Publication", "crandb_file_date",
    #   "date")
    #    Error: Can't subset columns that don't exist.
    # x Column `Date/Publication` doesn't exist.
    vars <- c("Package", "Version", "crandb_file_date", "date")
    history <- data.frame(history[, vars])
    all.archive <- pkgsearch::cran_package(package, "all")$archived

    if (all.archive) {
      repository <- rep("Archive", nrow(history))
    } else {
      repository <- c(rep("Archive", nrow(history) - 1), "CRAN")
    }

    date <- strsplit(history$crandb_file_date, "[ ]")
    date <- strsplit(history$date, "[ ]")
    date <- as.Date(vapply(date, function(x) x[1], character(1L)))
    out <- data.frame(history[, c("Package", "Version")], Date = date,
      Repository = repository, stringsAsFactors = FALSE)
  }

  out
}

#' Scrape package version history CRAN and Archive.
#'
#' History of version, date and size (source file).
#' @param package Character. Package name.
#' @param size Logical. Include size of source file.
#' @noRd

packageHistory0 <- function(package = "cholera", size = FALSE) {
  # set check.package = FALSE to pass "latest" packages.
  cran <- packageCRAN(package, check.package = FALSE, size = size)
  arch <- packageArchive(package, check.package = FALSE, size = size)
  if (any(is.na(cran))) cran <- NULL
  out <- rbind(arch, cran)
  row.names(out) <- NULL
  out
}

#' Scrape package data from CRAN.
#'
#' Version, date and size (source file) of most recent publication.
#' @param package Character. Package name.
#' @param check.package Logical. Validate and "spell check" package.
#' @param size Logical. Include size of source file.
#' @return An R data frame or NULL.
#' @examples
#' \dontrun{
#' packageCRAN(package = "HistData")
#' packageCRAN(package = "VR") # No version on CRAN (archived)
#' }
#' @export

packageCRAN <- function(package = "cholera", check.package = TRUE,
  size = FALSE) {

  if (check.package) package <- checkPackage(package)
  url <- "https://cran.r-project.org/src/contrib/"
  web_page <- mreadLines(url)
  pkg.match <- grepl(package, web_page, fixed = TRUE)

  if (any(pkg.match)) {
    pkg.data <- gsub("<.*?>", "", web_page[pkg.match])
    if (length(pkg.data) > 1) {
      multiple.matches <- unname(vapply(pkg.data, function(x) {
        unlist(strsplit(x[1], "_"))[1]
      }, character(1L)))

      if (package %in% multiple.matches) {
        pkg.data <- pkg.data[multiple.matches %in% package]
        out <- package_info(pkg.data)
      } else out <- NULL
    } else if (length(pkg.data) == 1) out <- package_info(pkg.data)

    if (!is.null(out)) {
      if (identical(out$package, package)) {
        if (size) out
        else out[, names(out) != "size"]
      }
    } else NA
  } else NA
}

#' Scrape package data from Archive.
#'
#' @param package Character. Package name.
#' @param check.package Logical. Validate and "spell check" package.
#' @param size Logical. Include size of source file.
#' @return An R data frame or NULL.
#' @export
#' @examples
#' \dontrun{
#' packageArchive(package = "HistData")
#' packageArchive(package = "adjustedcranlogs")  # No archived versions.
#' }

packageArchive <- function(package = "cholera", check.package = TRUE,
  size = FALSE) {

  if (check.package) package <- checkPackage(package)
  root.url <- "https://cran.r-project.org/src/contrib/Archive/"
  url <- paste0(root.url, package)

  if (RCurl::url.exists(url)) {
    web_page <- mreadLines(url)
    ancestry.check <- grepString("Ancestry", web_page) # 'Rmosek'

    if (any(ancestry.check)) {
      ancestry.url <- paste0(url, "/", "Ancestry")
      ancestry_page <- mreadLines(ancestry.url)
      ancestry.id <- grepString("</td><td align=\"right\">", ancestry_page)
      line.id <- which(ancestry.id)[-1]

      ancestry.data <- lapply(line.id, function(i) {
        pkg.data <- gsub("<.*?>", "", ancestry_page[i])
        package_info(pkg.data, repository = "Ancestry")
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

      # testthat_2.3.1.tar.gz.save
      # CRAN fixed 10 May 2019
      # filename.err <- vapply(archive.data, function(x) {
      #   grepl(".save", x)
      # }, logical(1L))
      # if (any(filename.err)) archive.data <- archive.data[!filename.err]

      if (any(ancestry.check2)) {
        version.date <- lapply(archive.data[!ancestry.check2], function(x) {
          unlist(strsplit(x, '.tar.gz'))
        })
      } else {
        version.date <- lapply(archive.data, function(x) {
          unlist(strsplit(x, '.tar.gz'))
        })
      }

      version.date <- lapply(version.date, function(x) {
        ptB <- unlist(strsplit(x[2], " "))
        data.frame(version = unlist(strsplit(x[1], "_"))[2],
                   date = as.Date(ptB[1]),
                   size = unlist(strsplit(ptB[length(ptB)], "&nbsp;")),
                   stringsAsFactors = FALSE)
      })

      # exception for readme (e.g.,'sdcTable')
      readme <- vapply(version.date, function(x) all(is.na(x)), logical(1L))
      if (any(readme)) version.date <- version.date[!readme]

      out <- data.frame(package, do.call(rbind, version.date),
        repository = "Archive", stringsAsFactors = FALSE)
    }

    if (any(ancestry.check)) out <- rbind(ancestry.data, out)
    out <- out[order(out$date), ]
    if (size) out
    else out[, names(out) != "size"]
  }
}

grepString <- function(string, dat, reg.exp = FALSE) {
  if (reg.exp) string <- paste0("\\<", string, "\\>")
  vapply(seq_along(dat), function(i) {
    grepl(string, dat[i])
  }, logical(1L))
}

package_info <- function(pkg.data, repository = "CRAN") {
  dat <- unlist(strsplit(pkg.data, '.tar.gz'))
  ptA <- unlist(strsplit(dat[1], "_"))
  ptB <- unlist(strsplit(dat[2], " "))
  data.frame(package = ptA[1],
             version = ptA[2],
             date = as.Date(ptB[1]),
             size = unlist(strsplit(ptB[length(ptB)], "&nbsp;")),
             repository = "CRAN",
             stringsAsFactors = FALSE)
}
