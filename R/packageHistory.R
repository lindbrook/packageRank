#' Extract package or R version history.
#'
#' Date and version of all publications.
#' @param package Character. Vector of package names (including "R").
#' @param check.package Logical. Validate and "spell check" package.
#' @export

packageHistory <- function(package = "cholera", check.package = TRUE) {
  package0 <- package

  if ("R" %in% package) {
    pkg.idx <- seq_along(package)
    r.position <- which(package == "R")
    pkg.idx <- pkg.idx[pkg.idx != r.position]

    r_v <- rversions::r_versions()
    names(r_v) <- tools::toTitleCase(names(r_v))
    r_v$Date <- as.Date(r_v$Date)
    nms <- names(r_v)
    r_v$Package <- "R"
    r_v <- list(r_v[, c("Package", nms)])

    package <- package[-r.position]
  }

  if (check.package) package <- checkPackage(package)

  # Use packageHistory0() for "missing" and latest packages.
  # e.g.,"VR" in cran_package() but not cran_package_history()
  history <- try(lapply(package, pkgsearch::cran_package_history),
    silent = TRUE)

  if (any(class(history) == "try-error")) {
    out <- lapply(package, packageHistory0)
  } else {
    # vars <- c("Package", "Version", "Date/Publication", "crandb_file_date",
    #   "date")
    #    Error: Can't subset columns that don't exist.
    # x Column `Date/Publication` doesn't exist.
    vars <- c("Package", "Version", "crandb_file_date", "date")

    history <- lapply(history, function(x) data.frame(x[, vars]))

    all.archive <- vapply(package, function(x) {
      pkgsearch::cran_package(x, version = "all")$archived
    }, logical(1L))

    out <- lapply(seq_along(history), function(i) {
      h <- history[[i]]

      if (all.archive[i]) {
        repository <- rep("Archive", nrow(h))
      } else {
        repository <- c(rep("Archive", nrow(h) - 1), "CRAN")
      }

      date <- strsplit(h$crandb_file_date, "[ ]")
      date <- strsplit(h$date, "[ ]")
      date <- as.Date(vapply(date, function(x) x[1], character(1L)))
      data.frame(h[, c("Package", "Version")], Date = date,
        Repository = repository, row.names = NULL, stringsAsFactors = FALSE)
    })
  }

  if ("R" %in% package0) {
    out <- c(out[seq_along(out) < r.position], r_v,
      out[seq_along(out) >= r.position])
  }

  if (length(out) == 1) out[[1]]
  else out
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
      if (identical(out$Package, package)) {
        if (size) out
        else out[, names(out) != "Size"]
      }
    }
  }
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
        data.frame(Version = unlist(strsplit(x[1], "_"))[2],
                   Date = as.Date(ptB[1]),
                   Size = unlist(strsplit(ptB[length(ptB)], "&nbsp;")),
                   stringsAsFactors = FALSE)
      })

      # exception for readme (e.g.,'sdcTable')
      readme <- vapply(version.date, function(x) all(is.na(x)), logical(1L))
      if (any(readme)) version.date <- version.date[!readme]

      out <- data.frame(Package = package, do.call(rbind, version.date),
        Repository = "Archive", stringsAsFactors = FALSE)
    }

    if (any(ancestry.check)) out <- rbind(ancestry.data, out)
    out <- out[order(out$Date), ]
    if (size) out
    else out[, names(out) != "Size"]
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
  data.frame(Package = ptA[1],
             Version = ptA[2],
             Date = as.Date(ptB[1]),
             Size = unlist(strsplit(ptB[length(ptB)], "&nbsp;")),
             Repository = "CRAN",
             stringsAsFactors = FALSE)
}
