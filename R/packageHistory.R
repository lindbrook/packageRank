#' Extract package or R version history.
#'
#' Date and version of all publications.
#' @param package Character. Vector of package names (including "R").
#' @param check.package Logical. Validate and "spell check" package.
#' @export

packageHistory <- function(package = "cholera", check.package = TRUE) {
  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)

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
    names(r_v) <- "R"
    
    package <- package[-r.position]
  }

  if (length(package) == 0) {
    out <- r_v
  } else {
    if (check.package) package <- checkPackage(package)
    
    # problem with pkgsearch::cran_package_history()
    cran <- mpackages_on_CRAN()
    on.cran <- package %in% cran$Package

    # Use packageHistory0() for "missing" and latest packages.
    # e.g., "VR" in cran_package() but not cran_package_history()
    # history <- try(lapply(package, pkgsearch::cran_package_history),
    #   silent = TRUE)
    
    if (any(!on.cran)) {
      archive.out <- lapply(package[!on.cran], packageHistory0)
    }
      
    if (any(on.cran)) {
       history <- lapply(package[on.cran], function(x) {
         pkgsearch::cran_package_history(x)
       })
      
      cran.out <- transform_pkgsearch(history)
    }
    
    if (exists("cran.out") & exists("archive.out")) {
      out <- c(cran.out, archive.out)
    } else if (exists("cran.out") & !exists("archive.out")) {
      out <- cran.out
    } else if (!exists("cran.out") & exists("archive.out")) {
      out <- archive.out
    }
    
    if (length(out) > 1) {
      pkg.nm <- vapply(out, function(x) x[1, "Package"], character(1L))
      id <- vapply(pkg.nm, function(x) which(package0 == x), numeric(1L))
      out <- out[order(id)]
      names(out) <- vapply(out, function(x) x[1, "Package"], character(1L))
    }
    
    if ("R" %in% package0) {
      out <- c(out[seq_along(out) < r.position], r_v,
               out[seq_along(out) >= r.position])
    }
  }
  
  if (length(out) == 1) out[[1]]
  else out
}

transform_pkgsearch <- function(history) {
  vars <- c("Package", "Version", "date", "Repository")
  lapply(history, function(x) {
    if ("Repository" %in% colnames(x)) {
      tmp <- data.frame(x[, vars])
      row.names(tmp) <- NULL
      tmp$Date <- as.Date(tmp$date)
      tmp$date <- NULL
      if (nrow(tmp) > 1) tmp[-nrow(tmp), "Repository"] <- "Archive"
      tmp <- tmp[, c("Package", "Version", "Date", "Repository")]
    } else {
      tmp <- data.frame(x[, c("Package", "Version", "date")])
      row.names(tmp) <- NULL
      tmp$Date <- as.Date(tmp$date)
      tmp$date <- NULL
      tmp$Repository <- "Archive"
    }
    tmp
  }) 
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

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
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

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
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
