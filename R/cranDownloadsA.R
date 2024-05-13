#' Daily package downloads from the RStudio CRAN mirror.
#'
#' Enhanced implementation of cranlogs::cran_downloads().
#' @param packages A character vector, the packages to query,
#'   or \code{NULL} for a sum of downloads for all packages.
#'   Alternatively, it can also be \code{"R"}, to query downloads
#'   of R itself. \code{"R"} cannot be mixed with packages.
#' @param when \code{last-day}, \code{last-week} or \code{last-month}.
#'   If this is given, then \code{from} and \code{to} are ignored.
#' @param from Start date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
#' @param to End date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.
#' @param check.package Logical. Validate and "spell check" package.
#' @param dev.mode Logical. Use validatePackage0() to scrape CRAN.
#' @param fix.cranlogs Logical. Use RStudio logs to fix 8 dates with duplicated data in 'cranlogs' results.
#' @noRd

cranDownloadsA <- function(packages = NULL, when = NULL, from = NULL,
  to = NULL, check.package = TRUE, dev.mode = FALSE, fix.cranlogs = TRUE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
  
  utc.date.time <- utc()
  utc.date <- as.Date(format(utc.date.time, "%Y-%m-%d"))
  upload.utc <- dateTime(utc.date, time = "17:00", tz = "GMT")
  today.log <- as.Date(format(upload.utc, "%Y-%m-%d")) - 1
  clogs <- try(cranlogs::cran_downloads(from = today.log, to = today.log),
    silent = TRUE)

  if (any(class(clogs) == "try-error")) {
    stop("'cranlogs' service not available.", call. = FALSE)
  }

  if (length(packages) > 1) {
    if ("R" %in% packages) {
      stop("R downloads cannot be mixed with package downloads.", call. = FALSE)
    }
  }

  first.log <- as.Date("2012-10-01") # first RStudio CRAN mirror log.
  # first.rlog <- as.Date("2014-05-23") # first non-problematic candidate
  first.r_log <- as.Date("2015-01-01") # match 'cranlogs'

  if (!is.null(packages)) {
    if (!"R" %in% packages) {
      if (check.package) packages <- checkPackage(packages)
      
      first.published <- do.call(c, lapply(packages, function(pkg) {
        packageHistory(pkg, check.package = FALSE)[1, "Date"]
      }))
      
      # w/o checkPackage(), NAs represent missing/misspelled packages 
      pkg.not.found <- is.na(first.published)
      
      if (any(pkg.not.found)) {
        first.published <- first.published[!pkg.not.found]
        if (!check.package) {
          if (any(pkg.not.found)) {
            not.found <- packages[pkg.not.found]
            msg <- "Misspelled or not on CRAN/Archive: "
            message(paste(msg, paste(not.found, collapse = ", ")))
          }
        }
        packages <- packages[!pkg.not.found]
      } else if (all(pkg.not.found)) {
        stop("All packages misspelled or not on CRAN/Archive.", call. = FALSE)
      }
      
      if (any(first.published < first.log)) {
        first.published[first.published < first.log] <- first.log
      }
    } else {
      r.history <- packageHistory("R", check.package = check.package)
      first.published <- r.history[1, "Date"]
      if (first.r_log >= first.published) first.published <- first.r_log
    }
  } else first.published <- first.log
  
  available.log <- logDate(warning.msg = FALSE, fix.date = FALSE)

  if (is.null(when) & is.null(from) & is.null(to)) {
    argmnts <- list(packages = packages, when = "last-day")
  } else if (!is.null(when) & is.null(from) & is.null(to)) {
    if (when %in% c("last-day", "last-week", "last-month")) {
      argmnts <- list(packages = packages, when = when)
    } else {
      stop('"when" must be "last-day", "last-week" or "last-month".',
        call. = FALSE)
      }
  } else if (is.null(when) & !is.null(from)) {
    start.date <- resolveDate(from, type = "from")
    if ("R" %in% packages) {
      if (start.date < first.r_log) {
        message("Logs for R download begin ", first.r_log, ".")
      }
    }
    if (!is.null(to)) {
      end.date <- resolveDate(to, type = "to")
      end.date.test <- first.published > end.date
      if (any(end.date.test)) {
        drop.pkgs <- paste(packages[end.date.test], collapse = ", ")
        message("Note: ", drop.pkgs, " not published by selected end date.")
        packages <- packages[!end.date.test]
        first.published <- first.published[!end.date.test]
      }
    } else {
      if (clogs$count != 0) {
        end.date <- available.log
      } else {
        end.date <- clogs$date - 1
        message("Today's 'cranlogs' results not available. Using previous.")
      }
    }
    if (start.date > end.date) stop('"from" must be <= "to".', call. = FALSE)
    argmnts <- list(packages = packages, from = start.date, to = end.date)
  } else if (is.null(when) & !is.null(to)) {
    end.date <- resolveDate(to, type = "to")
    if (is.null(packages)) {
      to.data <- cranlogs::cran_downloads(NULL, from = first.published,
        to = end.date)
    } else {
      end.date.test <- first.published > end.date
      if (any(end.date.test)) {
        drop.pkgs <- paste(packages[end.date.test], collapse = ", ")
        message("Note: ", drop.pkgs, " not published by selected end date.")
        packages <- packages[!end.date.test]
        first.published <- first.published[!end.date.test]
      }
      to.data <- lapply(seq_along(packages), function(i) {
        cranlogs::cran_downloads(packages[i], from = first.published[i],
          to = end.date)
      })
      if ("R" %in% packages) {
        if (end.date < first.r_log) {
          stop("Logs for R download begin ", first.r_log, ".", call. = FALSE)
        }
      }
    }
  }

  if (exists("argmnts")) {
    cranlogs.data <- do.call(cranlogs::cran_downloads, argmnts)
    if (is.null(argmnts$packages)) {
      cranlogs.data$cumulative <- cumsum(cranlogs.data$count)
    } else if ("R" %in% argmnts$packages) {
      cranlogs.data <- cranlogs.data[cranlogs.data$os != "NA", ]
      count <- tapply(cranlogs.data$count, list(cranlogs.data$date,
        cranlogs.data$os), sum)
      cumulative <- apply(count, 2, cumsum)
      dts <- rep(as.Date(row.names(count)), ncol(count))
      plt <- rep(colnames(count), each = nrow(count))
      cranlogs.data <- data.frame(date = dts, count = c(count),
        cumulative = c(cumulative), platform = plt, row.names = NULL)
    } else {
      if (any(duplicated(packages))) {
        grp <- seq_along(packages)
        cranlogs.data$grp <- rep(grp, each = length(unique(cranlogs.data$date)))
        cumulative <- unlist(lapply(grp, function(g) {
          cumsum(cranlogs.data[cranlogs.data$grp == g, "count"])
        }))
      } else {
        cumulative <- unlist(lapply(packages, function(pkg) {
          cumsum(cranlogs.data[cranlogs.data$package == pkg, "count"])
        }))
      }
      cranlogs.data <- cbind(cranlogs.data[, c("date", "count")],
        cumulative, cranlogs.data$package)
      sel <- (ncol(cranlogs.data) - 1):ncol(cranlogs.data)
      names(cranlogs.data)[sel] <- c("cumulative", "package")
    }
    out <- list(packages = packages, cranlogs.data = cranlogs.data,
      when = argmnts$when, from = argmnts$from, to = argmnts$to)
  } else {
    if (inherits(to.data, "list")) cranlogs.data <- do.call(rbind, to.data)
    else if (inherits(to.data, "data.frame")) cranlogs.data <- to.data

    if ("R" %in% packages) {
      cranlogs.data <- cranlogs.data[cranlogs.data$os != "NA", ]
      count <- tapply(cranlogs.data$count, list(cranlogs.data$date,
        cranlogs.data$os), sum)
      cumulative <- apply(count, 2, cumsum)
      dts <- rep(as.Date(row.names(count)), ncol(count))
      plt <- rep(colnames(count), each = nrow(count))
      cranlogs.data <- data.frame(date = dts, count = c(count),
        cumulative = c(cumulative), platform = plt, row.names = NULL)
    } else {
      if (is.null(packages)) {
        cranlogs.data$cumulative <- cumsum(cranlogs.data$count)
      } else {
        cumulative <- unlist(lapply(packages, function(p) {
          cumsum(cranlogs.data[cranlogs.data$package == p, "count"])
        }))
        cranlogs.data <- cbind(cranlogs.data[, c("date", "count")], cumulative,
          cranlogs.data$package)
        names(cranlogs.data)[ncol(cranlogs.data)] <- "package"
      }
    }

    id <- which.min(first.published)
    out <- list(packages = packages, cranlogs.data = cranlogs.data,
      when = NULL, from = first.published[id], to = end.date)
  }

  if (!is.null(packages)) {
    if (all(packages != "R")) {
      if (check.package) {
        out$cranlogs.data <- packageLifeFilter(out, packages, first.published) 
      }
    }
  }

  if ("R" %in% packages) {
    if (fix.cranlogs) out <- fixRCranlogs(out)
  } else {
    if (fix.cranlogs) out <- fixCranlogs(out)
  }
  class(out) <- "cranDownloads"
  out
}

