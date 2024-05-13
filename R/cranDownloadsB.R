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
#' @param fix.cranlogs Logical. Use RStudio logs to fix 8 dates with duplicated data in 'cranlogs' results.
#' @noRd

cranDownloadsB <- function(packages = NULL, when = NULL, from = NULL, to = NULL,
  fix.cranlogs = TRUE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)
 
  if (length(packages) > 1) {
    if ("R" %in% packages) {
      stop("R downloads cannot be mixed with package downloads.", call. = FALSE)
    }
  }

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
    if (!is.null(to)) {
      end.date <- resolveDate(to, type = "to")
    } else {
      end.date <- logDate()
    }
    if (start.date > end.date) {
      stop('"from" must be <= "to".', call. = FALSE)
    }
    argmnts <- list(packages = packages, from = start.date, to = end.date)
  } else if (is.null(when) & !is.null(to)) {
    end.date <- resolveDate(to, type = "to")
    if (is.null(from)) {
      stop('You must also provide a date for "from".', call. = FALSE)
    } else {
      start.date <- resolveDate(from, type = "from") 
    }
    argmnts <- list(packages = packages, from = start.date, to = end.date)
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
  } 

  if ("R" %in% packages) if (fix.cranlogs) out <- fixRCranlogs(out)
  else if (fix.cranlogs) out <- fixCranlogs(out)
  
  class(out) <- "cranDownloads"
  out
}