#' Extract Package Logs.
#'
#' @param dat Object. List of logs.
#' @param i Numeric. Day/ID.
#' @param pkg Character.
#' @param clean.output Logical.
#' @export

pkgLog0 <- function(dat, i = 1, pkg = "cholera", clean.output = TRUE) {
  cran_log <- dat[[i]]
  tmp <- cran_log[!is.na(cran_log$package) & cran_log$package == pkg, ]
  tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "Europe/Vienna")
  tmp <- tmp[order(tmp$t2), c(1:6, 8:10)]
  if (clean.output) row.names(tmp) <- NULL
  tmp
}

#' Extract Package Logs.
#'
#' @param dat Object. List of logs.
#' @param i Numeric. Day/ID.
#' @param triplet.filter Logical.
#' @param ip.filter Logical.
#' @param campaigns Logical. For use with ip.filter.
#' @param small.filter Logical.
#' @param sequence.filter Logical.
#' @param pkg Character.
#' @param multi.core Logical or Numeric.
#' @param clean.output Logical.
#' @export

pkgLog <- function(dat, i = 1, triplet.filter = TRUE, ip.filter = TRUE,
  campaigns = TRUE, small.filter = TRUE, sequence.filter = TRUE,
  pkg = "cholera", multi.core = TRUE, clean.output = TRUE) {

  cores <- multiCore(multi.core)
  cran_log <- dat[[i]]
  tmp <- cran_log[!is.na(cran_log$package) & cran_log$package == pkg, ]

  if (triplet.filter) tmp <- do.call(rbind, tripletFilter(tmp))

  if (ip.filter) {
    ip.outliers <- ipFilter3(cran_log)
    if (campaigns) {
      row.delete <- unlist(parallel::mclapply(ip.outliers, function(x) {
        campaigns(x, cran_log)
      }, mc.cores = cores))
      tmp <- tmp[!row.names(tmp) %in% row.delete, ]
    } else {
      tmp <- tmp[!tmp$ip_id %in% ip.outliers, ]
    }
  }

  if (small.filter) {
    size.audit <- length(unique(round(log10(tmp$size))))
    if (size.audit > 1) tmp <- smallFilter0(tmp)
  }

  if (sequence.filter) tmp <- sequenceFilter(tmp)

  tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "Europe/Vienna")
  tmp <- tmp[order(tmp$t2), !names(tmp) %in% "t2"]
  if (clean.output) row.names(tmp) <- NULL
  tmp
}

#' Filter Counts.
#'
#' @param lst Object. cran_log list of data frames.
#' @param pkg Character.
#' @param ip.filter Character. "campaign" or "ip".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

filterCounts <- function(lst, pkg = "cholera", ip.filter = "campaign",
  multi.core = TRUE) {
  cores <- multiCore(multi.core)

  out <- parallel::mclapply(lst, function(x) {
    filter_counts(x, pkg = pkg, ip.filter = ip.filter)
  }, mc.cores = cores)

  out <- list(data = do.call(rbind, out), pkg = pkg)
  class(out) <- "filterCounts"
  out
}

#' Filter counts helper.
#'
#' @param dat Object. cran_log data frame.
#' @param pkg Character.
#' @param ip.filter Character. "campaign" or "ip".
#' @noRd

filter_counts <- function(dat, pkg = "cholera", ip.filter = "campaign") {
  dat0 <- dat[!is.na(dat$package), ]
  dat <- dat0[dat0$package == pkg,]

  if (nrow(dat) != 0) {
    # Triplet filter
    out <- do.call(rbind, tripletFilter(dat))
    triplet.filtered <- nrow(out)

    # IP filter
    ip.outliers <- ipFilter3(dat0)
    if (ip.filter == "campaign") {
      row.delete <- unlist(lapply(ip.outliers, function(x) campaigns(x, dat0)))
      ip.sel <- !row.names(out) %in% row.delete
      ip.filtered <- sum(ip.sel)
      out <- out[ip.sel, ]
    } else if (ip.filter == "ip") {
      ip.sel <- !dat$ip_id %in% ip.outliers
      ip.filtered <- sum(ip.sel)
      out <- out[ip.sel, ]
    }

    # Small Filter
    small.filtered <- nrow(smallFilter0(dat))
    if (nrow(out) != 0) out <- smallFilter0(out)

    # Sequence Filter
    sequence.filtered <- nrow(sequenceFilter(dat))
    out <- sequenceFilter(out)

    data.frame(package = pkg, ct = nrow(dat), triplet = triplet.filtered,
      ip = ip.filtered, small = small.filtered, sequence = sequence.filtered ,
      all = nrow(out))
  } else {
    data.frame(package = pkg, ct = nrow(dat), triplet = 0, ip = 0, small = 0,
      sequence = 0, all = 0)
  }
}

#' Plot method for filterCounts().
#'
#' @param x object.
#' @param filter Character. "triplet", "ip", "small", "sequence", "all".
#' @param smooth Logical.
#' @param ... Additional plotting parameters.
#' @export

plot.filterCounts <- function(x, filter = "all", smooth = FALSE, ...) {
  dat <- x$data
  dates <- as.Date(row.names(dat))
  wed.id <- which(weekdays(dates, abbreviate = TRUE) == "Wed")

  plot(as.Date(row.names(dat)), dat$ct, pch = NA, ylim = range(dat[, -1]),
    xlab = "Date", ylab = "Downloads")
  abline(v = dates[wed.id], col = "lightgray", lwd = 2/3)
  lines(dates, dat$ct, pch = 0, type = "o", col = "red",)
  lines(dates, dat[, filter], type = "o", pch = 16)
  axis(3, at = dates[wed.id], labels = rep("W", length(wed.id)), cex.axis = 2/3,
       mgp = c(3, 0.5, 0))

  if (filter == "ip") {
    title(main = paste(x$pkg, "with", toupper(filter), "Filter"))
  } else {
    title(main = paste(x$pkg, "with", wordCase(filter), "Filter"))
  }

  if (smooth) {
    lines(stats::lowess(dates, dat$ct), col = "red", lty = "dotted", lwd = 2)
    lines(stats::lowess(dates, dat[, filter]), lty = "dotted", lwd = 2)
  }
}

wordCase <- function(x) {
  # tools::toTitleCase("all")?
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}
