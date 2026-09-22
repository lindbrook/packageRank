#' Extract Package Logs.
#'
#' @param lst Object. List of logs.
#' @param i Numeric. Day/ID.
#' @param pkg Character.
#' @param clean.output Logical.
#' @noRd

pkgLog0 <- function(lst, i = 1, pkg = "cholera", clean.output = TRUE) {
  cran_log <- cleanLog(lst[[i]])
  tmp <- cran_log[cran_log$package == pkg, ]
  tmp$t2 <- dateTime(tmp$date, tmp$time)
  tmp <- tmp[order(tmp$t2), c(1:6, 8:10)]
  if (clean.output) row.names(tmp) <- NULL
  tmp
}

#' Extract Package Logs.
#'
#' @param lst Object. List of logs.
#' @param i Numeric. Day/ID.
#' @param ip.filter Logical.
#' @param ip.campaigns Logical.
#' @param small.filter Logical.
#' @param sequence.filter Logical.
#' @param pkg Character.
#' @param multi.core Logical or Numeric.
#' @param clean.output Logical.
#' @noRd

pkgLog <- function(lst, i = 1, ip.filter = TRUE, ip.campaigns = TRUE, 
  small.filter = TRUE, sequence.filter = TRUE, pkg = "cholera", 
  multi.core = FALSE, clean.output = TRUE) {

  cores <- multiCore(multi.core)
  cran_log <- cleanLog(lst[[i]])

  if (ip.filter) {
    row.delete <- ipFilter(cran_log, campaigns = ip.campaigns,
      multi.core = cores)
    cran_log <- cran_log[!row.names(cran_log) %in% row.delete, ]
  }

  tmp <- cran_log[cran_log$package == pkg, ]

  if (nrow(tmp) != 0) {
    if (small.filter) tmp <- smallFilter(tmp)
    if (sequence.filter) {
      pkg.history <- packageRank::blog.data$pkg.history
      p.hist <- pkg.history[[pkg]]
      p.date <- names(lst)[i]
      sel <- p.hist$Date <= as.Date(p.date) & p.hist$Repository == "Archive"
      arch.pkg.history <- p.hist[sel, ]
      tmp <- sequenceFilter(tmp, arch.pkg.history)
    }

    tmp$t2 <- dateTime(tmp$date, tmp$time)
    tmp <- tmp[order(tmp$t2), !names(tmp) %in% "t2"]
    if (clean.output) row.names(tmp) <- NULL
  }

  tmp
}

#' Package Filter Counts.
#'
#' @param lst Object. cran_log list of data frames.
#' @param pkg Character.
#' @param ip.campaigns Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @noRd

packageFilterCounts <- function(lst, pkg = "cholera", ip.campaigns = TRUE,
  multi.core = FALSE) {

  cores <- multiCore(multi.core)
  dates <- names(lst)

  out <- parallel::mclapply(seq_along(lst), function(i) {
    filter_counts(lst[[i]], pkg, dates[i], ip.campaigns)
  }, mc.cores = cores)

  versions <- parallel::mclapply(lst, function(x)  {
    x <- x[!is.na(x$package), ]
    unique(x[x$package == pkg, "version"])
  }, mc.cores = cores)

  versions <- length(unique(unlist(versions)))

  out <- list(data = do.call(rbind, out),
              versions = versions,
              pkg = pkg,
              dates = dates)

  class(out) <- "packageFilterCounts"
  out
}

#' Filter counts helper.
#'
#' @param dat Object. cran_log data frame.
#' @param pkg Character.
#' @param date Character.
#' @param ip.campaigns Logical.
#' @noRd

filter_counts <- function(dat, pkg = "cholera", date, ip.campaigns) {
  dat0 <- cleanLog(dat)
  dat <- dat0[dat0$package == pkg, ]

  if (nrow(dat) != 0) {
    # IP filter #
    row.delete <- ipFilter(dat0, campaigns = ip.campaigns, multi.core = FALSE)
    ip.filtered <- sum(!row.names(dat) %in% row.delete)
    out <- dat[!row.names(dat) %in% row.delete, ]


    # Small Filter #
    small.filtered <- nrow(smallFilter(dat))
    if (nrow(out) != 0) out <- smallFilter(out)

    # Sequence Filter #
    pkg.history <- packageRank::blog.data$pkg.history
    p.hist <- pkg.history[[pkg]]
    sel <- p.hist$Date <= as.Date(date) & p.hist$Repository == "Archive"
    arch.pkg.history <- p.hist[sel, ]

    pre.filter <- nrow(dat) - nrow(out)
    out <- sequenceFilter(out, arch.pkg.history)
    sequence.filtered <- nrow(out) + pre.filter

    # Output #
    data.frame(package = pkg, ct = nrow(dat), ip = ip.filtered, 
      small = small.filtered, sequence = sequence.filtered, all = nrow(out))

  } else {
    data.frame(package = pkg, ct = nrow(dat), ip = 0, small = 0, sequence = 0, 
      all = 0)
  }
}

#' CRAN Filter Counts.
#'
#' @param lst Object. cran_log list of data frames.
#' @param ip.campaigns Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @noRd

cranFilterCounts <- function(lst, ip.campaigns = TRUE, multi.core = FALSE) {
  cores <- multiCore(multi.core)
  out <- parallel::mclapply(lst, function(x) {
    cran_log <- cleanLog(x)
    u.ct <- length(unique(cran_log$package))

    row.delete <- ipFilter(cran_log, campaigns = ip.campaigns,
      multi.core = cores)
    tmp <- cran_log[!row.names(cran_log) %in% unlist(row.delete), ]
    ip.ct <- length(unique(tmp$package))

    sm.tmp <- smallFilter(cran_log)
    sm.ct <- length(unique(sm.tmp$package))

    tmp <- smallFilter(tmp)
    ip_sm.ct <- length(unique(tmp$package))

    data.frame(ct = u.ct, ip = ip.ct, small = sm.ct, all = ip_sm.ct)
  }, mc.cores = cores)

  dates <- as.Date(names(out))
  out <- do.call(rbind, out)
  out <- list(data = data.frame(date = dates, out, row.names = NULL))
  class(out) <- "cranFilterCounts"
  out
}

wordCase <- function(x) {
  # tools::toTitleCase("all")?
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}
