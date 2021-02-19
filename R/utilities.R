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
#' @param triplet.filter Logical.
#' @param ip.filter Logical.
#' @param ip.campaigns Logical.
#' @param small.filter Logical.
#' @param sequence.filter Logical.
#' @param pkg Character.
#' @param multi.core Logical or Numeric.
#' @param clean.output Logical.
#' @noRd

pkgLog <- function(lst, i = 1, triplet.filter = TRUE, ip.filter = TRUE,
  ip.campaigns = TRUE, small.filter = TRUE, sequence.filter = TRUE,
  pkg = "cholera", multi.core = TRUE, clean.output = TRUE) {

  cores <- multiCore(multi.core)
  cran_log <- cleanLog(lst[[i]])

  if (ip.filter) {
    row.delete <- ipFilter(cran_log, campaigns = ip.campaigns,
      multi.core = cores)
    cran_log <- cran_log[!row.names(cran_log) %in% row.delete, ]
  }

  tmp <- cran_log[cran_log$package == pkg, ]

  if (nrow(tmp) != 0) {
    if (triplet.filter) tmp <- tripletFilter(tmp)
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
  multi.core = TRUE) {

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

    # Triplet filter #
    out <- tripletFilter(dat)
    triplet.filtered <- nrow(out)

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
    data.frame(package = pkg, ct = nrow(dat), triplet = triplet.filtered,
      ip = ip.filtered, small = small.filtered, sequence = sequence.filtered,
      all = nrow(out))

  } else {
    data.frame(package = pkg, ct = nrow(dat), triplet = 0, ip = 0, small = 0,
      sequence = 0, all = 0)
  }
}

#' Plot method for packageFilterCounts().
#'
#' @param x object.
#' @param filter Character. "triplet", "ip", "small", "sequence", "all".
#' @param smooth Logical.
#' @param median Logical.
#' @param legend.loc Character. Location of legend.
#' @param ... Additional plotting parameters.
#' @noRd

plot.packageFilterCounts <- function(x, filter = "all", smooth = FALSE,
  median = FALSE, legend.loc = "topleft", ...) {

  dat <- x$data
  dates <- as.Date(x$dates)
  wed.id <- which(weekdays(dates, abbreviate = TRUE) == "Wed")

  plot(dates, dat$ct, pch = NA, ylim = range(dat[, -1]), xlab = "Date",
    ylab = "Downloads")
  abline(v = dates[wed.id], col = "gray", lwd = 2/3)
  lines(dates, dat$ct, pch = 15, type = "o", col = "red",)
  lines(dates, dat[, filter], type = "o", pch = 16)
  axis(3, at = dates[wed.id], labels = rep("W", length(wed.id)), cex.axis = 2/3,
    mgp = c(3, 0.5, 0))
  legend(x = legend.loc,
       legend = c("unfiltered", "filtered"),
       col = c("red", "black"),
       pch = c(15, 16),
       bg = "white",
       cex = 2/3,
       lwd = 1,
       title = NULL)

  if (filter == "ip") {
    title(main = paste0("'", x$pkg, "'", " ", toupper(filter), " Filter"))
  } else if (filter == "all") {
    title(main = paste0("'", x$pkg, "'", ": ", wordCase(filter), " Filters"))
  } else {
    title(main = paste0("'", x$pkg, "'", ": ", wordCase(filter), " Filter"))
  }

  if (smooth) {
    lines(stats::lowess(dates, dat$ct), col = "red", lty = "dotted", lwd = 2)
    lines(stats::lowess(dates, dat[, filter]), lty = "dotted", lwd = 2)
  }

  if (median) {
    axis(4, at = median(dat$ct), labels = median(dat$ct), col.axis = "red")
    axis(4, at = median(dat[, filter]), labels = median(dat[, filter]))
  }

  tot <- colSums(dat[, -1])
  ptA <- paste0("unfiltered = ", format(tot["ct"], big.mark = ","),
    "; filtered = ")
  ptB <- paste0("% | ", x$versions, " vers. observed")
  delta.pct <- round(100 * (tot["ct"] - tot[filter]) / tot[filter], 1)
  title(sub = paste0(ptA, format(tot[filter], big.mark = ","), "; inflation = ",
    format(delta.pct, big.mark = ","), ptB))
}

#' CRAN Filter Counts.
#'
#' @param lst Object. cran_log list of data frames.
#' @param ip.campaigns Logical.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @noRd

cranFilterCounts <- function(lst, ip.campaigns = TRUE, multi.core = TRUE) {
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

#' Plot method for cranFilterCounts().
#'
#' @param x object.
#' @param filter Character.  "ip", "small", "all".
#' @param smooth Logical.
#' @param median Logical.
#' @param legend.loc Character. Location of legend.
#' @param add.legend Logical.
#' @param ... Additional plotting parameters.
#' @noRd

plot.cranFilterCounts <- function(x, filter = "all", smooth = FALSE,
  median = FALSE, legend.loc = "topleft", add.legend = TRUE, ...) {

  c.data <- x$data
  mo <- c.data$date
  id <- which(weekdays(mo, abbreviate = TRUE) == "Wed")

  plot(mo, c.data$ct, type = "o", col = "red", pch = 15,
    ylim = range(c.data[, -1]), xlab = "Date", ylab = "Count")

  # lines(mo, c.data$f.ct, type = "o", col = "black", pch = 16, lwd = 2)
  lines(mo, c.data[, filter], type = "o", pch = 16)

  abline(v = mo[id], col = "gray", lty = "dotted")
  axis(3, at = mo[id], labels = rep("W", length(id)), cex.axis = 2/3,
    col.ticks = "black", mgp = c(3, 0.5, 0))
  # title(main = "Packages Downloaded")
  if (add.legend) {
    legend(x = legend.loc,
           legend = c("all", "filtered"),
           col = c("red", "black"),
           pch = c(15, 16),
           bg = "white",
           cex = 2/3,
           lwd = 1,
           title = NULL)
  }

   if (filter == "ip") {
     title(main = paste0(toupper(filter), " Filter"))
   } else if (filter == "all") {
     title(main = paste0(wordCase(filter), " Filters"))
   } else {
     title(main = paste0(wordCase(filter), " Filter"))
   }

   if (smooth) {
     lines(stats::lowess(mo, c.data$u.ct), col = "red", lty = "dotted", lwd = 2)
     lines(stats::lowess(mo, c.data[, filter]), lty = "dotted", lwd = 2)
   }

   if (median) {
     axis(4, at = median(c.data$ct), labels = median(c.data$ct),
       col.axis = "red")
     axis(4, at = median(c.data[, filter]), labels = median(c.data[, filter]))
   }

   tot <- colSums(c.data[, -1])
   ptA <- paste0("unfiltered = ", format(tot["ct"], big.mark = ","),
     "; filtered = ")
   # ptB <- paste0("% | ", x$versions, " vers. observed")
   delta.pct <- round(100 * (tot["ct"] - tot[filter]) / tot[filter], 1)
   title(sub = paste0(ptA, format(tot[filter], big.mark = ","),
     "; inflation = ", format(delta.pct, big.mark = ",")))
}

wordCase <- function(x) {
  # tools::toTitleCase("all")?
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}
