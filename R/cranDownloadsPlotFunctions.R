# Plot functions for plot.cranDownloads() #

cranPlot <- function(x, statistic, graphics, obs.ct, points, log.y, smooth,
  se, f, span, r.version) {

  dat <- x$cranlogs.data
  last.obs.date <- x$last.obs.date
  type <- ifelse(points, "o", "l")

  y.nm <- statistic
  y.var <- dat[, y.nm]
  st <- strsplit(statistic, " ")[[1]]
  y.nm.case <- paste(toupper(substring(st, 1, 1)), substring(st, 2), sep = "",
    collapse = " ")

  if (obs.ct == 1) {
    if (graphics == "base") {
      if (log.y) {
        dotchart(log10(dat$count), xlab = paste("log10", y.nm.case),
          main = paste("R Package Downloads:", unique(dat$date)))
      } else {
        dotchart(dat$count, xlab = "Count",
          main = paste("R Package Downloads:", unique(dat$date)))
      }
    } else if (graphics == "ggplot2") {
      dat$platform <-  ""
      if (log.y) {
        dat2 <- dat
        dat2$count <- log10(dat2$count)
        p <- ggplot(data = dat2, aes_string(x = "count", y = "platform")) +
          geom_point(size = 2) + xlab(paste("log10", y.nm.case)) + ylab(NULL)
      } else {
        p <- ggplot(data = dat, aes_string(x = "count", y = "platform")) +
          geom_point(size = 2) + ylab(NULL)
      }

      p + theme_bw() +
        ggtitle(paste("R Package Downloads:", unique(dat$date))) +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5))
    }

  } else if (obs.ct > 1) {
    if (graphics == "base") {
      if (any(dat$in.progress)) {
        ip.sel <- dat$in.progress == TRUE
        ip.data <- dat[ip.sel, ]
        complete <- dat[!ip.sel, ]
        last.obs <- nrow(complete)

        obs.days <- as.numeric(format(last.obs.date , "%d"))
        exp.days <- as.numeric(format(lastDayMonth(ip.data$date)$date, "%d"))
        est.ct <- round(ip.data$count * exp.days / obs.days)

        est.data <- ip.data
        est.data$count <- est.ct
        last.cumulative <- complete[nrow(complete), "cumulative"]
        est.data$cumulative <- last.cumulative + est.ct

        xlim <- range(dat$date)

        if (statistic == "count") {
          ylim <- range(c(dat[, y.nm], est.data$count))
        } else if (statistic == "cumulative") {
          ylim <- range(c(dat[, y.nm], est.data$cumulative))
        }

        if (log.y) {
          plot(complete$date, complete[, y.nm], type = type,
            xlab = "Date", ylab = paste0("log10 ", y.nm.case), xlim = xlim,
            ylim = ylim, log = "y", pch = 16)
        } else {
          plot(complete$date, complete[, y.nm], type = type,
            xlab = "Date", ylab = y.nm.case, xlim = xlim, ylim = ylim, pch = 16)
        }

        points(ip.data[, "date"], ip.data[, y.nm], col = "black", pch = 0)
        points(est.data[, "date"], est.data[, y.nm], col = "red", pch = 1)

        segments(complete[last.obs, "date"],
                 complete[last.obs, y.nm],
                 ip.data$date,
                 ip.data[, y.nm],
                 lty = "dotted")
        segments(complete[last.obs, "date"],
                 complete[last.obs, y.nm],
                 est.data$date,
                 est.data[, y.nm],
                 col = "red")

        axis(4, at = ip.data[, y.nm], labels = "obs")
        axis(4, at = est.data[, y.nm], labels = "est", col.axis = "red",
          col.ticks = "red")

      } else if (any(dat$partial)) { # unit.observation = "week"
        unit.date <- dat$date

        wk1.start <- dat$date[1]
        wk1.end <- dat$date[2] - 1
        wk1 <- cranDownloads(from = wk1.start, to = wk1.end)

        if (weekdays(x$from) == "Sunday") {
          wk1.partial <- dat[dat$date == wk1.start, ]
          wk1.backdate <- wk1.partial
        } else {
          sel <- dat$partial & dat$date == wk1.start
          wk1.partial <- dat[sel, ]
          wk1.backdate <- wk1.partial
          wk1.backdate$count <- sum(wk1$cranlogs.data$count)
          wk1.backdate$cumulative <- wk1.backdate$count
          cumulative.recompute <- cumsum(c(wk1.backdate$cumulative,
            dat$count[-1]))
        }

        current.wk <- dat[nrow(dat), ]
        current.wk.est <- current.wk

        weekdays.elapsed <- as.integer(x$last.obs.date -
          unit.date[length(unit.date)] + 1)

        if (as.integer(weekdays.elapsed) != 0) { # monday exception
          current.wk.est$count <- 7L / weekdays.elapsed * current.wk$count
        } else {
          current.wk.est$count <- 7L * current.wk$count
        }

        if (weekdays(x$from) != "Sunday") {
          current.wk.est$cumulative <-
            cumulative.recompute[(nrow(dat) - 1)] + current.wk.est$count
          first.last <- c(1, nrow(dat))
          dat.recompute <- rbind(wk1.backdate, dat[-first.last, ],
            current.wk.est)
          dat.recompute$cumulative[-first.last] <-
            cumulative.recompute[-first.last]
          current.wk$cumulative <- current.wk$count +
            rev(cumulative.recompute[-first.last])[1]
        } else {
          dat.recompute <- dat
        }

        complete <- dat.recompute[-c(1, nrow(dat.recompute)), ]
        wk1.partial$date <- max(min(wk1$cranlogs.data$date), x$from)

        xlim <- range(dat$date)
        ylim.data <- rbind(dat, dat.recompute)
        ylim <- range(ylim.data[, y.nm])

        if (log.y) {
          plot(complete[, c("date", statistic)], type = type, xlab = "Date",
            ylab = paste0("log10 ", y.nm.case), xlim = xlim, ylim = ylim,
            pch = 16, log = "y")
        } else {
          plot(complete[, c("date", statistic)], type = type, xlab = "Date",
            ylab = y.nm.case, xlim = xlim, ylim = ylim, pch = 16)
        }

        if (weekdays(x$from) == "Sunday") {
          points(wk1.partial$date, wk1.partial[, y.nm], pch = 16)
          segments(wk1.partial$date, wk1.partial[, y.nm],
                   complete[1, "date"], complete[1, y.nm])
        } else {
          points(wk1.backdate[, c("date", y.nm)], col = "dodgerblue", pch = 8)
          points(wk1.partial$date, wk1.partial[, y.nm], pch = 0, col = "gray")
          segments(wk1.backdate$date, wk1.backdate[, y.nm],
                   complete[1, "date"], complete[1, y.nm],
                   col = "dodgerblue")
          segments(wk1.partial$date, wk1.partial[, y.nm],
                   complete[1, "date"], complete[1, y.nm],
                   lty = "dotted")
        }

        if (weekdays(last.obs.date) == "Saturday") {
          points(current.wk$date, current.wk[, y.nm], pch = 16)
          segments(complete[nrow(complete), "date"],
                   complete[nrow(complete), y.nm],
                   current.wk$date,
                   current.wk[, y.nm])
        } else {
          points(current.wk.est$date, current.wk.est[, y.nm], col = "red")
          points(current.wk$date, current.wk[, y.nm], pch = 0, col = "gray")
          segments(complete[nrow(complete), "date"],
                   complete[nrow(complete), y.nm],
                   current.wk.est$date,
                   current.wk.est[, y.nm],
                   col = "red")
          segments(complete[nrow(complete), "date"],
                   complete[nrow(complete), y.nm],
                   current.wk$date,
                   current.wk[, y.nm],
                   lty = "dotted")
          axis(4, at = dat[nrow(dat), y.nm], labels = "obs")
          axis(4, at = current.wk.est[, y.nm], labels = "est", col.axis = "red",
            col.ticks = "red")
        }
      } else {
        if (log.y) {
          plot(dat$date, dat[, y.nm], type = type, xlab = "Date",
            ylab = paste0("log10 ", y.nm.case), log = "y")
        } else {
          plot(dat$date, dat[, y.nm], type = type, xlab = "Date",
            ylab = y.nm.case)
        }
      }

      if (r.version) {
        r_v <- rversions::r_versions()
        axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
          cex.axis = 2/3, padj = 0.9)
      }

      if (smooth) {
        if (any(dat$in.progress)) {
          smooth.data <- complete
          lines(stats::lowess(smooth.data$date, smooth.data[, y.nm], f = f),
            col = "blue")
        } else if (any(dat$partial)) {
          smooth.data <- rbind(wk1.backdate, complete)
          if (weekdays(last.obs.date) == "Saturday") {
            smooth.data <- rbind(smooth.data, current.wk)
          }
          lines(stats::lowess(smooth.data$date, smooth.data[, y.nm], f = f),
            col = "blue")
        } else {
          lines(stats::lowess(dat$date, dat[, y.nm], f = f), col = "blue")
        }
      }

      title(main = "Total Package Downloads")

    } else if (graphics == "ggplot2") {
      if (statistic == "count") {
        p <- ggplot(data = dat, aes_string("date", "count"))
      } else if (statistic == "cumulative") {
        p <- ggplot(data = dat, aes_string("date", "cumulative"))
      }

      if (any(dat$in.progress)) {
        ip.sel <- dat$in.progress == TRUE
        ip.data <- dat[ip.sel, ]
        complete <- dat[!ip.sel, ]
        last.obs <- nrow(complete)

        obs.days <- as.numeric(format(last.obs.date , "%d"))
        exp.days <- as.numeric(format(lastDayMonth(ip.data$date)$date, "%d"))
        est.ct <- round(ip.data$count * exp.days / obs.days)

        est.data <- ip.data
        est.data$count <- est.ct
        last.cumulative <- complete[nrow(complete), "cumulative"]
        est.data$cumulative <- last.cumulative + est.ct

        est.seg <- rbind(complete[last.obs, ], est.data)
        obs.seg <- rbind(complete[last.obs, ], ip.data)

        p <- p + geom_line(data = complete, size = 1/3) +
          scale_color_manual(name = "In-progress",
                             breaks = c("Observed", "Estimate"),
                             values = c("Observed" = "black",
                                        "Estimate" = "red")) +
          scale_shape_manual(name = "In-progress",
                             breaks = c("Observed", "Estimate"),
                             values = c("Observed" = 0, "Estimate" = 1)) +
          scale_linetype_manual(name = "In-progress",
                                breaks = c("Observed", "Estimate"),
                                values = c("Observed" = "dotted",
                                           "Estimate" = "solid")) +
          geom_line(data = est.seg, size = 1/3,
            aes(col = "Estimate", linetype = "Estimate")) +
          geom_line(data = obs.seg, size = 1/3,
            aes(col = "Observed", linetype = "Observed")) +
          geom_point(data = est.data,
            aes(colour = "Estimate", shape = "Estimate")) +
          geom_point(data = ip.data,
            aes(colour = "Observed", shape = "Observed"))

        if (points) p <- p + geom_point(data = complete)
        if (log.y) p <- p + scale_y_log10() + ylab(paste("log10", statistic))
        if (smooth) {
          if (any(dat$in.progress)) {
            smooth.data <- complete
            p <- p + geom_smooth(data = smooth.data, method = "loess",
              formula = "y ~ x", se = se, span = span)
          } else if (any(dat$partial)) {
            smooth.data <- rbind(wk1.backdate, complete)
            p <- p + geom_smooth(data = smooth.data, method = "loess",
              formula = "y ~ x", se = se, span = span)
          } else {
            p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se,
              span = span)
          }
        }

      } else if (any(dat$partial)) {
        unit.date <- dat$date

        wk1.start <- dat$date[1]
        wk1.end <- dat$date[2] - 1
        wk1 <- cranDownloads(from = wk1.start, to = wk1.end)

        if (weekdays(x$from) == "Sunday") {
          wk1.partial <- dat[dat$date == wk1.start, ]
          wk1.backdate <- wk1.partial
        } else {
          sel <- dat$partial & dat$date == wk1.start
          wk1.partial <- dat[sel, ]
          wk1.backdate <- wk1.partial

          wk1.backdate$count <- sum(wk1$cranlogs.data$count)
          wk1.backdate$cumulative <- wk1.backdate$count
          cumulative.recompute <- cumsum(c(wk1.backdate$cumulative,
            dat$count[-1]))
        }

        current.wk <- dat[nrow(dat), ]
        current.wk.est <- current.wk

        weekdays.elapsed <- as.integer(x$last.obs.date -
          unit.date[length(unit.date)] + 1)

        if (weekdays.elapsed != 0) { # monday exception
          current.wk.est$count <- 7L / weekdays.elapsed * current.wk$count
        } else {
          current.wk.est$count <- 7L * current.wk$count
        }

        if (weekdays(x$from) != "Sunday") {
          current.wk.est$cumulative <-
            cumulative.recompute[(nrow(dat) - 1)] + current.wk.est$count
          first.last <- c(1, nrow(dat))
          dat.recompute <- rbind(wk1.backdate, dat[-first.last, ],
            current.wk.est)
          dat.recompute$cumulative[-first.last] <-
            cumulative.recompute[-first.last]
          current.wk$cumulative <- current.wk$count +
            rev(cumulative.recompute[-first.last])[1]
        } else {
          dat.recompute <- dat
        }

        complete <- dat.recompute[-c(1, nrow(dat.recompute)), ]
        wk1.partial$date <- max(min(wk1$cranlogs.data$date), x$from)

        wk1.backdate.seg <- rbind(complete[1, ], wk1.backdate)
        wk1.partial.seg <- rbind(complete[1, ], wk1.partial)
        current.wk.seg <- rbind(complete[nrow(complete), ], current.wk)
        current.wk.est.seg <- rbind(complete[nrow(complete), ], current.wk.est)

        p <- p + geom_line(data = complete, size = 1/3) +
          scale_color_manual(name = "",
                             breaks = c("Backdate",
                                        "Partial/In-Progress",
                                        "Estimate"),
                             values = c("Backdate" = "dodgerblue",
                                        "Partial/In-Progress" = "black",
                                        "Estimate" = "red")) +
          scale_linetype_manual(name = "",
                                breaks = c("Backdate",
                                           "Partial/In-Progress",
                                           "Estimate"),
                                values = c("Backdate" = "solid",
                                           "Partial/In-Progress" = "dotted",
                                           "Estimate" = "solid")) +
          scale_shape_manual(name = "",
                             breaks = c("Backdate",
                                        "Partial/In-Progress",
                                        "Estimate"),
                             values = c("Backdate" = 8,
                                        "Partial/In-Progress" = 0,
                                        "Estimate" = 1))

        if (weekdays(last.obs.date) == "Saturday") {
          p <- p +
            geom_line(data = current.wk.est.seg, size = 1/3) +
            geom_point(data = current.wk.est, shape = 16)
        } else {
          p <- p +
            geom_line(data = current.wk.est.seg, size = 1/3,
              aes(colour = "Estimate", linetype = "Estimate")) +
            geom_point(data = current.wk.est, size = 1.5,
              aes(colour = "Estimate", shape = "Estimate")) +
            geom_line(data = current.wk.seg, size = 1/3,
              aes(colour = "Partial/In-Progress",
                  linetype = "Partial/In-Progress")) +
            geom_point(data = current.wk,
              aes(colour = "Partial/In-Progress",
                  shape = "Partial/In-Progress"))
         }

         if (weekdays(x$from) == "Sunday") {
           p <- p +
             geom_line(data = wk1.partial.seg, size = 1/3) +
             geom_point(data = wk1.partial)
         } else {
           p <- p +
             geom_line(data = wk1.backdate.seg, size = 1/3,
               aes(colour = "Backdate", linetype = "Backdate")) +
             geom_point(data = wk1.backdate,
               aes(colour = "Backdate", shape = "Backdate")) +
             geom_line(data = wk1.partial.seg, size = 1/3,
               aes(colour = "Partial/In-Progress",
                   linetype = "Partial/In-Progress")) +
             geom_point(data = wk1.partial,
               aes(colour = "Partial/In-Progress",
                   shape = "Partial/In-Progress"))
         }

        if (points) p <- p + geom_point(data = complete)
        if (log.y) p <- p + scale_y_log10() + ylab(paste("log10", statistic))
        if (smooth) {
          if (any(dat$in.progress)) {
            smooth.data <- complete
            p <- p + geom_smooth(data = smooth.data, method = "loess",
              formula = "y ~ x", se = se, span = span)
          } else if (any(dat$partial)) {
            smooth.data <- rbind(wk1.backdate, complete)
            if (weekdays(last.obs.date) == "Saturday") {
              smooth.data <- rbind(smooth.data, current.wk)
            }
            p <- p + geom_smooth(data = smooth.data, method = "loess",
              formula = "y ~ x", se = se, span = span)
          } else {
            p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se,
              span = span)
          }
        }
      } else {
        p <- p + geom_line(size = 1/3)
        if (points) p <- p + geom_point()
        if (log.y) p <- p + scale_y_log10() + ylab(paste("log10", statistic))
        if (smooth) {
          p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se,
            span = span)
        }
      }

      p <- p + theme_bw() +
        ggtitle("Total Package Downloads") +
        theme(legend.position = "bottom",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5))

      suppressWarnings(print(p))
    }
  }
}

singlePlot <- function(x, statistic, graphics, obs.ct, points, smooth,
  se, f, span, log.y, package.version, dev.mode, r.version, same.xy) {

  dat <- x$cranlogs.data
  last.obs.date <- x$last.obs.date
  type <- ifelse(points, "o", "l")

  y.nm <- statistic
  y.var <- dat[, y.nm]
  y.nm.case <- tools::toTitleCase(statistic)

  if (statistic == "count") {
    ttl <- "Package Download Counts"
  } else if (statistic == "cumulative") {
    ttl <- "Cumulative Package Downloads"
  }

  if (graphics == "base") {
    if (obs.ct == 1) {
      if (log.y) {
        if (any(dat$count == 0)) {
          zero.ct.pkg <- unique(dat[dat$count == 0, "package"])
          dat[dat$count == 0, "count"] <- 1
          for (p in zero.ct.pkg) {
            dat$cumulative <- cumsum(dat[dat$package == p, "count"])
          }
        }
        dotchart(log10(dat$count), labels = dat$package,
          xlab = paste("log10", y.nm.case), main = paste(ttl, unique(dat$date)))
      } else {
        dotchart(dat$count, labels = dat$package, xlab = "Count",
          main = paste(ttl, unique(dat$date)))
      }
    } else if (obs.ct > 1) {
      if (same.xy) {
        xlim <- range(dat$date)
      } else {
        xlim <- NULL
      }

      if (length(x$packages) > 1) grDevices::devAskNewPage(ask = TRUE)

      if (any(dat$in.progress)) {
        plot.data <- lapply(x$package, function(pkg) {
          pkg.dat <- dat[dat$package == pkg, ]

          if (any(pkg.dat$count == 0)) {
            zero.ct.pkg <- unique(pkg.dat[pkg.dat$count == 0, "package"])
            pkg.dat[pkg.dat$count == 0, "count"] <- 1
            for (p in zero.ct.pkg) {
              pkg.dat$cumulative <- cumsum(pkg.dat[pkg.dat$package == p,
                "count"])
            }
          }

          ip.sel <- pkg.dat$in.progress == TRUE
          ip.data <- pkg.dat[ip.sel, ]
          complete <- pkg.dat[!ip.sel, ]

          obs.days <- as.numeric(format(last.obs.date , "%d"))
          exp.days <- as.numeric(format(lastDayMonth(ip.data$date)$date, "%d"))
          est.ct <- round(ip.data$count * exp.days / obs.days)

          est.data <- ip.data
          est.data$count <- est.ct
          last.cumulative <- complete[nrow(complete), "cumulative"]
          est.data$cumulative <- last.cumulative + est.ct

          list(complete = complete, ip.data = ip.data,
            est.data = est.data)
         })

        tmp <- lapply(plot.data, function(x) do.call(rbind, x))
        tmp <- do.call(rbind, tmp)
        xlim <- range(tmp$date)
        ylim <- range(tmp[, y.nm])

        invisible(lapply(seq_along(plot.data), function(i) {
          complete <- plot.data[[i]]$complete
          ip.data <- plot.data[[i]]$ip.data
          est.data <- plot.data[[i]]$est.data

          if (log.y) {
            plot(complete$date, complete[, y.nm], type = type,
              xlab = "Date", ylab = paste0("log10 ", y.nm.case), xlim = xlim,
              ylim = ylim, log = "y", pch = 16)
          } else {
            plot(complete$date, complete[, y.nm], type = type,
              xlab = "Date", ylab = y.nm.case, xlim = xlim, ylim = ylim,
              pch = 16)
          }

          points(ip.data[, "date"], ip.data[, y.nm], col = "black", pch = 0)
          points(est.data[, "date"], est.data[, y.nm], col = "red", pch = 1)

          last.obs <- nrow(complete)
          segments(complete[last.obs, "date"],
                   complete[last.obs, y.nm],
                   ip.data$date,
                   ip.data[, y.nm],
                   lty = "dotted")
          segments(complete[last.obs, "date"],
                   complete[last.obs, y.nm],
                   est.data$date,
                   est.data[, y.nm],
                   col = "red")

          axis(4, at = ip.data[, y.nm], labels = "obs")
          axis(4, at = est.data[, y.nm], labels = "est", col.axis = "red",
            col.ticks = "red")

          if (package.version) {
            if (dev.mode) p_v <- packageHistory0(est.data$package)
            else p_v <- packageHistory(est.data$package)
            axis(3, at = p_v$Date, labels = p_v$Version, cex.axis = 2/3,
              padj = 0.9, col.axis = "red", col.ticks = "red")
          }

          if (r.version) {
            r_v <- rversions::r_versions()
            axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
              cex.axis = 2/3, padj = 0.9)
          }

          if (smooth) {
            if (any(dat$in.progress)) {
              smooth.data <- complete
              lines(stats::lowess(smooth.data$date, smooth.data[, y.nm],
                f = f), col = "blue")
            } else {
              lines(stats::lowess(dat$date, dat[, y.nm], f = f), col = "blue")
            }
          }
          title(main = est.data$package)
        }))

      } else if (any(dat$partial)) { # unit.observation = "week"
        plot.data <- lapply(x$package, function(pkg) {
          pkg.dat <- dat[dat$package == pkg, ]

          if (log.y) {
            if (any(pkg.dat$count == 0)) {
              zero.ct.pkg <- unique(pkg.dat[pkg.dat$count == 0, "package"])
              pkg.dat[pkg.dat$count == 0, "count"] <- 1
              for (p in zero.ct.pkg) {
                pkg.dat$cumulative <- cumsum(pkg.dat[pkg.dat$package == p,
                  "count"])
              }
            }
          }

          unit.date <- pkg.dat$date

          wk1.start <- pkg.dat$date[1]
          wk1.end <- pkg.dat$date[2] - 1
          wk1 <- cranDownloads(pkg, from = wk1.start, to = wk1.end)

          if (weekdays(x$from) == "Sunday") {
            wk1.partial <- pkg.dat[pkg.dat$date == wk1.start, ]
            wk1.backdate <- wk1.partial
          } else {
            sel <- pkg.dat$partial & pkg.dat$date == wk1.start
            wk1.partial <- pkg.dat[sel, ]
            wk1.backdate <- wk1.partial
            wk1.backdate$count <- sum(wk1$cranlogs.data$count)
            wk1.backdate$cumulative <- wk1.backdate$count
            cumulative.recompute <- cumsum(c(wk1.backdate$cumulative,
              pkg.dat$count[-1]))
          }

          current.wk <- pkg.dat[nrow(pkg.dat), ]
          current.wk.est <- current.wk

          weekdays.elapsed <- as.integer(last.obs.date -
            unit.date[length(unit.date)] + 1)

          if (weekdays.elapsed != 0) {
            current.wk.est$count <- 7L / weekdays.elapsed * current.wk$count
          } else {
            current.wk.est$count <- 7L * current.wk$count
          }

          if (weekdays(x$from) != "Sunday") {
            current.wk.est$cumulative <-
              cumulative.recompute[(nrow(pkg.dat) - 1)] + current.wk.est$count
            first.last <- c(1, nrow(pkg.dat))
            pkg.dat.recompute <- rbind(wk1.backdate, pkg.dat[-first.last, ],
              current.wk.est)
            pkg.dat.recompute$cumulative[-first.last] <-
              cumulative.recompute[-first.last]
            current.wk$cumulative <- current.wk$count +
              rev(cumulative.recompute[-first.last])[1]
          } else {
            pkg.dat.recompute <- pkg.dat
          }

          if (weekdays.elapsed == 7) {
            sel <- pkg.dat.recompute$date != min(pkg.dat.recompute$date)
            complete <- pkg.dat.recompute[sel, ]
          } else {
            complete <- pkg.dat.recompute[!pkg.dat.recompute$partial, ]
          }

          wk1.partial$date <- max(min(wk1$cranlogs.data$date), x$from)

          list(pkg.dat = pkg.dat,
               wk1.partial = wk1.partial,
               wk1.backdate = wk1.backdate,
               current.wk = current.wk,
               current.wk.est = current.wk.est,
               pkg.dat.recompute = pkg.dat.recompute,
               complete = complete)
        })

        ylim.lst <- lapply(plot.data, function(x) {
          x[c("pkg.dat", "pkg.dat.recompute")]
        })

        ylim.data <- do.call(rbind, lapply(ylim.lst, function(x) {
          do.call(rbind, x)
        }))

        ylim <- range(ylim.data[, y.nm])

        invisible(lapply(seq_along(plot.data), function(i) {
          pkg.dat <- plot.data[[i]]$pkg.dat
          wk1.partial <- plot.data[[i]]$wk1.partial
          wk1.backdate <- plot.data[[i]]$wk1.backdate
          current.wk <- plot.data[[i]]$current.wk
          current.wk.est <- plot.data[[i]]$current.wk.est
          pkg.dat.recompute <- plot.data[[i]]$pkg.dat.recompute
          complete <- plot.data[[i]]$complete

          if (log.y) {
            plot(complete[, c("date", y.nm)], type = type, xlab = "Date",
              ylab = paste0("log10 ", y.nm.case), xlim = xlim, ylim = ylim,
              pch = 16, log = "y")
          } else {
            plot(complete[, c("date", y.nm)], type = type, xlab = "Date",
              ylab = y.nm.case, xlim = xlim, ylim = ylim, pch = 16)
          }

          if (weekdays(x$from) == "Sunday") {
            points(wk1.partial[, c("date", y.nm)], pch = 16)
            segments(wk1.partial$date, wk1.partial[, y.nm],
                     complete[1, "date"], complete[1, y.nm])
          } else {
            points(wk1.backdate[, c("date", y.nm)], col = "dodgerblue", pch = 8)
            points(wk1.partial$date, wk1.partial[, y.nm], pch = 0, col = "gray")
            segments(wk1.backdate$date, wk1.backdate[, y.nm],
                     complete[1, "date"], complete[1, y.nm],
                     col = "dodgerblue")
            segments(wk1.partial$date, wk1.partial[, y.nm],
                     complete[1, "date"], complete[1, y.nm],
                     lty = "dotted")
          }

          if (weekdays(last.obs.date) == "Saturday") {
            points(current.wk$date, current.wk[, y.nm], pch = 16)
            segments(complete[nrow(complete), "date"],
                     complete[nrow(complete), y.nm],
                     current.wk$date,
                     current.wk[, y.nm])
          } else {
            points(current.wk.est$date, current.wk.est[, y.nm], col = "red")
            points(current.wk$date, current.wk[, y.nm], pch = 0, col = "gray")
            segments(complete[nrow(complete), "date"],
                     complete[nrow(complete), y.nm],
                     current.wk.est$date,
                     current.wk.est[, y.nm],
                     col = "red")
            segments(complete[nrow(complete), "date"],
                     complete[nrow(complete), y.nm],
                     current.wk$date,
                     current.wk[, y.nm],
                     lty = "dotted")
            axis(4, at = pkg.dat[nrow(pkg.dat), y.nm], labels = "obs")
            axis(4, at = current.wk.est[, y.nm], labels = "est",
              col.axis = "red", col.ticks = "red")
          }

          if (package.version) {
            if (dev.mode) p_v <- packageHistory0(current.wk.est$package)
            else p_v <- packageHistory(current.wk.est$package)
            axis(3, at = p_v$Date, labels = p_v$Version, cex.axis = 2/3,
              padj = 0.9, col.axis = "red", col.ticks = "red")
          }

          if (r.version) {
            r_v <- rversions::r_versions()
            axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
              cex.axis = 2/3, padj = 0.9)
          }

          if (smooth) {
            if (any(pkg.dat$partial)) {
              smooth.data <- rbind(wk1.backdate, complete)
              if (weekdays(last.obs.date) == "Saturday") {
                smooth.data <- rbind(smooth.data, current.wk)
              }
              lines(stats::lowess(smooth.data$date, smooth.data[, y.nm],
                f = f), col = "blue")
            } else {
              lines(stats::lowess(pkg.dat$date, pkg.dat[, y.nm], f = f),
                col = "blue")
            }
          }

          title(main = wk1.backdate$package)
        }))

      } else {
        if (log.y) {
          if (any(dat$count == 0)) {
            zero.ct.pkg <- unique(dat[dat$count == 0, "package"])
            dat[dat$count == 0, "count"] <- 1
            for (p in zero.ct.pkg) {
              dat$cumulative <- cumsum(dat[dat$package == p, "count"])
            }
          }
        }

        ylim <- range(dat[, y.nm])

        invisible(lapply(x$package, function(pkg) {
          pkg.dat <- dat[dat$package == pkg, ]
          type <- ifelse(points, "o", "l")

          if (log.y) {
            plot(pkg.dat$date, pkg.dat[, y.nm], type = type, xlab = "Date",
              ylab = paste0("log10 ", y.nm.case), xlim = xlim, ylim = ylim,
              log = "y")
          } else {
            plot(pkg.dat$date, pkg.dat[, y.nm], type = type, xlab = "Date",
              ylab = y.nm.case, xlim = xlim, ylim = ylim)
          }

          if (package.version) {
            if (dev.mode) p_v <- packageHistory0(pkg)
            else p_v <- packageHistory(pkg)
            axis(3, at = p_v$Date, labels = p_v$Version, cex.axis = 2/3,
              padj = 0.9, col.axis = "red", col.ticks = "red")
          }

          if (r.version) {
            r_v <- rversions::r_versions()
            axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
              cex.axis = 2/3, padj = 0.9)
          }

          if (smooth) {
            lines(stats::lowess(pkg.dat$date, pkg.dat[, y.nm], f = f),
              col = "blue")
          }
          title(main = pkg)
        }))
      }

      if (length(x$packages) > 1) grDevices::devAskNewPage(ask = FALSE)
    }

  } else if (graphics == "ggplot2") {
    if (obs.ct == 1) {
      if (log.y) {
        if (any(dat$count == 0)) {
          zero.ct.pkg <- unique(dat[dat$count == 0, "package"])
          dat[dat$count == 0, "count"] <- 1
          for (p in zero.ct.pkg) {
            dat$cumulative <- cumsum(dat[dat$package == p, "count"])
          }
        }
      }

      p <- ggplot(data = dat) +
           theme_bw() +
           theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor = element_blank()) +
           facet_wrap(~ date, nrow = 2)

      if (statistic == "count") {
        p <- p + geom_point(aes_string("count", "package"), size = 1.5)
      } else if (statistic == "cumulative") {
        p <- p + geom_point(aes_string("cumulative", "package"), size = 1.5)
      }

      if (log.y) p <- p + scale_x_log10() + xlab(paste("log10", y.nm.case))

    } else if (obs.ct > 1) {
      if (log.y) {
        if (any(dat$count == 0)) {
          zero.ct.pkg <- unique(dat[dat$count == 0, "package"])
          dat[dat$count == 0, "count"] <- 1
          for (p in zero.ct.pkg) {
            dat$cumulative <- cumsum(dat[dat$package == p, "count"])
          }
        }
      }

      if (statistic == "count") {
        p <- ggplot(data = dat, aes_string("date", "count"))
      } else if (statistic == "cumulative") {
        p <- ggplot(data = dat, aes_string("date", "cumulative"))
      }

      if (any(dat$in.progress)) {
        g <- lapply(x$packages, function(pkg) {
          pkg.data <- dat[dat$package == pkg, ]
          ip.sel <- pkg.data$in.progress == TRUE
          ip.data <- pkg.data[ip.sel, ]
          complete <- pkg.data[!ip.sel, ]
          last.obs <- nrow(complete)

          obs.days <- as.numeric(format(last.obs.date , "%d"))
          exp.days <- as.numeric(format(lastDayMonth(ip.data$date)$date, "%d"))
          est.ct <- round(ip.data$count * exp.days / obs.days)

          est.data <- ip.data
          est.data$count <- est.ct
          last.cumulative <- complete[nrow(complete), "cumulative"]
          est.data$cumulative <- last.cumulative + est.ct

          list(ip.data = ip.data,
               complete = complete,
               est.data = est.data,
               est.seg = rbind(complete[last.obs, ], est.data),
               obs.seg = rbind(complete[last.obs, ], ip.data))
        })

        ip.data <- do.call(rbind, lapply(g, function(x) x$ip.data))
        complete <- do.call(rbind, lapply(g, function(x) x$complete))
        est.data <- do.call(rbind, lapply(g, function(x) x$est.data))
        est.seg <- do.call(rbind, lapply(g, function(x) x$est.seg))
        obs.seg <- do.call(rbind, lapply(g, function(x) x$obs.seg))

        p <- p + geom_line(data = complete, size = 1/3) +
          scale_color_manual(name = "In-progress",
                             breaks = c("Observed", "Estimate"),
                             values = c("Observed" = "black",
                                        "Estimate" = "red")) +
          scale_shape_manual(name = "In-progress",
                             breaks = c("Observed", "Estimate"),
                             values = c("Observed" = 0, "Estimate" = 1)) +
          scale_linetype_manual(name = "In-progress",
                                breaks = c("Observed", "Estimate"),
                                values = c("Observed" = "dotted",
                                           "Estimate" = "solid")) +
          geom_line(data = est.seg, size = 1/3,
            aes(col = "Estimate", linetype = "Estimate")) +
          geom_line(data = obs.seg, size = 1/3,
            aes(col = "Observed", linetype = "Observed")) +
          geom_point(data = est.data,
            aes(colour = "Estimate", shape = "Estimate")) +
          geom_point(data = ip.data,
            aes(colour = "Observed", shape = "Observed"))

        if (points) p <- p + geom_point(data = complete)

      } else if (any(dat$partial)) {
        ggplot.data <- lapply(x$package, function(pkg) {
          pkg.dat <- dat[dat$package == pkg, ]
          unit.date <- pkg.dat$date

          wk1.start <- pkg.dat$date[1]
          wk1.end <- pkg.dat$date[2] - 1
          wk1 <- cranDownloads(pkg, from = wk1.start, to = wk1.end)

          if (weekdays(x$from) == "Sunday") {
            wk1.partial <- pkg.dat[pkg.dat$date == wk1.start, ]
            wk1.backdate <- wk1.partial
          } else {
            sel <- pkg.dat$partial & pkg.dat$date == wk1.start
            wk1.partial <- pkg.dat[sel, ]
            wk1.backdate <- wk1.partial

            wk1.backdate$count <- sum(wk1$cranlogs.data$count)
            wk1.backdate$cumulative <- wk1.backdate$count
            cumulative.recompute <- cumsum(c(wk1.backdate$cumulative,
              pkg.dat$count[-1]))
          }

          current.wk <- pkg.dat[nrow(pkg.dat), ]
          current.wk.est <- current.wk

          weekdays.elapsed <- as.integer(last.obs.date -
            unit.date[length(unit.date)] + 1)

          if (weekdays.elapsed != 0) {
            current.wk.est$count <- 7L / weekdays.elapsed * current.wk$count
          } else {
            current.wk.est$count <- 7L * current.wk$count
          }

          if (weekdays(x$from) != "Sunday") {
            current.wk.est$cumulative <-
              cumulative.recompute[(nrow(pkg.dat) - 1)] + current.wk.est$count
            first.last <- c(1, nrow(pkg.dat))
            pkg.dat.recompute <- rbind(wk1.backdate, pkg.dat[-first.last, ],
              current.wk.est)
            pkg.dat.recompute$cumulative[-first.last] <-
              cumulative.recompute[-first.last]
            current.wk$cumulative <- current.wk$count +
              rev(cumulative.recompute[-first.last])[1]
          } else {
            pkg.dat.recompute <- pkg.dat
          }

          complete <- pkg.dat.recompute[-c(1, nrow(pkg.dat.recompute)), ]
          wk1.partial$date <- max(min(wk1$cranlogs.data$date), x$from)

          list(pkg.dat = pkg.dat,
               wk1.partial = wk1.partial,
               wk1.backdate = wk1.backdate,
               current.wk = current.wk,
               current.wk.est = current.wk.est,
               pkg.dat.recompute = pkg.dat.recompute,
               complete = complete,
               wk1.backdate.seg = rbind(complete[1, ], wk1.backdate),
               wk1.partial.seg = rbind(complete[1, ], wk1.partial),
               current.wk.seg = rbind(complete[nrow(complete), ],
                 current.wk),
               current.wk.est.seg = rbind(complete[nrow(complete), ],
                 current.wk.est),
               pkg.dat.recompute = pkg.dat.recompute)
        })

        complete <- do.call(rbind, lapply(ggplot.data, function(x) x$complete))
        wk1.partial <- do.call(rbind, lapply(ggplot.data, function(x)
          x$wk1.partial))
        wk1.backdate <- do.call(rbind, lapply(ggplot.data, function(x)
          x$wk1.backdate))
        current.wk <- do.call(rbind, lapply(ggplot.data, function(x)
          x$current.wk))
        current.wk.est <- do.call(rbind, lapply(ggplot.data, function(x)
          x$current.wk.est))
        wk1.backdate.seg <- do.call(rbind, lapply(ggplot.data, function(x)
          x$wk1.backdate.seg))
        wk1.partial.seg <- do.call(rbind, lapply(ggplot.data, function(x)
          x$wk1.partial.seg))
        current.wk.seg <- do.call(rbind, lapply(ggplot.data,
          function(x) x$current.wk.seg))
        current.wk.est.seg <- do.call(rbind, lapply(ggplot.data,
          function(x) x$current.wk.est.seg))

        p <- p + geom_line(data = complete, size = 1/3) +
          scale_color_manual(name = "",
                             breaks = c("Backdate",
                                        "Partial/In-Progress",
                                        "Estimate"),
                             values = c("Backdate" = "dodgerblue",
                                        "Partial/In-Progress" = "black",
                                        "Estimate" = "red")) +
          scale_linetype_manual(name = "",
                                breaks = c("Backdate",
                                           "Partial/In-Progress",
                                           "Estimate"),
                                values = c("Backdate" = "solid",
                                           "Partial/In-Progress" = "dotted",
                                           "Estimate" = "solid")) +
          scale_shape_manual(name = "",
                             breaks = c("Backdate",
                                        "Partial/In-Progress",
                                        "Estimate"),
                             values = c("Backdate" = 8,
                                        "Partial/In-Progress" = 0,
                                        "Estimate" = 1))

        if (weekdays(last.obs.date) == "Saturday") {
           p <- p +
            geom_line(data = current.wk.seg, size = 1/3) +
            geom_point(data = current.wk, shape = 16)
        } else {
          p <- p +
            geom_line(data = current.wk.est.seg, size = 1/3,
              aes(colour = "Estimate", linetype = "Estimate")) +
            geom_point(data = current.wk.est, size = 1.5,
              aes(colour = "Estimate", shape = "Estimate")) +
            geom_line(data = current.wk.seg, size = 1/3,
              aes(colour = "Partial/In-Progress",
                  linetype = "Partial/In-Progress")) +
            geom_point(data = current.wk,
              aes(colour = "Partial/In-Progress",
                  shape = "Partial/In-Progress"))
        }

        if (weekdays(x$from) == "Sunday") {
          p <- p +
            geom_line(data = wk1.partial.seg, size = 1/3) +
            geom_point(data = wk1.partial)
        } else {
          p <- p +
          geom_line(data = wk1.backdate.seg, size = 1/3,
            aes(colour = "Backdate", linetype = "Backdate")) +
          geom_point(data = wk1.backdate,
            aes(colour = "Backdate", shape = "Backdate")) +
          geom_line(data = wk1.partial.seg, size = 1/3,
            aes(colour = "Partial/In-Progress",
                linetype = "Partial/In-Progress")) +
          geom_point(data = wk1.partial,
            aes(colour = "Partial/In-Progress", shape = "Partial/In-Progress"))
        }

        if (points) p <- p + geom_point(data = complete)

      } else {
        p <- p + geom_line(size = 1/3)
        if (points) p <- p + geom_point()
      }

      if (log.y) p <- p + scale_y_log10() + ylab(paste("log10", statistic))

      if (smooth) {
        if (any(dat$in.progress)) {
          smooth.data <- complete
          p <- p + geom_smooth(data = smooth.data, method = "loess",
            formula = "y ~ x", se = se, span = span)
        } else if (any(dat$partial)) {
          smooth.data <- rbind(complete, wk1.backdate.seg)
          if (weekdays(last.obs.date) == "Saturday") {
            smooth.data <- rbind(smooth.data, current.wk)
          }
          p <- p + geom_smooth(data = smooth.data, method = "loess",
            formula = "y ~ x", se = se, span = span)
        } else {
          p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se,
            span = span)
        }
      }

      p <- p + facet_wrap(~ package, nrow = 2) +
               theme_bw() +
               theme(legend.position = "bottom",
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
    }
    suppressWarnings(print(p))
  }
}

multiPlot <- function(x, statistic, graphics, obs.ct, log.y,
  legend.location, ip.legend.location, points, smooth, se, f, span) {

  dat <- x$cranlogs.data
  last.obs.date <- x$last.obs.date
  y.nm.case <- tools::toTitleCase(statistic)

  if (statistic == "count") {
    ttl <- "Package Download Counts"
  } else if (statistic == "cumulative") {
    ttl <- "Cumulative Package Downloads"
  }

  if (graphics == "base") {
    if (obs.ct == 1) {
      if (log.y) {
        if (any(dat$count == 0)) {
          zero.ct.pkg <- unique(dat[dat$count == 0, "package"])
          dat[dat$count == 0, "count"] <- 1
          for (p in zero.ct.pkg) {
            dat$cumulative <- cumsum(dat[dat$package == p, "count"])
          }
        }
        dotchart(log10(dat$count), labels = dat$package,
          xlab = paste("log10", y.nm.case), main = paste(ttl, unique(dat$date)))
      } else {
        dotchart(dat$count, labels = dat$package, xlab = "Count",
           main = paste(ttl, unique(dat$date)))
      }
    } else if (obs.ct > 1) {
      if (length(x$packages) > 8) {
        stop('Use <= 8 packages when graphics = "base".', call. = FALSE)
      } else {
        # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
        # http://jfly.iam.u-tokyo.ac.jp/color/
        # The palette with grey:
        # cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
        #   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
          "#0072B2", "#D55E00", "#CC79A7")

        vars <- c("date", statistic)
        type <- ifelse(points, "o", "l")
        xlim <- range(dat$date)
        ylim <- range(dat[, statistic])

        if (any(dat$in.progress)) {
          pkg.data <- lapply(x$package, function(pkg) {
            tmp <- dat[dat$package == pkg, ]

            if (log.y) {
              if (any(tmp$count == 0)) {
                zero.ct.pkg <- unique(tmp[tmp$count == 0, "package"])
                tmp[tmp$count == 0, "count"] <- 1
                for (p in zero.ct.pkg) {
                  tmp$cumulative <- cumsum(tmp[tmp$package == p, "count"])
                }
              }
            }

            ip.data <- tmp[tmp$in.progress == TRUE, ]
            complete <- tmp[tmp$in.progress == FALSE, ]
            last.obs <- nrow(complete)

            obs.days <- as.numeric(format(last.obs.date , "%d"))
            exp.days <- as.numeric(format(lastDayMonth(ip.data$date)$date,
              "%d"))
            est.ct <- round(ip.data$count * exp.days / obs.days)

            est.data <- ip.data
            est.data$count <- est.ct
            last.cumulative <- complete[nrow(complete), "cumulative"]
            est.data$cumulative <- last.cumulative + est.ct

            list(complete = complete, est.data = est.data, ip.data = ip.data)
          })

          est.stat <- lapply(pkg.data, function(x) x$est.data)
          est.stat <- do.call(rbind, est.stat)[, statistic]
          ylim <- range(c(ylim, est.stat))

          if (log.y) {
            plot(dat[, vars], pch = NA, log = "y", xlim = xlim, ylim = ylim,
              main = ttl)
          } else {
            plot(dat[, vars], pch = NA, xlim = xlim, ylim = ylim, main = ttl)
          }

          invisible(lapply(seq_along(pkg.data), function(i) {
            complete <- pkg.data[[i]]$complete
            est.data <- pkg.data[[i]]$est.data
            ip.data <- pkg.data[[i]]$ip.data
            last.obs <- nrow(complete)

            lines(complete$date, complete[, statistic], col = cbPalette[i])
            segments(complete[last.obs, "date"],
                     complete[last.obs, statistic],
                     ip.data$date,
                     ip.data[, statistic],
                     col = cbPalette[i],
                     lty = "dotted")
            segments(complete[last.obs, "date"],
                     complete[last.obs, statistic],
                     est.data$date,
                     est.data[, statistic],
                     col = cbPalette[i],
                     lty = "longdash")
             points(est.data[, "date"], est.data[, statistic],
               col = cbPalette[i])
             points(ip.data[, "date"], ip.data[, statistic], pch = 0,
               col = cbPalette[i])

            if (points) {
              points(complete[, "date"], complete[, statistic],
                col = cbPalette[i], pch = 16)
            }

            if (smooth) {
              smooth.data <- complete
              lines(stats::lowess(smooth.data$date, smooth.data[, statistic],
                f = f), col = cbPalette[i])
            }
          }))

          legend(x = ip.legend.location,
                legend = c("Observed", "Estimate"),
                pch = 0:1,
                bg = "white",
                cex = 2/3,
                title = NULL,
                bty = "n",
                lty = c("dotted", "longdash"))

        } else if (any(dat$partial)) {
          plot.data <- lapply(x$package, function(pkg) {
            pkg.dat <- dat[dat$package == pkg, ]

            if (log.y) {
              if (any(pkg.dat$count == 0)) {
                zero.ct.pkg <- unique(pkg.dat[pkg.dat$count == 0, "package"])
                pkg.dat[pkg.dat$count == 0, "count"] <- 1
                for (p in zero.ct.pkg) {
                  pkg.dat$cumulative <- cumsum(pkg.dat[pkg.dat$package == p,
                    "count"])
                }
              }
            }

            unit.date <- pkg.dat$date

            wk1.start <- pkg.dat$date[1]
            wk1.end <- pkg.dat$date[2] - 1
            wk1 <- cranDownloads(pkg, from = wk1.start, to = wk1.end)

            if (weekdays(x$from) == "Sunday") {
              wk1.partial <- pkg.dat[pkg.dat$date == wk1.start, ]
              wk1.backdate <- wk1.partial
            } else {
              sel <- pkg.dat$partial & pkg.dat$date == wk1.start
              wk1.partial <- pkg.dat[sel, ]
              wk1.backdate <- wk1.partial
              wk1.backdate$count <- sum(wk1$cranlogs.data$count)
              wk1.backdate$cumulative <- wk1.backdate$count
              cumulative.recompute <- cumsum(c(wk1.backdate$cumulative,
                pkg.dat$count[-1]))
            }

            current.wk <- pkg.dat[nrow(pkg.dat), ]
            current.wk.est <- current.wk

            weekdays.elapsed <- as.integer(x$last.obs.date -
              unit.date[length(unit.date)] + 1)

            if (weekdays.elapsed != 0) { # monday exception
              current.wk.est$count <- 7L / weekdays.elapsed * current.wk$count
            } else {
              current.wk.est$count <- 7L * current.wk$count
            }

            if (weekdays(x$from) != "Sunday") {
              current.wk.est$cumulative <-
                cumulative.recompute[(nrow(pkg.dat) - 1)] + current.wk.est$count
              first.last <- c(1, nrow(pkg.dat))
              pkg.dat.recompute <- rbind(wk1.backdate, pkg.dat[-first.last, ],
                current.wk.est)
              pkg.dat.recompute$cumulative[-first.last] <-
                cumulative.recompute[-first.last]
              current.wk$cumulative <- current.wk$count +
                rev(cumulative.recompute[-first.last])[1]
            } else {
              pkg.dat.recompute <- pkg.dat
            }

            if (weekdays.elapsed == 7) {
              sel <- pkg.dat.recompute$date != min(pkg.dat.recompute$date)
              complete <- pkg.dat.recompute[sel, ]
            } else {
              complete <- pkg.dat.recompute[!pkg.dat.recompute$partial, ]
            }

            wk1.partial$date <- max(min(wk1$cranlogs.data$date), x$from)

            list(pkg.dat = pkg.dat,
                 wk1.partial = wk1.partial,
                 wk1.backdate = wk1.backdate,
                 current.wk = current.wk,
                 current.wk.est = current.wk.est,
                 pkg.dat.recompute = pkg.dat.recompute,
                 complete = complete)
          })

          ylim.lst <- lapply(plot.data, function(x) {
            x[c("pkg.dat", "pkg.dat.recompute")]
          })

          ylim.data <- do.call(rbind, lapply(ylim.lst, function(x) {
            do.call(rbind, x)
          }))

          ylim <- range(ylim.data[, statistic])

          if (log.y) {
            plot(dat[, vars], pch = NA, log = "y", xlim = xlim, ylim = ylim,
              main = ttl)
          } else {
            plot(dat[, vars], pch = NA, xlim = xlim, ylim = ylim, main = ttl)
          }

          invisible(lapply(seq_along(plot.data), function(i) {
            pkg.dat <- plot.data[[i]]$pkg.dat
            wk1.partial <- plot.data[[i]]$wk1.partial
            wk1.backdate <- plot.data[[i]]$wk1.backdate
            current.wk <- plot.data[[i]]$current.wk
            current.wk.est <- plot.data[[i]]$current.wk.est
            pkg.dat.recompute <- plot.data[[i]]$pkg.dat.recompute
            complete <- plot.data[[i]]$complete

            if (points) points(complete[, vars], col = cbPalette[i], pch = 16)
            lines(complete[, c("date", statistic)], col = cbPalette[i])

            if (weekdays(x$from) == "Sunday") {
              points(wk1.partial$date, wk1.partial[, statistic], pch = 16,
                col = cbPalette[i])
              segments(wk1.partial$date, wk1.partial[, statistic],
                       complete[1, "date"], complete[1, statistic],
                       col = cbPalette[i])
            } else {
              points(wk1.backdate[, c("date", statistic)], col = cbPalette[i],
                pch = 8)
              points(wk1.partial$date, wk1.partial[, statistic], pch = 0,
                col = cbPalette[i])
              segments(wk1.backdate$date, wk1.backdate[, statistic],
                       complete[1, "date"], complete[1, statistic],
                       col = cbPalette[i], lty = "longdash")
              segments(wk1.partial$date, wk1.partial[, statistic],
                       complete[1, "date"], complete[1, statistic],
                       col = cbPalette[i], lty = "dotted")
            }

            if (weekdays(last.obs.date) == "Saturday") {
              points(current.wk$date, current.wk[, statistic], pch = 16,
                col = cbPalette[i])
              segments(complete[nrow(complete), "date"],
                       complete[nrow(complete), statistic],
                       current.wk$date,
                       current.wk[, statistic],
                       col = cbPalette[i])
            } else {
              points(current.wk.est$date, current.wk.est[, statistic], pch = 1,
                col = cbPalette[i])
              points(current.wk$date, current.wk[, statistic], pch = 0,
                col = cbPalette[i])
              segments(complete[nrow(complete), "date"],
                       complete[nrow(complete), statistic],
                       current.wk.est$date,
                       current.wk.est[, statistic],
                       col = cbPalette[i],
                       lty = "longdash")
              segments(complete[nrow(complete), "date"],
                       complete[nrow(complete), statistic],
                       current.wk$date,
                       current.wk[, statistic],
                       col = cbPalette[i],
                       lty = "dotted")
             }

            if (smooth) {
              smooth.data <- rbind(wk1.backdate, complete)
              if (weekdays(last.obs.date) == "Saturday") {
                smooth.data <- rbind(smooth.data, current.wk)
              }
              lines(stats::lowess(smooth.data[, vars], f = f),
                col = cbPalette[i])
            }
          }))

          legend(x = ip.legend.location,
                legend = c("Backdate", "Observed", "Estimate"),
                pch = c(8, 0, 1),
                bg = "white",
                cex = 2/3,
                title = NULL,
                bty = "n",
                lty = c("longdash", "dotted", "longdash"))

        } else {
          if (log.y) {
            if (any(dat$count == 0)) {
              zero.ct.pkg <- unique(dat[dat$count == 0, "package"])
              dat[dat$count == 0, "count"] <- 1
              for (p in zero.ct.pkg) {
                dat$cumulative <- cumsum(dat[dat$package == p, "count"])
              }
            }
            plot(dat[, vars], pch = NA, log = "y", xlim = xlim, ylim = ylim,
              main = ttl)
          } else {
            plot(dat[, vars], pch = NA, xlim = xlim, ylim = ylim, main = ttl)
          }

          invisible(lapply(seq_along(x$packages), function(i) {
            tmp <- dat[dat$package == x$packages[i], ]
            lines(tmp$date, tmp[, statistic], col = cbPalette[i])

            if (points) {
              points(tmp[, "date"], tmp[, statistic], col = cbPalette[i],
                pch = 16)
            }

            if (smooth) {
              lines(stats::lowess(dat[dat$package == x$packages[i], vars],
                f = f), col = cbPalette[i])
            }
          }))
        }

        id <- seq_along(x$packages)

        if (points) {
          legend(x = legend.location,
                 legend = x$packages,
                 col = cbPalette[id],
                 pch = NA,
                 bg = "white",
                 cex = 2/3,
                 title = NULL,
                 lwd = 1,
                 bty = "n")
        } else {
          legend(x = legend.location,
               legend = x$packages,
               col = cbPalette[id],
               pch = NA,
               bg = "white",
               cex = 2/3,
               title = NULL,
               lwd = 1,
               bty = "n")
        }
      }
    }
  } else if (graphics == "ggplot2") {
    if (obs.ct == 1) {
      p <- ggplot(data = dat, aes_string("count", y = "package"))
      if (log.y) {
        # p + scale_x_log10() + xlab("log10(count)") doesn't work!
        dat2 <- dat
        if (any(dat2$count == 0)) {
          zero.ct.pkg <- unique(dat2[dat2$count == 0, "package"])
          dat2[dat2$count == 0, "count"] <- 1
          for (p in zero.ct.pkg) {
            dat2$cumulative <- cumsum(dat2[dat2$package == p, "count"])
          }
        }
        dat2$count <- log10(dat2$count)
        p <- ggplot(data = dat2, aes_string(x = "count", y = "package")) +
             xlab(paste("log10", y.nm.case))
      }

      p <- p + geom_hline(yintercept = c(1, 2), linetype = "dotted") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

    } else if (obs.ct > 1) {
      if (log.y) {
        if (any(dat$count == 0)) {
          zero.ct.pkg <- unique(dat[dat$count == 0, "package"])
          dat[dat$count == 0, "count"] <- 1
          for (p in zero.ct.pkg) {
            dat$cumulative <- cumsum(dat[dat$package == p, "count"])
          }
        }
      }
      if (statistic == "count") {
        p <- ggplot(data = dat, aes_string(x = "date", y = "count",
          colour = "package")) + ggtitle("Package Download Counts")
      } else if (statistic == "cumulative") {
        p <- ggplot(data = dat, aes_string(x = "date", y = "cumulative",
          colour = "package")) + ggtitle("Cumulative Package Downloads")
      }

      if (any(dat$in.progress)) {
        g <- lapply(x$packages, function(pkg) {
          pkg.data <- dat[dat$package == pkg, ]
          ip.sel <- pkg.data$in.progress == TRUE
          ip.data <- pkg.data[ip.sel, ]
          complete <- pkg.data[!ip.sel, ]
          last.obs <- nrow(complete)

          obs.days <- as.numeric(format(last.obs.date , "%d"))
          exp.days <- as.numeric(format(lastDayMonth(ip.data$date)$date, "%d"))
          est.ct <- round(ip.data$count * exp.days / obs.days)

          est.data <- ip.data
          est.data$count <- est.ct
          last.cumulative <- complete[nrow(complete), "cumulative"]
          est.data$cumulative <- last.cumulative + est.ct

          list(ip.data = ip.data,
               complete = complete,
               est.data = est.data,
               est.seg = rbind(complete[last.obs, ], est.data),
               obs.seg = rbind(complete[last.obs, ], ip.data))
        })

        ip.data <- do.call(rbind, lapply(g, function(x) x$ip.data))
        complete <- do.call(rbind, lapply(g, function(x) x$complete))
        est.data <- do.call(rbind, lapply(g, function(x) x$est.data))
        est.seg <- do.call(rbind, lapply(g, function(x) x$est.seg))
        obs.seg <- do.call(rbind, lapply(g, function(x) x$obs.seg))

        p <- p + geom_line(data = complete, size = 1/3) +
          scale_shape_manual(name = "In-progress",
                             breaks = c("Observed", "Estimate"),
                             values = c("Observed" = 0, "Estimate" = 1)) +
          scale_linetype_manual(name = "In-progress",
                                breaks = c("Observed", "Estimate"),
                                values = c("Observed" = "dotted",
                                           "Estimate" = "longdash")) +
          geom_line(data = est.seg, size = 1/3, aes(linetype = "Estimate")) +
          geom_line(data = obs.seg, size = 1/3, aes(linetype = "Observed")) +
          geom_point(data = est.data, aes(shape = "Estimate")) +
          geom_point(data = ip.data, aes(shape = "Observed")) +
          theme(legend.position = "bottom",
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))

        if (points) p <- p + geom_point(data = complete)

      } else if (any(dat$partial)) {
        ggplot.data <- lapply(x$package, function(pkg) {
          pkg.dat <- dat[dat$package == pkg, ]
          unit.date <- pkg.dat$date

          wk1.start <- pkg.dat$date[1]
          wk1.end <- pkg.dat$date[2] - 1
          wk1 <- cranDownloads(pkg, from = wk1.start, to = wk1.end)

          if (weekdays(x$from) == "Sunday") {
            wk1.partial <- pkg.dat[pkg.dat$date == wk1.start, ]
            wk1.backdate <- wk1.partial
          } else {
            sel <- pkg.dat$partial & pkg.dat$date == wk1.start
            wk1.partial <- pkg.dat[sel, ]
            wk1.backdate <- wk1.partial

            wk1.backdate$count <- sum(wk1$cranlogs.data$count)
            wk1.backdate$cumulative <- wk1.backdate$count
            cumulative.recompute <- cumsum(c(wk1.backdate$cumulative,
              pkg.dat$count[-1]))
          }

          current.wk <- pkg.dat[nrow(pkg.dat), ]
          current.wk.est <- current.wk

          weekdays.elapsed <- as.integer(x$last.obs.date -
            unit.date[length(unit.date)] + 1)

          if (weekdays.elapsed != 0) { # monday exception
            current.wk.est$count <- 7L / weekdays.elapsed * current.wk$count
          } else {
            current.wk.est$count <- 7L * current.wk$count
          }

          if (weekdays(x$from) != "Sunday") {
            current.wk.est$cumulative <-
              cumulative.recompute[(nrow(pkg.dat) - 1)] + current.wk.est$count
            first.last <- c(1, nrow(pkg.dat))
            pkg.dat.recompute <- rbind(wk1.backdate, pkg.dat[-first.last, ],
              current.wk.est)
            pkg.dat.recompute$cumulative[-first.last] <-
              cumulative.recompute[-first.last]
            current.wk$cumulative <- current.wk$count +
              rev(cumulative.recompute[-first.last])[1]
          } else {
            pkg.dat.recompute <- pkg.dat
          }

          complete <- pkg.dat.recompute[-c(1, nrow(pkg.dat.recompute)), ]
          wk1.partial$date <- max(min(wk1$cranlogs.data$date), x$from)

          list(pkg.dat = pkg.dat,
               wk1.partial = wk1.partial,
               wk1.backdate = wk1.backdate,
               current.wk = current.wk,
               current.wk.est = current.wk.est,
               pkg.dat.recompute = pkg.dat.recompute,
               complete = complete,
               wk1.backdate.seg = rbind(complete[1, ], wk1.backdate),
               wk1.partial.seg = rbind(complete[1, ], wk1.partial),
               current.wk.seg = rbind(complete[nrow(complete), ], current.wk),
               current.wk.est.seg = rbind(complete[nrow(complete), ],
                 current.wk.est),
               pkg.dat.recompute = pkg.dat.recompute)
        })

        complete <- do.call(rbind, lapply(ggplot.data, function(x) x$complete))
        wk1.partial <- do.call(rbind, lapply(ggplot.data, function(x)
          x$wk1.partial))
        wk1.backdate <- do.call(rbind, lapply(ggplot.data, function(x)
          x$wk1.backdate))
        current.wk <- do.call(rbind, lapply(ggplot.data, function(x)
          x$current.wk))
        current.wk.est <- do.call(rbind, lapply(ggplot.data, function(x)
          x$current.wk.est))
        wk1.backdate.seg <- do.call(rbind, lapply(ggplot.data, function(x)
          x$wk1.backdate.seg))
        wk1.partial.seg <- do.call(rbind, lapply(ggplot.data, function(x)
          x$wk1.partial.seg))
        current.wk.seg <- do.call(rbind, lapply(ggplot.data,
          function(x) x$current.wk.seg))
        current.wk.est.seg <- do.call(rbind, lapply(ggplot.data,
          function(x) x$current.wk.est.seg))

        p <- p + geom_line(data = complete, size = 1/3) +
          scale_linetype_manual(name = "",
                                breaks = c("Backdate",
                                           "Partial/In-Progress",
                                           "Estimate"),
                                values = c("Backdate" = "dashed",
                                           "Partial/In-Progress" = "dotted",
                                           "Estimate" = "dashed")) +
          scale_shape_manual(name = "",
                             breaks = c("Backdate",
                                        "Partial/In-Progress",
                                        "Estimate"),
                             values = c("Backdate" = 8,
                                        "Partial/In-Progress" = 0,
                                        "Estimate" = 1))

        if (weekdays(last.obs.date) == "Saturday") {
          p <- p +
            geom_line(data = current.wk.seg, size = 1/3) +
            geom_point(data = current.wk, shape = 16)
        } else {
          p <- p +
            geom_line(data = current.wk.seg, size = 1/3,
              aes(linetype = "Partial/In-Progress")) +
            geom_point(data = current.wk, aes(shape = "Partial/In-Progress")) +
            geom_line(data = current.wk.est.seg, size = 1/3,
              aes(linetype = "Estimate")) +
            geom_point(data = current.wk.est, size = 1.5,
              aes(shape = "Estimate"))
        }

        if (weekdays(x$from) == "Sunday") {
          p <- p +
            geom_line(data = wk1.partial.seg, size = 1/3) +
            geom_point(data = wk1.partial)
        } else {
          p <- p +
            geom_line(data = wk1.backdate.seg, size = 1/3,
              aes(linetype = "Backdate")) +
            geom_point(data = wk1.backdate, aes(shape = "Backdate")) +
            geom_line(data = wk1.partial.seg, size = 1/3,
              aes(linetype = "Partial/In-Progress")) +
            geom_point(data = wk1.partial,
              aes(shape = "Partial/In-Progress"))
        }

        if (points) p <- p + geom_point(data = complete)

      } else {
        p <- p + geom_line(size = 1/3) +
          theme(legend.position = "bottom",
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))

        if (points) p <- p + geom_point()
      }

      if (log.y) p <- p + scale_y_log10() + ylab(paste("log10", statistic))

      if (smooth) {
        if (smooth) {
          if (any(dat$in.progress)) {
            smooth.data <- complete
            p <- p + geom_smooth(data = smooth.data, method = "loess",
              formula = "y ~ x", se = se, span = span)
          } else if (any(dat$partial)) {
            smooth.data <- rbind(complete, wk1.backdate)
            if (weekdays(last.obs.date) == "Saturday") {
              smooth.data <- rbind(smooth.data, current.wk)
            }
            p <- p + geom_smooth(data = smooth.data, method = "loess",
              formula = "y ~ x", se = se, span = span)
          } else {
            p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se,
              span = span)
          }
        }
      }
      p <- p + theme_bw() +
        theme(legend.position = "bottom",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5))

      suppressWarnings(print(p))
    }
  }
}

rPlot <- function(x, statistic, graphics, obs.ct, legend.location,
  ip.legend.location, points, log.y, smooth, se, r.version, f, span,
  multi.plot) {

  dat <- x$cranlogs.data
  ylab <- tools::toTitleCase(statistic)
  last.obs.date <- x$last.obs.date
  type <- ifelse(points, "o", "l")

  if (obs.ct == 1) {
    if (graphics == "base") {
      if (log.y) {
        dotchart(log10(dat$count), labels = dat$platform,
          xlab = paste("log10", statistic),
          main = paste("R Downloads:", unique(dat$date)))
      } else {
        dotchart(dat$count, labels = dat$platform, xlab = "Count",
          main = paste("R Downloads:", unique(dat$date)))
      }
    } else if (graphics == "ggplot2") {
      if (log.y) {
        dat2 <- dat
        dat2$count <- log10(dat2$count)
        p <- ggplot(data = dat2, aes_string(x = "count", y = "platform")) +
          geom_point(size = 2) +
          xlab(paste("log10", ylab))
      } else {
        p <- ggplot(data = dat, aes_string(x = "count", y = "platform")) +
          geom_point(size = 2)
      }

      p + theme_bw() +
        ggtitle(paste("R Downloads:", unique(dat$date))) +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5))
    }

  } else if (obs.ct > 1) {
    if (graphics == "base") {
      pltfrm.col <- c("red", "dodgerblue", "black")
      names(pltfrm.col) <- c("osx", "src", "win")

      if (any(dat$in.progress)) {
        pltfrm <- sort(unique(dat$platform))

        p.data <- lapply(pltfrm, function(x) {
          pkg.dat <- dat[dat$platform == x, ]
          ip.sel <- pkg.dat$in.progress == TRUE
          ip.data <- pkg.dat[ip.sel, ]
          complete <- pkg.dat[!ip.sel, ]
          obs.days <- as.numeric(format(last.obs.date , "%d"))
          exp.days <- as.numeric(format(lastDayMonth(ip.data$date)$date, "%d"))
          est.ct <- round(ip.data$count * exp.days / obs.days)
          est.data <- ip.data
          est.data$count <- est.ct
          last.cumulative <- complete[nrow(complete), "cumulative"]
          est.data$cumulative <- last.cumulative + est.ct
          list(ip.data = ip.data, complete = complete,
            est.data = est.data)
        })

        est.stat <- vapply(p.data, function(x) x$est.data[, statistic],
          numeric(1L))
        complete <- lapply(p.data, function(x) x$complete)
        est.data <- lapply(p.data, function(x) x$est.data)
        ip.data <- lapply(p.data, function(x) x$ip.data)

        last.obs <- unique(vapply(complete, nrow, integer(1L)))
        ylim <- range(c(dat[, statistic], est.stat))

        if (log.y) {
          plot(dat$date, dat[, statistic], pch = NA, xlab = "Date",
            ylab = paste("log10", ylab), ylim = ylim, log = "y")
        } else {
          plot(dat$date, dat[, statistic], pch = NA, xlab = "Date", ylab = ylab,
            ylim = ylim)
        }

        if (points) {
          invisible(lapply(seq_along(complete), function(i) {
            tmp <- complete[[i]]
            points(tmp[, "date"], tmp[, statistic], col = pltfrm.col[i],
              pch = 16)
          }))
        }

        invisible(lapply(seq_along(complete), function(i) {
          tmp <- complete[[i]]
          lines(tmp$date, tmp[, statistic], type = type, col = pltfrm.col[i])
        }))

        invisible(lapply(seq_along(est.data), function(i) {
          tmp <- est.data[[i]]
          points(tmp[, "date"], tmp[, statistic], col = pltfrm.col[i], pch = 1)
        }))

        invisible(lapply(seq_along(ip.data), function(i) {
          tmp <- ip.data[[i]]
          points(tmp[, "date"], tmp[, statistic], col = pltfrm.col[i], pch = 0)
        }))

        invisible(lapply(seq_along(complete), function(i) {
          tmpA <- complete[[i]]
          tmpB <- ip.data[[i]]
          segments(tmpA[last.obs, "date"], tmpA[last.obs, statistic],
                   tmpB$date, tmpB[, statistic], lty = "dotted")
        }))

        invisible(lapply(seq_along(complete), function(i) {
          tmpA <- complete[[i]]
          tmpB <- est.data[[i]]
          segments(tmpA[last.obs, "date"], tmpA[last.obs, statistic], tmpB$date,
                   tmpB[, statistic], lty = "longdash", col = pltfrm.col[i])
        }))

        if (points) {
          legend(x = legend.location,
                 legend = c("win", "mac", "src"),
                 col = c("black", "red", "dodgerblue"),
                 pch = rep(16, 3),
                 bg = "white",
                 cex = 2/3,
                 title = NULL,
                 lwd = 1,
                 bty = "n")
        } else {
          legend(x = legend.location,
                 legend = c("win", "mac", "src"),
                 col = c("black", "red", "dodgerblue"),
                 bg = "white",
                 cex = 2/3,
                 title = NULL,
                 lwd = 1,
                 bty = "n")
        }

        legend(x = ip.legend.location,
               legend = c("Est", "Obs"),
               pch = 1:0,
               bg = "white",
               cex = 2/3,
               title = NULL,
               lty = c("longdash", "dotted"),
               bty = "n")

      } else if (any(dat$partial)) {
        pltfrm <- unique(dat$platform)
        platform.data <- split(dat, dat$platform)

        unit.date <- unique(dat$date)

        wk1.start <- unit.date[1]
        wk1.end <- unit.date[2] - 1
        wk1 <- cranDownloads(x$packages, from = wk1.start, to = wk1.end)

        wk1.partial <- do.call(rbind, lapply(platform.data, function(x)
          x[x$date == wk1.start, ]))
        wk1.backdate <- wk1.partial

        if (weekdays(x$from) != "Sunday") {
          wk1.backdate$count <- tapply(wk1$cranlogs.data$count,
            wk1$cranlogs.data$platform, sum)
        }

        current.wk <- do.call(rbind, lapply(platform.data, function(x)
          x[x$date == unit.date[length(unit.date)], ]))

        current.wk.est <- current.wk

        weekdays.elapsed <- as.integer(last.obs.date -
          unit.date[length(unit.date)] + 1)

        current.wk.est$count <- vapply(pltfrm, function(p) {
          p.data <- current.wk[current.wk$platform == p, ]
          if (weekdays.elapsed != 0) {
            7L / weekdays.elapsed * p.data$count
          } else {
            7L * p.data$count
          }
        }, numeric(1L))

        if (weekdays(x$from) != "Sunday") {
          first.last <- c(1, nrow(platform.data[[1]]))

          platform.recompute <- lapply(pltfrm, function(p) {
            tmp <- rbind(wk1.backdate[wk1.backdate$platform == p, ],
                         platform.data[[p]][-first.last, ],
                         current.wk.est[current.wk.est$platform == p, ])
            tmp$cumulative <- cumsum(tmp$count)
            tmp
          })

          names(platform.recompute) <- pltfrm

          current.wk.est <- do.call(rbind, lapply(platform.recompute,
            function(x) x[nrow(x), ]))
        } else {
          platform.recompute <- platform.data
        }

        if (weekdays.elapsed == 7) {
          complete <- lapply(platform.recompute, function(x) {
            x[x$date != min(x$date), ]
          })
        } else {
          complete <- lapply(platform.recompute, function(x) x[!x$partial, ])
        }

        wk1.partial$date <- max(min(wk1$cranlogs.data$date), x$from)

        range.data <- rbind(do.call(rbind, complete), wk1.partial,
          wk1.backdate, current.wk, current.wk.est)
        xlim <- range(range.data$date)
        ylim <- range(range.data[, statistic])

        if (log.y) {
          plot(dat$date, dat[, statistic], pch = NA, xlab = "Date",
            ylab = paste("log10", ylab), ylim = ylim, log = "y")
        } else {
          plot(dat$date, dat[, statistic], pch = NA, xlab = "Date", ylab = ylab,
            ylim = ylim)
        }

        if (points) {
          invisible(lapply(seq_along(complete), function(i) {
            tmp <- complete[[i]]
            points(tmp$date, tmp[, statistic], col = pltfrm.col[i], pch = 16)
          }))
        }

        invisible(lapply(seq_along(complete), function(i) {
          tmp <- complete[[i]]
          lines(tmp$date, tmp[, statistic], type = type, col = pltfrm.col[i])
        }))

        vars <- c("date", statistic)

        if (weekdays(x$from) == "Sunday") {
          points(wk1.partial[, vars], pch = 16, col = pltfrm.col)
          invisible(lapply(seq_along(complete), function(i) {
            tmp <- complete[[i]][1, ]
            segments(tmp$date, tmp[, statistic],
                     wk1.partial[i, "date"], wk1.partial[i, statistic],
                     col = pltfrm.col[i])
           }))
        } else {
          points(wk1.partial[, vars], pch = 0, col = pltfrm.col)
          points(wk1.backdate[, vars], pch = 8, col = pltfrm.col)
          invisible(lapply(seq_along(complete), function(i) {
            tmp <- complete[[i]][1, ]
            segments(tmp$date, tmp[, statistic],
                     wk1.partial[i, "date"], wk1.partial[i, statistic],
                     lty = "dotted", col = pltfrm.col[i])
            segments(tmp[1, "date"], tmp[1, statistic],
                     wk1.backdate[i, "date"], wk1.backdate[i, statistic],
                     lty = "dashed", col = pltfrm.col[i])
          }))
        }

        if (weekdays(last.obs.date) != "Saturday") {
          points(current.wk.est[, vars], col = pltfrm.col)
          points(current.wk[, vars], col = pltfrm.col, pch = 0)
          invisible(lapply(seq_along(complete), function(i) {
            tmp <- complete[[i]][nrow(complete[[i]]), ]
            segments(tmp$date, tmp[, statistic],
                     current.wk[i, "date"], current.wk[i, statistic],
                     lty = "dotted", col = pltfrm.col[i])
            segments(tmp$date, tmp[, statistic],
                     current.wk.est[i, "date"], current.wk.est[i, statistic],
                     lty = "dashed", col = pltfrm.col[i])
          }))
        }

        if (points) {
          legend(x = legend.location,
                 legend = c("win", "mac", "src"),
                 col = c("black", "red", "dodgerblue"),
                 pch = rep(16, 3),
                 bg = "white",
                 cex = 2/3,
                 title = NULL,
                 lwd = 1,
                 bty = "n")
        } else {
          legend(x = legend.location,
                 legend = c("win", "mac", "src"),
                 col = c("black", "red", "dodgerblue"),
                 bg = "white",
                 cex = 2/3,
                 title = NULL,
                 lwd = 1,
                 bty = "n")
        }

        legend(x = ip.legend.location,
               legend = c("Backdate", "Est", "Obs"),
               pch = c(8, 1, 0),
               bg = "white",
               cex = 2/3,
               title = NULL,
               bty = "n")

      } else {
        if (log.y) {
          plot(dat[dat$platform == "win", "date"],
               dat[dat$platform == "win", statistic],
               pch = NA, ylim = range(dat[, statistic]),
               xlab = "Date", ylab = paste("log10", ylab), log = "y")
        } else {
          plot(dat[dat$platform == "win", "date"],
               dat[dat$platform == "win", statistic],
               pch = NA, ylim = range(dat[, statistic]),
               xlab = "Date", ylab = ylab)
        }

        pltfrm <- sort(unique(dat$platform))
        pltfrm.col <- c("red", "dodgerblue", "black")
        names(pltfrm.col) <- c("osx", "src", "win")

        if (points) {
          invisible(lapply(seq_along(pltfrm), function(i) {
            points(dat[dat$platform == pltfrm[i], "date"],
                  dat[dat$platform == pltfrm[i], statistic],
                  pch = 16, col = pltfrm.col[i])
          }))
        }

        invisible(lapply(seq_along(pltfrm), function(i) {
          lines(dat[dat$platform == pltfrm[i], "date"],
                dat[dat$platform == pltfrm[i], statistic],
                type = type, col = pltfrm.col[i])
        }))

        if (points) {
          legend(x = legend.location,
                 legend = c("win", "mac", "src"),
                 col = c("black", "red", "dodgerblue"),
                 pch = rep(16, 8),
                 bg = "white",
                 cex = 2/3,
                 title = "Platform",
                 lwd = 1)
        } else {
          legend(x = legend.location,
                 legend = c("win", "mac", "src"),
                 col = c("black", "red", "dodgerblue"),
                 bg = "white",
                 cex = 2/3,
                 title = "Platform",
                 lwd = 1)
        }
      }

      if (smooth) {
        if (any(dat$in.progress)) {
          invisible(lapply(seq_along(complete), function(i) {
            tmp <- complete[[i]]
            smooth.data <- stats::lowess(tmp$date, tmp[, statistic])
            lines(smooth.data, lty = "solid", lwd = 1.5, col = pltfrm.col[i])
          }))
        } else if (any(dat$partial)) {
          invisible(lapply(seq_along(complete), function(i) {
            tmp <- rbind(complete[[i]], wk1.backdate[i, ])
            smooth.data <- stats::lowess(tmp$date, tmp[, statistic])
            lines(smooth.data, lty = "solid", lwd = 1.5, col = pltfrm.col[i])
          }))
        } else {
          invisible(lapply(seq_along(pltfrm), function(i) {
            sel <- dat$platform == pltfrm[i]
            smooth.data <- stats::lowess(dat[sel, "date"], dat[sel, statistic],
              f = f)
            lines(smooth.data, lty = "solid", lwd = 1.5, col = pltfrm.col[i])
          }))
        }
      }

      if (r.version) {
        r_v <- rversions::r_versions()
        axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
          cex.axis = 2/3, padj = 0.9)
      }

      title(main = "R Downloads")

    } else if (graphics == "ggplot2") {
      if (statistic == "count") {
        if (multi.plot) {
          p <- ggplot(data = dat, aes_string("date", "count",
            colour = "platform"))
        } else {
          p <- ggplot(data = dat, aes_string("date", "count")) +
            facet_wrap(~ platform, nrow = 2)
        }
      } else if (statistic == "cumulative") {
        if (multi.plot) {
          p <- ggplot(data = dat, aes_string("date", "cumulative",
            colour = "platform"))
        } else {
          p <- ggplot(data = dat, aes_string("date", "cumulative")) +
            facet_wrap(~ platform, nrow = 2)
        }
      }

      if (any(dat$in.progress)) {
        pltfrm <- sort(unique(dat$platform))
        p.data <- lapply(seq_along(pltfrm), function(i) {
          pkg.dat <- dat[dat$platform == pltfrm[i], ]
          ip.sel <- pkg.dat$in.progress == TRUE
          ip.data <- pkg.dat[ip.sel, ]
          complete <- pkg.dat[!ip.sel, ]
          last.obs <- nrow(complete)
          obs.days <- as.numeric(format(last.obs.date , "%d"))
          exp.days <- as.numeric(format(lastDayMonth(ip.data$date)$date, "%d"))
          est.ct <- round(ip.data$count * exp.days / obs.days)
          est.data <- ip.data
          est.data$count <- est.ct

          last.cumulative <- complete[nrow(complete), "cumulative"]
          est.data$cumulative <- last.cumulative + est.ct

          list(ip.data = ip.data,
               complete = complete,
               est.data = est.data,
               est.seg = rbind(complete[last.obs, ], est.data),
               obs.seg = rbind(complete[last.obs, ], ip.data))
        })

        est.stat <- vapply(p.data, function(x) {
          x$est.data[, statistic]
        }, numeric(1L))

        complete <- do.call(rbind, lapply(p.data, function(x) x$complete))
        est.data <- do.call(rbind, lapply(p.data, function(x) x$est.data))
        obs.data <- do.call(rbind, lapply(p.data, function(x) x$ip.data))

        est.seg <- do.call(rbind, lapply(p.data, function(x) x$est.seg))
        obs.seg <- do.call(rbind, lapply(p.data, function(x) x$obs.seg))

        p <- p + geom_line(data = complete, size = 1/3)

        if (multi.plot) {
          p <- p +
            scale_shape_manual(name = "In-progress",
                               breaks = c("Observed", "Estimate"),
                               values = c("Observed" = 0, "Estimate" = 1)) +
            scale_linetype_manual(name = "In-progress",
                                  breaks = c("Observed", "Estimate"),
                                  values = c("Observed" = "dotted",
                                             "Estimate" = "solid")) +
            geom_line(data = est.seg, aes(linetype = "Estimate")) +
            geom_line(data = obs.seg, aes(linetype = "Observed")) +
            geom_point(data = est.data, aes(shape = "Estimate")) +
            geom_point(data = obs.data, aes(shape = "Observed"))
        } else {
          p <- p +
            scale_color_manual(name = "In-progress",
                               breaks = c("Observed", "Estimate"),
                               values = c("Observed" = "black",
                                          "Estimate" = "red")) +
            scale_shape_manual(name = "In-progress",
                               breaks = c("Observed", "Estimate"),
                               values = c("Observed" = 0, "Estimate" = 1)) +
            scale_linetype_manual(name = "In-progress",
                                  breaks = c("Observed", "Estimate"),
                                  values = c("Observed" = "dotted",
                                             "Estimate" = "solid")) +
            geom_line(data = est.seg,
              aes(colour = "Estimate", linetype = "Estimate")) +
            geom_line(data = obs.seg,
              aes(col = "Observed", linetype = "Observed")) +
            geom_point(data = est.data,
              aes(colour = "Estimate", shape = "Estimate")) +
            geom_point(data = obs.data,
              aes(colour = "Observed", shape = "Observed"))
        }

        if (points) p <- p + geom_point(data = complete)
        if (log.y) p <- p + scale_y_log10() + ylab(paste("log10", ylab))

        p <- p + theme_bw() +
          ggtitle("R Downloads") +
          theme(legend.position = "bottom",
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))


      } else if (any(dat$partial)) {
        pltfrm <- unique(dat$platform)

        platform.data <- lapply(split(dat, dat$platform), function(x)
          x[order(x$date), ])

        unit.date <- unique(dat$date)

        wk1.start <- unit.date[1]
        wk1.end <- unit.date[2] - 1
        wk1 <- cranDownloads(x$packages, from = wk1.start, to = wk1.end)

        wk1.partial <- do.call(rbind, lapply(platform.data, function(x)
          x[x$date == wk1.start, ]))

        wk1.backdate <- wk1.partial

        if (weekdays(x$from) != "Sunday") {
          wk1.backdate$count <- tapply(wk1$cranlogs.data$count,
            wk1$cranlogs.data$platform, sum)
        }

        current.wk <- do.call(rbind, lapply(platform.data, function(x)
          x[x$date == unit.date[length(unit.date)], ]))

        current.wk.est <- current.wk

        weekdays.elapsed <- as.integer(last.obs.date -
          unit.date[length(unit.date)] + 1)

        current.wk.est$count <- vapply(pltfrm, function(p) {
          p.data <- current.wk[current.wk$platform == p, ]
          if (weekdays.elapsed != 0) {
            7L / weekdays.elapsed * p.data$count
          } else {
            7L * p.data$count
          }
        }, numeric(1L))

        if (weekdays(x$from) != "Sunday") {
          first.last <- c(1, nrow(platform.data[[1]]))

          platform.recompute <- lapply(pltfrm, function(p) {
            tmp <- rbind(wk1.backdate[wk1.backdate$platform == p, ],
                         platform.data[[p]][-first.last, ],
                         current.wk.est[current.wk.est$platform == p, ])
            tmp$cumulative <- cumsum(tmp$count)
            tmp
          })

          names(platform.recompute) <- pltfrm

          current.wk.est <- do.call(rbind, lapply(platform.recompute,
            function(x) x[nrow(x), ]))
        } else {
          platform.recompute <- platform.data
        }

        complete <- do.call(rbind, lapply(platform.recompute, function(x)
          x[!x$partial, ]))

        wk1.partial <- do.call(rbind, lapply(pltfrm, function(p) {
          tmp <- wk1$cranlogs.data
          wk1.date <- tmp[tmp$platform == p & tmp$date == min(tmp$date), "date"]
          wk1.tmp <- wk1.partial[wk1.partial$platform == p,  ]
          wk1.tmp$date <- max(wk1.date, x$from)
          wk1.tmp
        }))

        wk1.partial.seg <- do.call(rbind, lapply(pltfrm, function(p) {
          sel <- complete$platform == p & complete$date == min(complete$date)
          rbind(complete[sel, ], wk1.partial[wk1.partial$platform == p, ])
        }))

        wk1.backdate.seg <- do.call(rbind, lapply(pltfrm, function(p) {
          sel <- complete$platform == p & complete$date == min(complete$date)
          rbind(complete[sel, ], wk1.backdate[wk1.backdate$platform == p, ])
        }))

        current.wk.seg <- do.call(rbind, lapply(pltfrm, function(p) {
          sel <- complete$platform == p & complete$date == max(complete$date)
          rbind(complete[sel, ], current.wk[current.wk$platform == p, ])
        }))

        current.wk.est.seg <- do.call(rbind, lapply(pltfrm, function(p) {
          sel <- complete$platform == p & complete$date == max(complete$date)
          rbind(complete[sel, ], current.wk.est[current.wk.est$platform == p, ])
        }))

        if (multi.plot) {
          p <- p + geom_line(data = complete, size = 1/3) +
            scale_linetype_manual(name = "",
                                  breaks = c("Backdate",
                                             "Partial/In-Progress",
                                             "Observed"),
                                  values = c("Backdate" = "longdash",
                                             "Partial/In-Progress" = "longdash",
                                             "Observed" = "dotted")) +
            scale_shape_manual(name = "",
                               breaks = c("Backdate",
                                          "Partial/In-Progress",
                                          "Observed"),
                               values = c("Backdate" = 8,
                                          "Partial/In-Progress" = 1,
                                          "Observed" = 0))
        } else {
          p <- p + geom_line(data = complete, size = 1/3) +
            scale_colour_manual(name = "",
                                breaks = c("Backdate",
                                          "Partial/In-Progress",
                                          "Observed"),
                                values = c("Backdate" = "dodgerblue",
                                           "Partial/In-Progress" = "red",
                                           "Observed" = "gray")) +
            scale_linetype_manual(name = "",
                                  breaks = c("Backdate",
                                             "Partial/In-Progress",
                                             "Observed"),
                                  values = c("Backdate" = "longdash",
                                             "Partial/In-Progress" = "longdash",
                                             "Observed" = "dotted")) +
            scale_shape_manual(name = "",
                               breaks = c("Backdate",
                                          "Partial/In-Progress",
                                          "Observed"),
                               values = c("Backdate" = 8,
                                          "Partial/In-Progress" = 1,
                                          "Observed" = 0))
        }

        if (weekdays(last.obs.date) == "Saturday") {
          p <- p +
            geom_line(data = current.wk.seg, size = 1/3) +
            geom_point(data = current.wk)
        } else {
          if (multi.plot) {
            p <- p +
              geom_line(data = current.wk.est.seg, size = 1/3,
                aes(linetype = "Partial/In-Progress")) +
              geom_point(data = current.wk.est, size = 1.5,
                aes(shape = "Partial/In-Progress")) +
              geom_line(data = current.wk.seg, size = 1/3,
                aes(linetype = "Observed")) +
              geom_point(data = current.wk, aes(shape = "Observed"))
          } else {
            p <- p +
              geom_line(data = current.wk.est.seg, size = 1/3,
                aes(colour = "Partial/In-Progress",
                    linetype = "Partial/In-Progress")) +
              geom_point(data = current.wk.est, size = 1.5,
                aes(colour = "Partial/In-Progress",
                    shape = "Partial/In-Progress")) +
              geom_line(data = current.wk.seg, size = 1/3,
                aes(colour = "Observed", linetype = "Observed")) +
              geom_point(data = current.wk,
                aes(colour = "Observed", shape = "Observed"))
          }
        }

        if (weekdays(x$from) == "Sunday") {
          p <- p +
            geom_line(data = wk1.partial.seg, size = 1/3) +
            geom_point(data = wk1.partial)
        } else {
          if (multi.plot) {
            p <- p +
              geom_line(data = wk1.backdate.seg, size = 1/3,
                aes(linetype = "Backdate")) +
              geom_point(data = wk1.backdate, aes(shape = "Backdate")) +
              geom_line(data = wk1.partial.seg, size = 1/3,
                aes(linetype = "Observed")) +
              geom_point(data = wk1.partial, aes(shape = "Observed"))
          } else {
            p <- p +
              geom_line(data = wk1.backdate.seg, size = 1/3,
                aes(colour = "Backdate", linetype = "Backdate")) +
              geom_point(data = wk1.backdate,
                aes(shape = "Backdate", colour = "Backdate")) +
              geom_line(data = wk1.partial.seg, size = 1/3,
                aes(colour = "Observed", linetype = "Observed")) +
              geom_point(data = wk1.partial,
                aes(colour = "Observed", shape = "Observed"))
          }
        }

        if (points) p <- p + geom_point(data = complete)

      } else {
        p <- p + geom_line(size = 0.5) +
          ggtitle("R Downloads") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))

        if (points) p <- p + geom_point()
        if (log.y) p <- p + scale_y_log10() + ylab(paste("log10", ylab))
        if (!multi.plot) p <- p + facet_wrap(~ platform, nrow = 2)
      }

      if (log.y) p <- p + scale_y_log10() + ylab(paste("log10", ylab))
      if (smooth) {
        if (any(dat$in.progress)) {
          smooth.data <- complete
          p <- p + geom_smooth(data = smooth.data, method = "loess",
            formula = "y ~ x", se = se, span = span)
        } else if (any(dat$partial)) {
          smooth.data <- rbind(wk1.backdate, complete)
          if (weekdays(last.obs.date) == "Saturday") {
            smooth.data <- rbind(smooth.data, current.wk)
          }
          p <- p + geom_smooth(data = smooth.data, method = "loess",
            formula = "y ~ x", se = se, span = span)
        } else {
          p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se,
            span = span)
        }
      }

      p <- p + theme_bw() +
        ggtitle("Total R Downloads") +
        theme(legend.position = "bottom",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5))

      suppressWarnings(print(p))
    }
  }
}

rTotPlot <- function(x, statistic, graphics,  obs.ct, legend.location, points,
  log.y, smooth, se, r.version, f, span) {

  dat <- x$cranlogs.data
  last.obs.date <- x$last.obs.date
  ct <- tapply(dat$count, dat$date, sum)

  if (any(dat$in.progress)) {
    dat <- data.frame(date = unique(dat$date),
                      count = ct,
                      cumulative = cumsum(ct),
                      in.progress = dat[dat$platform == "win", "in.progress"],
                      row.names = NULL)
  } else if (any(dat$partial)) {
    dat <- data.frame(date = unique(dat$date),
                      count = ct,
                      cumulative = cumsum(ct),
                      partial = dat[dat$platform == "win", "partial"],
                      row.names = NULL)
  } else {
    dat <- data.frame(date = unique(dat$date),
                      count = ct,
                      cumulative = cumsum(ct),
                      row.names = NULL)
  }

  y.nm.case <- tools::toTitleCase(statistic)

  if (obs.ct == 1) {
    if (graphics == "base") {
      if (log.y) {
        dotchart(log10(dat$count), labels = dat$platform,
          xlab = paste("log10", y.nm.case),
          main = paste("R Downloads:", unique(dat$date)))
        mtext("R", side = 2, las = 1, line = 1)
      } else {
        dotchart(dat$count, labels = dat$platform, xlab = "Count",
          main = paste("R Downloads:", unique(dat$date)))
        mtext("R", side = 2, las = 1, line = 1)
      }
    } else if (graphics == "ggplot2") {
      dat$platform <-  "R"
      if (log.y) {
        dat2 <- dat
        dat2$count <- log10(dat2$count)
        p <- ggplot(data = dat2, aes_string(x = "count", y = "platform")) +
          geom_point(size = 2) + xlab(paste("log10", y.nm.case)) + ylab(NULL)
      } else {
        p <- ggplot(data = dat, aes_string("count", "platform")) +
          geom_point(size = 2) + ylab(NULL)
      }

      p + theme_bw() +
        ggtitle(paste("R Downloads:", unique(dat$date))) +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5))
    }

  } else if (obs.ct > 1) {
    if (graphics == "base") {
      type <- ifelse(points, "o", "l")
      vars <- c("date", statistic)

      if (any(dat$in.progress)) {
        ip.sel <- dat$in.progress == TRUE
        ip.data <- dat[ip.sel, ]
        complete <- dat[!ip.sel, ]
        last.obs <- nrow(complete)

        obs.days <- as.numeric(format(last.obs.date , "%d"))
        exp.days <- as.numeric(format(lastDayMonth(ip.data$date)$date, "%d"))
        est.ct <- round(ip.data$count * exp.days / obs.days)

        est.data <- ip.data
        est.data$count <- est.ct
        last.cumulative <- complete[nrow(complete), "cumulative"]
        est.data$cumulative <- last.cumulative + est.ct

        if (statistic == "count") {
          ylim <- range(c(dat[, statistic], est.data$count))
        } else if (statistic == "cumulative") {
          ylim <- range(c(dat[, statistic], est.data$cumulative))
        }

        xlim <- range(dat$date)

        if (log.y) {
          plot(complete[, vars], type = type, xlab = "Date",
            ylab = paste0("log10 ", y.nm.case), xlim = xlim, ylim = ylim,
            log = "y", pch = 16)
        } else {
          plot(complete[, vars], type = type, xlab = "Date", ylab = y.nm.case,
            xlim = xlim, ylim = ylim, pch = 16)
        }

        points(ip.data[, vars], col = "black", pch = 0)
        points(est.data[, vars], col = "red", pch = 1)

        segments(complete[last.obs, "date"],
                 complete[last.obs, statistic],
                 ip.data$date,
                 ip.data[, statistic],
                 lty = "dotted")
        segments(complete[last.obs, "date"],
                 complete[last.obs, statistic],
                 est.data$date,
                 est.data[, statistic],
                 col = "red")

        axis(4, at = ip.data[, statistic], labels = "obs")
        axis(4, at = est.data[, statistic], labels = "est", col.axis = "red",
          col.ticks = "red")

      } else if (any(dat$partial)) {
        unit.date <- dat$date

        wk1.start <- dat$date[1]
        wk1.end <- dat$date[2] - 1
        wk1 <- cranDownloads(x$packages, from = wk1.start, to = wk1.end)

        if (weekdays(x$from) == "Sunday") {
          wk1.partial <- dat[dat$date == wk1.start, ]
          wk1.backdate <- wk1.partial
        } else {
          sel <- dat$partial & dat$date == wk1.start
          wk1.partial <- dat[sel, ]
          wk1.backdate <- wk1.partial
          wk1.backdate$count <- sum(wk1$cranlogs.data$count)
          wk1.backdate$cumulative <- wk1.backdate$count
          cumulative.recompute <- cumsum(c(wk1.backdate$cumulative,
            dat$count[-1]))
        }

        current.wk <- dat[nrow(dat), ]
        current.wk.est <- current.wk

        weekdays.elapsed <- as.integer(x$last.obs.date -
          unit.date[length(unit.date)] + 1)

        if (as.integer(weekdays.elapsed) != 0) { # monday exception
          current.wk.est$count <- 7L / weekdays.elapsed * current.wk$count
        } else {
          current.wk.est$count <- 7L * current.wk$count
        }

        if (weekdays(x$from) != "Sunday") {
          current.wk.est$cumulative <-
            cumulative.recompute[(nrow(dat) - 1)] + current.wk.est$count
          first.last <- c(1, nrow(dat))
          dat.recompute <- rbind(wk1.backdate, dat[-first.last, ],
            current.wk.est)
          dat.recompute$cumulative[-first.last] <-
            cumulative.recompute[-first.last]
          current.wk$cumulative <- current.wk$count +
            rev(cumulative.recompute[-first.last])[1]
        } else {
          dat.recompute <- dat
        }

        if (weekdays.elapsed == 7) {
          sel <- dat.recompute$date != min(dat.recompute$date)
          complete <- dat.recompute[sel, ]
        } else {
          complete <- dat.recompute[!dat.recompute$partial, ]
        }

        wk1.partial$date <- max(min(wk1$cranlogs.data$date), x$from)

        xlim <- range(dat$date)
        ylim.data <- rbind(dat, dat.recompute)
        ylim <- range(ylim.data[, statistic])

        if (log.y) {
          plot(complete[, vars], type = type, xlab = "Date",
            ylab = paste0("log10 ", y.nm.case), xlim = xlim, ylim = ylim,
            pch = 16, log = "y")
        } else {
          plot(complete[, vars], type = type, xlab = "Date", ylab = y.nm.case,
            xlim = xlim, ylim = ylim, pch = 16)
        }

        if (weekdays(x$from) == "Sunday") {
          points(wk1.partial[, vars], pch = 16)
          segments(wk1.partial$date, wk1.partial[, statistic],
                   complete[1, "date"], complete[1, statistic])
        } else {
          points(wk1.backdate[, vars], col = "dodgerblue", pch = 8)
          points(wk1.partial[, vars], pch = 0, col = "gray")
          segments(wk1.backdate$date, wk1.backdate[, statistic],
                   complete[1, "date"], complete[1, statistic],
                   col = "dodgerblue")
          segments(wk1.partial$date, wk1.partial[, statistic],
                   complete[1, "date"], complete[1, statistic],
                   lty = "dotted")
        }

        if (weekdays(last.obs.date) == "Saturday") {
          points(current.wk[, vars], pch = 16)
          segments(complete[nrow(complete), "date"],
                   complete[nrow(complete), statistic],
                   current.wk$date,
                   current.wk[, statistic])
        } else {
          points(current.wk.est[, vars], col = "red")
          points(current.wk[, vars], pch = 0, col = "gray")
          segments(complete[nrow(complete), "date"],
                   complete[nrow(complete), statistic],
                   current.wk.est$date,
                   current.wk.est[, statistic],
                   col = "red")
          segments(complete[nrow(complete), "date"],
                   complete[nrow(complete), statistic],
                   current.wk$date,
                   current.wk[, statistic],
                   lty = "dotted")
          axis(4, at = dat[nrow(dat), statistic], labels = "obs")
          axis(4, at = current.wk.est[, statistic], labels = "est",
            col.axis = "red", col.ticks = "red")
        }
      } else {
        if (log.y) {
          plot(dat[, vars], type = type, xlab = "Date", ylab = y.nm.case,
            log = "y")
        } else {
          plot(dat[, vars], type = type, xlab = "Date", ylab = y.nm.case)
        }
      }

      if (smooth) {
        if (any(dat$in.progress)) {
          smooth.data <- complete
          lines(stats::lowess(smooth.data$date, smooth.data[, statistic],
            f = f), col = "blue", lwd = 1.25)
        } else if (any(dat$partial)) {
          smooth.data <- rbind(wk1.backdate, complete)
          lines(stats::lowess(smooth.data$date, smooth.data[, statistic],
            f = f), col = "blue", lwd = 1.25)
        } else {
           lines(stats::lowess(dat$date, dat[, statistic], f), col = "blue",
            lwd = 1.25)
        }
      }

      if (r.version) {
        r_v <- rversions::r_versions()
        axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
          cex.axis = 2/3, padj = 0.9)
      }

      title(main = "Total R Downloads")

    } else if (graphics == "ggplot2") {
      if (statistic == "count") {
        p <- ggplot(data = dat, aes_string("date", "count"))
      } else if (statistic == "cumulative") {
        p <- ggplot(data = dat, aes_string("date", "cumulative"))
      }

      if (any(dat$in.progress)) {
        ip.sel <- dat$in.progress == TRUE
        ip.data <- dat[ip.sel, ]
        complete <- dat[!ip.sel, ]
        last.obs <- nrow(complete)

        obs.days <- as.numeric(format(last.obs.date , "%d"))
        exp.days <- as.numeric(format(lastDayMonth(ip.data$date)$date, "%d"))
        est.ct <- round(ip.data$count * exp.days / obs.days)

        est.data <- ip.data
        est.data$count <- est.ct
        last.cumulative <- complete[nrow(complete), "cumulative"]
        est.data$cumulative <- last.cumulative + est.ct

        est.seg <- rbind(complete[last.obs, ], est.data)
        obs.seg <- rbind(complete[last.obs, ], ip.data)

        p <- p + geom_line(data = complete, size = 1/3) +
          scale_color_manual(name = "In-progress",
                             breaks = c("Observed", "Estimate"),
                             values = c("Observed" = "black",
                                        "Estimate" = "red")) +
          scale_shape_manual(name = "In-progress",
                             breaks = c("Observed", "Estimate"),
                             values = c("Observed" = 0, "Estimate" = 1)) +
          scale_linetype_manual(name = "In-progress",
                                breaks = c("Observed", "Estimate"),
                                values = c("Observed" = "dotted",
                                           "Estimate" = "solid")) +
          geom_line(data = est.seg, size = 1/3,
            aes(colour = "Estimate", linetype = "Estimate")) +
          geom_line(data = obs.seg,  size = 1/3,
            aes(col = "Observed", linetype = "Observed")) +
          geom_point(data = est.data,
            aes(colour = "Estimate", shape = "Estimate")) +
          geom_point(data = ip.data,
            aes(colour = "Observed", shape = "Observed"))

        if (points) p <- p + geom_point(data = complete)

      } else if (any(dat$partial)) {
        unit.date <- dat$date

        wk1.start <- dat$date[1]
        wk1.end <- dat$date[2] - 1
        wk1 <- cranDownloads(x$packages, from = wk1.start, to = wk1.end)

        if (weekdays(x$from) == "Sunday") {
          wk1.partial <- dat[dat$date == wk1.start, ]
          wk1.backdate <- wk1.partial
        } else {
          sel <- dat$partial & dat$date == wk1.start
          wk1.partial <- dat[sel, ]
          wk1.backdate <- wk1.partial

          wk1.backdate$count <- sum(wk1$cranlogs.data$count)
          wk1.backdate$cumulative <- wk1.backdate$count
          cumulative.recompute <- cumsum(c(wk1.backdate$cumulative,
            dat$count[-1]))
        }

        current.wk <- dat[nrow(dat), ]
        current.wk.est <- current.wk

        weekdays.elapsed <- as.integer(x$last.obs.date -
          unit.date[length(unit.date)] + 1)

        if (weekdays.elapsed != 0) { # monday exception
          current.wk.est$count <- 7L / weekdays.elapsed * current.wk$count
        } else {
          current.wk.est$count <- 7L * current.wk$count
        }

        if (weekdays(x$from) != "Sunday") {
          current.wk.est$cumulative <-
            cumulative.recompute[(nrow(dat) - 1)] + current.wk.est$count
          first.last <- c(1, nrow(dat))
          dat.recompute <- rbind(wk1.backdate, dat[-first.last, ],
            current.wk.est)
          dat.recompute$cumulative[-first.last] <-
            cumulative.recompute[-first.last]
          current.wk$cumulative <- current.wk$count +
            rev(cumulative.recompute[-first.last])[1]
        } else {
          dat.recompute <- dat
        }

        complete <- dat.recompute[-c(1, nrow(dat.recompute)), ]
        wk1.partial$date <- max(min(wk1$cranlogs.data$date), x$from)

        wk1.backdate.seg <- rbind(complete[1, ], wk1.backdate)
        wk1.partial.seg <- rbind(complete[1, ], wk1.partial)
        current.wk.seg <- rbind(complete[nrow(complete), ], current.wk)
        current.wk.est.seg <- rbind(complete[nrow(complete), ], current.wk.est)

        p <- p + geom_line(data = complete, size = 1/3) +
          scale_color_manual(name = "",
                             breaks = c("Backdate",
                                        "Partial/In-Progress",
                                        "Estimate"),
                             values = c("Backdate" = "dodgerblue",
                                        "Partial/In-Progress" = "black",
                                        "Estimate" = "red")) +
          scale_linetype_manual(name = "",
                                breaks = c("Backdate",
                                           "Partial/In-Progress",
                                           "Estimate"),
                                values = c("Backdate" = "solid",
                                           "Partial/In-Progress" = "dotted",
                                           "Estimate" = "solid")) +
          scale_shape_manual(name = "",
                             breaks = c("Backdate",
                                        "Partial/In-Progress",
                                        "Estimate"),
                             values = c("Backdate" = 8,
                                        "Partial/In-Progress" = 0,
                                        "Estimate" = 1))

        if (weekdays(last.obs.date) == "Saturday") {
          p <- p +
            geom_line(data = current.wk.seg, size = 1/3) +
            geom_point(data = current.wk)
        } else {
          p <- p +
            geom_line(data = current.wk.est.seg, size = 1/3,
              aes(colour = "Estimate", linetype = "Estimate")) +
            geom_point(data = current.wk.est, size = 1.5,
              aes(colour = "Estimate", shape = "Estimate")) +
            geom_line(data = current.wk.seg, size = 1/3,
              aes(colour = "Partial/In-Progress",
                  linetype = "Partial/In-Progress")) +
            geom_point(data = current.wk,
              aes(colour = "Partial/In-Progress",
                  shape = "Partial/In-Progress"))
        }

        if (weekdays(x$from) == "Sunday") {
          p <- p +
            geom_line(data = wk1.partial.seg, size = 1/3) +
            geom_point(data = wk1.partial)
        } else {
          p <- p +
            geom_line(data = wk1.backdate.seg, size = 1/3,
              aes(colour = "Backdate", linetype = "Backdate")) +
            geom_point(data = wk1.backdate,
              aes(colour = "Backdate", shape = "Backdate")) +
            geom_line(data = wk1.partial.seg, size = 1/3,
              aes(colour = "Partial/In-Progress",
                  linetype = "Partial/In-Progress")) +
            geom_point(data = wk1.partial,
              aes(colour = "Partial/In-Progress",
                  shape = "Partial/In-Progress"))
        }

        if (points) p <- p + geom_point(data = complete)

      } else {
        p <- p + geom_line(size = 0.5) +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5)) +
          ggtitle("Total R Downloads")

        if (points) p <- p + geom_point()
      }

      if (log.y) p <- p + scale_y_log10() + ylab(paste("log10", y.nm.case))
      if (smooth) {
        if (any(dat$in.progress)) {
          smooth.data <- complete
          p <- p + geom_smooth(data = smooth.data, method = "loess",
            formula = "y ~ x", se = se, span = span)
        } else if (any(dat$partial)) {
          smooth.data <- rbind(wk1.backdate, complete)
          p <- p + geom_smooth(data = smooth.data, method = "loess",
            formula = "y ~ x", se = se, span = span)
        } else {
          p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se,
            span = span)
        }
      }

      p <- p + theme_bw() +
        ggtitle("Total R Downloads") +
        theme(legend.position = "bottom",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5))

      suppressWarnings(print(p))
    }
  }
}
