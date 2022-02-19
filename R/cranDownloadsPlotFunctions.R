# Plot functions for plot.cranDownloads() #

cranPlot <- function(x, statistic, graphics, points, log.count, smooth, se, f,
  span, r.version) {

  dat <- x$cranlogs.data
  last.obs.date <- x$last.obs.date

  if (statistic == "count") {
    y.nm.case <- "Count"
    y.nm <- tolower(y.nm.case)
  } else if (statistic == "cumulative") {
    y.nm.case <- "Cumulative"
    y.nm <- tolower(y.nm.case)
  }

  type <- ifelse(points, "o", "l")

  if (graphics == "base") {
    if (any(dat$in.progress)) {
      ip.sel <- dat$in.progress == TRUE
      ip.data <- dat[ip.sel, ]
      complete.data <- dat[!ip.sel, ]
      last.obs <- nrow(complete.data)

      obs.days <- as.numeric(format(last.obs.date , "%d"))
      exp.days <- as.numeric(format(ip.data[, "date"], "%d"))
      est.ct <- round(ip.data$count * exp.days / obs.days)

      est.data <- ip.data
      est.data$count <- est.ct
      last.cumulative <- complete.data[nrow(complete.data), "cumulative"]
      est.data$cumulative <- last.cumulative + est.ct

      xlim <- range(dat$date)

      if (statistic == "count") {
        ylim <- range(c(dat[, y.nm], est.data$count))
      } else if (statistic == "cumulative") {
        ylim <- range(c(dat[, y.nm], est.data$cumulative))
      }

      if (log.count) {
        plot(complete.data$date, complete.data[, y.nm], type = type,
          xlab = "Date", ylab = paste0("log10 ", y.nm.case), xlim = xlim,
          ylim = ylim, log = "y", pch = 16)
      } else {
        plot(complete.data$date, complete.data[, y.nm], type = type,
          xlab = "Date", ylab = y.nm.case, xlim = xlim, ylim = ylim, pch = 16)
      }

      points(ip.data[, "date"], ip.data[, y.nm], col = "black", pch = 0)
      points(est.data[, "date"], est.data[, y.nm], col = "red", pch = 1)

      segments(complete.data[last.obs, "date"],
               complete.data[last.obs, y.nm],
               ip.data$date,
               ip.data[, y.nm],
               lty = "dotted")
      segments(complete.data[last.obs, "date"],
               complete.data[last.obs, y.nm],
               est.data$date,
               est.data[, y.nm],
               col = "red")

      axis(4, at = ip.data[, y.nm], labels = "obs")
      axis(4, at = est.data[, y.nm], labels = "est", col.axis = "red",
        col.ticks = "red")

    } else {
      if (log.count) {
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
        smooth.data <- complete.data
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
      complete.data <- dat[!ip.sel, ]
      last.obs <- nrow(complete.data)

      obs.days <- as.numeric(format(last.obs.date , "%d"))
      exp.days <- as.numeric(format(ip.data[, "date"], "%d"))
      est.ct <- round(ip.data$count * exp.days / obs.days)

      est.data <- ip.data
      est.data$count <- est.ct
      last.cumulative <- complete.data[nrow(complete.data), "cumulative"]
      est.data$cumulative <- last.cumulative + est.ct

      est.seg <- rbind(complete.data[last.obs, ], est.data)
      obs.seg <- rbind(complete.data[last.obs, ], ip.data)

      p <- p + geom_line(data = complete.data, size = 1/3) +
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

      if (points) p <- p + geom_point(data = complete.data)
      if (log.count) p <- p + scale_y_log10() + ylab("log10 count")
      if (smooth) {
        if (any(dat$in.progress)) {
          smooth.data <- complete.data
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
      if (log.count) p <- p + scale_y_log10() + ylab("log10 count")
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

singlePlot <- function(x, statistic, graphics, obs.ct, points, smooth,
  se, f, span, log.count, package.version, dev.mode, r.version, same.xy) {

  dat <- x$cranlogs.data
  last.obs.date <- x$last.obs.date
  type <- ifelse(points, "o", "l")

  if (statistic == "count") {
    y.var <- dat$count
    y.nm.case <- "Count"
    y.nm <- tolower(y.nm.case)
    ttl <- "Package Download Counts"
  } else if (statistic == "cumulative") {
    y.var <- dat$cumulative
    y.nm.case <- "Cumulative"
    y.nm <- tolower(y.nm.case)
    ttl <- "Cumulative Package Downloads"
  }

  if (graphics == "base") {
    if (obs.ct == 1) {
      if (log.count) {
        dotchart(log10(dat$count), labels = dat$package,
          xlab = "log10 Count", main = paste(ttl, unique(dat$date)))
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
          ip.sel <- pkg.dat$in.progress == TRUE
          ip.data <- pkg.dat[ip.sel, ]
          complete.data <- pkg.dat[!ip.sel, ]

          obs.days <- as.numeric(format(last.obs.date , "%d"))
          exp.days <- as.numeric(format(ip.data[, "date"], "%d"))
          est.ct <- round(ip.data$count * exp.days / obs.days)

          est.data <- ip.data
          est.data$count <- est.ct
          last.cumulative <- complete.data[nrow(complete.data), "cumulative"]
          est.data$cumulative <- last.cumulative + est.ct

          list(complete.data = complete.data, ip.data = ip.data,
            est.data = est.data)
         })

        tmp <- lapply(plot.data, function(x) do.call(rbind, x))
        tmp <- do.call(rbind, tmp)
        ylim <- range(tmp[, y.nm])

        invisible(lapply(seq_along(plot.data), function(i) {
          complete.data <- plot.data[[i]]$complete.data
          ip.data <- plot.data[[i]]$ip.data
          est.data <- plot.data[[i]]$est.data

          if (log.count) {
            plot(complete.data$date, complete.data[, y.nm], type = type,
              xlab = "Date", ylab = paste0("log10 ", y.nm.case), xlim = xlim,
              ylim = ylim, log = "y", pch = 16)
          } else {
            plot(complete.data$date, complete.data[, y.nm], type = type,
              xlab = "Date", ylab = y.nm.case, xlim = xlim, ylim = ylim,
              pch = 16)
          }

          points(ip.data[, "date"], ip.data[, y.nm], col = "black", pch = 0)
          points(est.data[, "date"], est.data[, y.nm], col = "red", pch = 1)

          last.obs <- nrow(complete.data)
          segments(complete.data[last.obs, "date"],
                   complete.data[last.obs, y.nm],
                   ip.data$date,
                   ip.data[, y.nm],
                   lty = "dotted")
          segments(complete.data[last.obs, "date"],
                   complete.data[last.obs, y.nm],
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
              smooth.data <- complete.data
              lines(stats::lowess(smooth.data$date, smooth.data[, y.nm],
                f = f), col = "blue")
            } else {
              lines(stats::lowess(dat$date, dat[, y.nm], f = f), col = "blue")
            }
          }
          title(main = est.data$package)
        }))

      } else {
        ylim <- range(dat[, y.nm])

        invisible(lapply(x$package, function(pkg) {
          pkg.dat <- dat[dat$package == pkg, ]
          type <- ifelse(points, "o", "l")

          if (log.count) {
            plot(pkg.dat$date, pkg.dat[, y.nm], type = type, xlab = "Date",
              ylab = paste0("log10 ", y.nm.case), log = "y", xlim = xlim,
              ylim = ylim)
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

      if (log.count) p <- p + scale_x_log10() + xlab("log10 Count")

    } else if (obs.ct > 1) {
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
          complete.data <- pkg.data[!ip.sel, ]
          last.obs <- nrow(complete.data)

          obs.days <- as.numeric(format(last.obs.date , "%d"))
          exp.days <- as.numeric(format(ip.data[, "date"], "%d"))
          est.ct <- round(ip.data$count * exp.days / obs.days)

          est.data <- ip.data
          est.data$count <- est.ct
          last.cumulative <- complete.data[nrow(complete.data), "cumulative"]
          est.data$cumulative <- last.cumulative + est.ct

          list(ip.data = ip.data,
               complete.data = complete.data,
               est.data = est.data,
               est.seg = rbind(complete.data[last.obs, ], est.data),
               obs.seg = rbind(complete.data[last.obs, ], ip.data))
        })

        ip.data <- do.call(rbind, lapply(g, function(x) x$ip.data))
        complete.data <- do.call(rbind, lapply(g, function(x) x$complete.data))
        est.data <- do.call(rbind, lapply(g, function(x) x$est.data))
        est.seg <- do.call(rbind, lapply(g, function(x) x$est.seg))
        obs.seg <- do.call(rbind, lapply(g, function(x) x$obs.seg))

        p <- p + geom_line(data = complete.data, size = 1/3) +
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

        if (points) p <- p + geom_point(data = complete.data)

      } else {
        p <- p + geom_line(size = 1/3)
        if (points) p <- p + geom_point()
      }

      if (log.count) p <- p + scale_y_log10() + ylab("log10 count")

      if (smooth) {
        if (any(dat$in.progress)) {
          smooth.data <- complete.data
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

multiPlot <- function(x, statistic, graphics, obs.ct, log.count,
  legend.location, ip.legend.location, points, smooth, se, f, span) {

  dat <- x$cranlogs.data
  last.obs.date <- x$last.obs.date

  if (statistic == "count") {
    ttl <- "Package Download Counts"
  } else if (statistic == "cumulative") {
    ttl <- "Cumulative Package Downloads"
  }

  if (graphics == "base") {
    if (obs.ct == 1) {
      if (log.count) {
        dotchart(log10(dat$count), labels = dat$package, xlab = "log10 Count",
          main = paste(ttl, unique(dat$date)))
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
            ip.data <- tmp[tmp$in.progress == TRUE, ]
            complete.data <- tmp[tmp$in.progress == FALSE, ]
            last.obs <- nrow(complete.data)

            obs.days <- as.numeric(format(last.obs.date , "%d"))
            exp.days <- as.numeric(format(ip.data[, "date"], "%d"))
            est.ct <- round(ip.data$count * exp.days / obs.days)

            est.data <- ip.data
            est.data$count <- est.ct
            last.cumulative <- complete.data[nrow(complete.data), "cumulative"]
            est.data$cumulative <- last.cumulative + est.ct

            list(complete.data = complete.data, est.data = est.data,
              ip.data = ip.data)
          })

          est.stat <- lapply(pkg.data, function(x) x$est.data)
          est.stat <- do.call(rbind, est.stat)[, statistic]
          ylim <- range(c(ylim, est.stat))

          if (log.count) {
            plot(dat[, vars], pch = NA, log = "y", xlim = xlim, ylim = ylim,
              main = ttl)
          } else {
            plot(dat[, vars], pch = NA, xlim = xlim, ylim = ylim, main = ttl)
          }

          invisible(lapply(seq_along(pkg.data), function(i) {
            complete.data <- pkg.data[[i]]$complete.data
            est.data <- pkg.data[[i]]$est.data
            ip.data <- pkg.data[[i]]$ip.data
            last.obs <- nrow(complete.data)

            lines(complete.data$date, complete.data[, statistic],
              col = cbPalette[i])

            segments(complete.data[last.obs, "date"],
                     complete.data[last.obs, statistic],
                     ip.data$date,
                     ip.data[, statistic],
                     col = "black",
                     lty = "dotted")
            segments(complete.data[last.obs, "date"],
                     complete.data[last.obs, statistic],
                     est.data$date,
                     est.data[, statistic],
                     col = "black",
                     lty = "longdash")

             points(est.data[, "date"], est.data[, statistic],
               col = cbPalette[i])
             points(ip.data[, "date"], ip.data[, statistic], pch = 0,
               col = cbPalette[i])

            if (points) {
              points(complete.data[, "date"], complete.data[, statistic],
                col = cbPalette[i], pch = 16)
            }

            if (smooth) {
              smooth.data <- complete.data
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

        } else {
          if (log.count) {
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
                 pch = 16,
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
      if (log.count) {
        # p + scale_x_log10() + xlab("log10(count)") doesn't work!
        dat2 <- dat
        dat2$count <- log10(dat2$count)
        p <- ggplot(data = dat2, aes_string(x = "count", y = "package")) +
             xlab("log10 Count")
      }

      p <- p + geom_hline(yintercept = c(1, 2), linetype = "dotted") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

    } else if (obs.ct > 1) {
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
          complete.data <- pkg.data[!ip.sel, ]
          last.obs <- nrow(complete.data)

          obs.days <- as.numeric(format(last.obs.date , "%d"))
          exp.days <- as.numeric(format(ip.data[, "date"], "%d"))
          est.ct <- round(ip.data$count * exp.days / obs.days)

          est.data <- ip.data
          est.data$count <- est.ct
          last.cumulative <- complete.data[nrow(complete.data), "cumulative"]
          est.data$cumulative <- last.cumulative + est.ct

          list(ip.data = ip.data,
               complete.data = complete.data,
               est.data = est.data,
               est.seg = rbind(complete.data[last.obs, ], est.data),
               obs.seg = rbind(complete.data[last.obs, ], ip.data))
        })

        ip.data <- do.call(rbind, lapply(g, function(x) x$ip.data))
        complete.data <- do.call(rbind, lapply(g, function(x) x$complete.data))
        est.data <- do.call(rbind, lapply(g, function(x) x$est.data))
        est.seg <- do.call(rbind, lapply(g, function(x) x$est.seg))
        obs.seg <- do.call(rbind, lapply(g, function(x) x$obs.seg))

        p <- p + geom_line(data = complete.data, size = 1/3) +
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

        if (points) {
          p <- p + geom_point(data = complete.data)
        }

      } else {
        p <- p + geom_line(size = 1/3) +
          theme(legend.position = "bottom",
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))

        if (points) p <- p + geom_point()
      }

      if (log.count) p <- p + scale_y_log10() + ylab("log10 count")

      if (smooth) {
        if (any(dat$in.progress)) {
          smooth.data <- complete.data
          p <- p + geom_smooth(data = smooth.data, method = "loess",
            formula = "y ~ x", se = se, span = span)
        } else {
          p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se,
            span = span)
        }
      }
    }
    suppressWarnings(print(p))
  }
}

rPlot <- function(x, statistic, graphics, obs.ct, legend.location,
  ip.legend.location, points, log.count, smooth, se, r.version, f, span,
  multi.plot) {

  dat <- x$cranlogs.data
  ylab <- tools::toTitleCase(statistic)
  last.obs.date <- x$last.obs.date
  type <- ifelse(points, "o", "l")

  if (obs.ct == 1) {
    if (graphics == "base") {
      if (log.count) {
        dotchart(log10(dat$count), labels = dat$platform, xlab = "log10 Count",
          main = paste("R Downloads:", unique(dat$date)))
      } else {
        dotchart(dat$count, labels = dat$platform, xlab = "Count",
          main = paste("R Downloads:", unique(dat$date)))
      }
    } else if (graphics == "ggplot2") {
      if (log.count) {
        dat2 <- dat
        dat2$count <- log10(dat2$count)
        p <- ggplot(data = dat2, aes_string(x = "count", y = "platform")) +
          geom_point(size = 2) +
          xlab("log10 Count")
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
      if (any(dat$in.progress)) {
        pltfrm <- unique(dat$platform)
        pltfrm.col <- c("red", "dodgerblue", "black")

        p.data <- lapply(seq_along(pltfrm), function(i) {
          pkg.dat <- dat[dat$platform == pltfrm[i], ]
          ip.sel <- pkg.dat$in.progress == TRUE
          ip.data <- pkg.dat[ip.sel, ]
          complete.data <- pkg.dat[!ip.sel, ]
          obs.days <- as.numeric(format(last.obs.date , "%d"))
          exp.days <- as.numeric(format(ip.data[, "date"], "%d"))
          est.ct <- round(ip.data$count * exp.days / obs.days)
          est.data <- ip.data
          est.data$count <- est.ct
          last.cumulative <- complete.data[nrow(complete.data), "cumulative"]
          est.data$cumulative <- last.cumulative + est.ct
          list(ip.data = ip.data, complete.data = complete.data,
            est.data = est.data)
        })

        est.stat <- vapply(p.data, function(x) {
          x$est.data[, statistic]
        }, numeric(1L))

        complete.data <- lapply(p.data, function(x) x$complete.data)
        est.data <- lapply(p.data, function(x) x$est.data)
        ip.data <- lapply(p.data, function(x) x$ip.data)

        last.obs <- unique(vapply(complete.data, nrow, integer(1L)))
        ylim <- range(c(dat[, statistic], est.stat))

        if (log.count) {
          plot(dat$date, dat[, statistic], pch = NA, xlab = "Date",
            ylab = paste("log10", ylab), ylim = ylim, log = "y")
        } else {
          plot(dat$date, dat[, statistic], pch = NA, xlab = "Date", ylab = ylab,
            ylim = ylim)
        }

        if (points) {
          invisible(lapply(seq_along(complete.data), function(i) {
            tmp <- complete.data[[i]]
            points(tmp[, "date"], tmp[, statistic], col = pltfrm.col[i],
              pch = 16)
          }))
        }

        invisible(lapply(seq_along(complete.data), function(i) {
          tmp <- complete.data[[i]]
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

        invisible(lapply(seq_along(complete.data), function(i) {
          tmpA <- complete.data[[i]]
          tmpB <- ip.data[[i]]
          segments(tmpA[last.obs, "date"], tmpA[last.obs, statistic],
            tmpB$date, tmpB[, statistic], lty = "dotted")
        }))

        invisible(lapply(seq_along(complete.data), function(i) {
          tmpA <- complete.data[[i]]
          tmpB <- est.data[[i]]
          segments(tmpA[last.obs, "date"], tmpA[last.obs, statistic], tmpB$date,
            tmpB[, statistic], lty = "longdash", col = pltfrm.col[i])
        }))

        if (smooth) {
          invisible(lapply(seq_along(complete.data), function(i) {
            smooth.data <- complete.data[[i]]
            lines(stats::lowess(smooth.data$date, smooth.data[, statistic],
              f = f), col = pltfrm.col[i], lty = "solid", lwd = 1.5)
          }))
        }

        legend(x = legend.location,
               legend = c("win", "mac", "src"),
               col = c("black", "red", "dodgerblue"),
               pch = rep(16, 3),
               bg = "white",
               cex = 2/3,
               title = NULL,
               lwd = 1,
               bty = "n")

         legend(x = ip.legend.location,
               legend = c("Obs", "Est"),
               pch = 0:1,
               bg = "white",
               cex = 2/3,
               title = NULL,
               lty = c("dotted", "solid"),
               bty = "n")

        if (r.version) {
          r_v <- rversions::r_versions()
          axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
            cex.axis = 2/3, padj = 0.9)
        }

        title(main = "R Downloads")

      } else {
        if (log.count) {
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

        pltfrm <- unique(dat$platform)
        pltfrm.col <- c("red", "dodgerblue", "black")

        invisible(lapply(seq_along(pltfrm), function(i) {
          lines(dat[dat$platform == pltfrm[i], "date"],
                dat[dat$platform == pltfrm[i], statistic],
                type = type, pch = 0, col = pltfrm.col[i])
        }))

        legend(x = legend.location,
               legend = c("win", "mac", "src"),
               col = c("black", "red", "dodgerblue"),
               pch = c(1, 0, 2),
               bg = "white",
               cex = 2/3,
               title = "Platform",
               lwd = 1)

        if (smooth) {
          invisible(lapply(seq_along(pltfrm), function(i) {
            sm.data <- stats::lowess(dat[dat$platform == pltfrm[i], "date"],
              dat[dat$platform == pltfrm[i], statistic], f = f)
            lines(sm.data, lty = "solid", lwd = 1.5, col = pltfrm.col[i])
          }))
        }

        if (r.version) {
          r_v <- rversions::r_versions()
          axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
            cex.axis = 2/3, padj = 0.9)
        }
        title(main = "R Downloads")
      }

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
        pltfrm <- unique(dat$platform)
        pltfrm.col <- c("red", "blue", "black")

        p.data <- lapply(seq_along(pltfrm), function(i) {
          pkg.dat <- dat[dat$platform == pltfrm[i], ]
          ip.sel <- pkg.dat$in.progress == TRUE
          ip.data <- pkg.dat[ip.sel, ]
          complete.data <- pkg.dat[!ip.sel, ]
          last.obs <- nrow(complete.data)
          obs.days <- as.numeric(format(last.obs.date , "%d"))
          exp.days <- as.numeric(format(ip.data[, "date"], "%d"))
          est.ct <- round(ip.data$count * exp.days / obs.days)
          est.data <- ip.data
          est.data$count <- est.ct
          last.cumulative <- complete.data[nrow(complete.data), "cumulative"]
          est.data$cumulative <- last.cumulative + est.ct

          list(ip.data = ip.data,
               complete.data = complete.data,
               est.data = est.data,
               est.seg = rbind(complete.data[last.obs, ], est.data),
               obs.seg = rbind(complete.data[last.obs, ], ip.data))
        })

        est.stat <- vapply(p.data, function(x) {
          x$est.data[, statistic]
        }, numeric(1L))

        ylim <- range(c(dat[, statistic], est.stat))

        complete.data <- lapply(p.data, function(x) x$complete.data)
        est.data <- lapply(p.data, function(x) x$est.data)
        obs.data <- lapply(p.data, function(x) x$ip.data)

        complete.data <- do.call(rbind, complete.data)
        est.data <- do.call(rbind, est.data)
        obs.data <- do.call(rbind, obs.data)

        est.seg <- do.call(rbind, lapply(p.data, function(x) x$est.seg))
        obs.seg <- do.call(rbind, lapply(p.data, function(x) x$obs.seg))

        p <- p + geom_line(data = complete.data, size = 1/3)

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

        if (points) p <- p + geom_point(data = complete.data)
        if (log.count) p <- p + scale_y_log10() + ylab("log10 Count")

        if (smooth) {
          if (any(dat$in.progress)) {
            smooth.data <- complete.data
            p <- p + geom_smooth(data = smooth.data, method = "loess",
              formula = "y ~ x", se = se, span = span)
          } else {
            p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se,
              span = span)
          }
        }

        p <- p + theme_bw() +
          ggtitle("R Downloads") +
          theme(legend.position = "bottom",
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))

      } else {
        p <- p + geom_line(size = 0.5) +
          ggtitle("R Downloads") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5))

        if (points) p <- p + geom_point()
        if (log.count) p <- p + scale_y_log10() + ylab("log10 Count")
        if (!multi.plot) p <- p + facet_wrap(~ platform, nrow = 2)
        if (smooth) {
          p <- p + geom_smooth(method = "loess", formula = "y ~ x", se = se,
            span = span)
        }
      }

      suppressWarnings(print(p))
    }
  }
}

rTotPlot <- function(x, statistic, graphics, legend.location, points,
  log.count, smooth, se, r.version, f, span) {

  dat <- x$cranlogs.data
  last.obs.date <- x$last.obs.date
  ct <-  tapply(dat$count, dat$date, sum)

  if (any(dat$in.progress)) {
    dat <- data.frame(date = unique(dat$date),
                      count = ct,
                      cumulative = cumsum(ct),
                      in.progress = dat[dat$platform == "win", "in.progress"],
                      row.names = NULL)
  } else {
    dat <- data.frame(date = unique(dat$date),
                      count = ct,
                      cumulative = cumsum(ct),
                      row.names = NULL)
  }

  ylab <- tools::toTitleCase(statistic)

  if (graphics == "base") {
    type <- ifelse(points, "o", "l")

    if (any(dat$in.progress)) {
      ip.sel <- dat$in.progress == TRUE
      ip.data <- dat[ip.sel, ]
      complete.data <- dat[!ip.sel, ]
      last.obs <- nrow(complete.data)

      obs.days <- as.numeric(format(last.obs.date , "%d"))
      exp.days <- as.numeric(format(ip.data[, "date"], "%d"))
      est.ct <- round(ip.data$count * exp.days / obs.days)

      est.data <- ip.data
      est.data$count <- est.ct
      last.cumulative <- complete.data[nrow(complete.data), "cumulative"]
      est.data$cumulative <- last.cumulative + est.ct

      if (statistic == "count") {
        ylim <- range(c(dat[, statistic], est.data$count))
      } else if (statistic == "cumulative") {
        ylim <- range(c(dat[, statistic], est.data$cumulative))
      }

      xlim <- range(dat$date)

      if (log.count) {
        plot(complete.data$date, complete.data[, statistic], type = type,
          xlab = "Date", ylab = paste0("log10 ", ylab), xlim = xlim,
          ylim = ylim, log = "y", pch = 16)
      } else {
        plot(complete.data$date, complete.data[, statistic], type = type,
          xlab = "Date", ylab = ylab, xlim = xlim, ylim = ylim, pch = 16)
      }

      points(ip.data[, "date"], ip.data[, statistic], col = "black", pch = 0)
      points(est.data[, "date"], est.data[, statistic], col = "red", pch = 1)

      segments(complete.data[last.obs, "date"],
               complete.data[last.obs, statistic],
               ip.data$date,
               ip.data[, statistic],
               lty = "dotted")
      segments(complete.data[last.obs, "date"],
               complete.data[last.obs, statistic],
               est.data$date,
               est.data[, statistic],
               col = "red")

      axis(4, at = ip.data[, statistic], labels = "obs")
      axis(4, at = est.data[, statistic], labels = "est", col.axis = "red",
        col.ticks = "red")

      if (smooth) {
        smooth.data <- complete.data
        lines(stats::lowess(smooth.data$date, smooth.data[, statistic], f = f),
          col = "blue")
      }

      if (r.version) {
        r_v <- rversions::r_versions()
        axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
          cex.axis = 2/3, padj = 0.9)
      }

      title(main = "Total R Downloads")

    } else {
      if (log.count) {
        plot(dat$date, dat[, statistic], type = type, xlab = "Date",
          ylab = ylab, log = "y")
      } else {
        plot(dat$date, dat[, statistic], type = type, xlab = "Date",
          ylab = ylab)
      }

      if (smooth) {
        lines(stats::lowess(dat$date, dat[, statistic], f), col = "blue",
          lwd = 1.25)
      }

      if (r.version) {
        r_v <- rversions::r_versions()
        axis(3, at = as.Date(r_v$date), labels = paste("R", r_v$version),
          cex.axis = 2/3, padj = 0.9)
      }

      title(main = "Total R Downloads")
    }

  } else if (graphics == "ggplot2") {
    if (statistic == "count") {
      p <- ggplot(data = dat, aes_string("date", "count"))
    } else if (statistic == "cumulative") {
      p <- ggplot(data = dat, aes_string("date", "cumulative"))
    }

    if (any(dat$in.progress)) {
      ip.sel <- dat$in.progress == TRUE
      ip.data <- dat[ip.sel, ]
      complete.data <- dat[!ip.sel, ]
      last.obs <- nrow(complete.data)

      obs.days <- as.numeric(format(last.obs.date , "%d"))
      exp.days <- as.numeric(format(ip.data[, "date"], "%d"))
      est.ct <- round(ip.data$count * exp.days / obs.days)

      est.data <- ip.data
      est.data$count <- est.ct
      last.cumulative <- complete.data[nrow(complete.data), "cumulative"]
      est.data$cumulative <- last.cumulative + est.ct

      est.seg <- rbind(complete.data[last.obs, ], est.data)
      obs.seg <- rbind(complete.data[last.obs, ], ip.data)

      p <- p + geom_line(data = complete.data, size = 1/3) +
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

      if (points) p <- p + geom_point(data = complete.data)

    } else {
      p <- p + geom_line(size = 0.5) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        ggtitle("Total R Downloads")

      if (points) p <- p + geom_point()
    }

    if (log.count) p <- p + scale_y_log10() + ylab("log10 Count")
    if (smooth) {
      if (any(dat$in.progress)) {
        smooth.data <- complete.data
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
