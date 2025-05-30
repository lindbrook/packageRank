# Auxiliary functions for plot.cranDownloads() #

aggregateData <- function(x, unit.observation, cores) {
  dat <- x$cranlogs.data
  if ("package" %in% names(dat) | "platform" %in% names(dat)) {
    if ("package" %in% names(dat)) {
      grp <- x$packages
    } else if ("platform" %in% names(dat)) {
      grp <- unique(dat$platform)
    }
    if (unit.observation == "year") {
      out <- parallel::mclapply(grp, function(g) {
        if ("package" %in% names(dat)) {
          tmp <- dat[dat$package == g, ]
        } else if ("platform" %in% names(dat)) {
          tmp <- dat[dat$platform == g, ]
        }
        unit <- as.numeric(format(tmp$date, "%Y"))
        unit.ct <- tapply(tmp$count, unit, sum)
        max.obs <- max(tmp$date)
        max.exp <- as.Date(paste0(max(as.numeric(names(unit.ct))), "-12-31"))
        if (max.obs < max.exp) ip <- c(rep(FALSE, length(unit.ct) - 1), TRUE)
        else ip <- rep(FALSE, length(unit.ct))
        grp.data <- data.frame(unit.obs = as.numeric(names(unit.ct)),
          count = unname(unit.ct), cumulative = cumsum(unname(unit.ct)),
          date = as.Date(paste0(names(unit.ct), "-12-31")), in.progress = ip,
          group = g)
        if ("package" %in% names(dat)) {
          names(grp.data)[names(grp.data) == "group"] <- "package"
        } else if ("platform" %in% names(dat)) {
          names(grp.data)[names(grp.data) == "group"] <- "platform"
        }
        grp.data
      }, mc.cores = cores)
    } else if (unit.observation == "month") {
      out <- parallel::mclapply(grp, function(g) {
        if ("package" %in% names(dat)) {
          tmp <- dat[dat$package == g, ]
        } else if ("platform" %in% names(dat)) {
          tmp <- dat[dat$platform == g, ]
        }
        unit <- format(tmp$date, "%Y-%m")
        unit.ct <- tapply(tmp$count, unit, sum)
        last.day.mo <- lastDayMonth(tmp$date)
        grp.data <- data.frame(unit.obs = names(unit.ct),
                               count = unname(unit.ct),
                               cumulative = cumsum(unname(unit.ct)),
                               date = as.Date(paste0(names(unit.ct), "-01")),
                               in.progress = last.day.mo$in.progress,
                               end.date = last.day.mo$date,
                               group = g)
        if ("package" %in% names(dat)) {
          names(grp.data)[names(grp.data) == "group"] <- "package"
        } else if ("platform" %in% names(dat)) {
          names(grp.data)[names(grp.data) == "group"] <- "platform"
        }
        grp.data
      }, mc.cores = cores)
    } else if (unit.observation == "week") {
      out <- parallel::mclapply(grp, function(g) {
        if ("package" %in% names(dat)) {
          tmp <- dat[dat$package == g, ]
        } else if ("platform" %in% names(dat)) {
          tmp <- dat[dat$platform == g, ]
        }
        unit <- as.Date(cut(tmp$date, breaks = "week", start.on.monday = FALSE))
        unit.ct <- tapply(tmp$count, unit, sum)
        unit.date <- as.Date(names(unit.ct))
        partial <- rep(FALSE, length(unit.ct))
        if (x$from != unit.date[1]) partial[1] <- TRUE
        if (x$to == unit.date[length(unit.date)] + 7 - 1) {
          partial[length(partial)] <- FALSE
        } else {
          partial[length(partial)] <- TRUE
        }
        grp.data <- data.frame(unit.obs = names(unit.ct),
          count = unname(unit.ct), cumulative = cumsum(unname(unit.ct)),
          date = unit.date, partial = partial, group = g)
        if ("package" %in% names(dat)) {
          names(grp.data)[names(grp.data) == "group"] <- "package"
        } else if ("platform" %in% names(dat)) {
          names(grp.data)[names(grp.data) == "group"] <- "platform"
        }
        grp.data
      }, mc.cores = cores)
    }
    out <- do.call(rbind, out)
  } else {
    if (unit.observation == "year") {
      unit <- format(dat$date, "%Y")
      unit.ct <- tapply(dat$count, unit, sum)
      max.obs <- max(dat$date)
      max.exp <- as.Date(paste0(max(as.numeric(names(unit.ct))), "-12-31"))
      if (max.obs < max.exp) ip <- c(rep(FALSE, length(unit.ct) - 1), TRUE)
      else ip <- rep(FALSE, length(unit.ct))
      out <- data.frame(unit.obs = names(unit.ct), count = unname(unit.ct),
        cumulative = cumsum(unname(unit.ct)),
        date = as.Date(paste0(names(unit.ct), "-12-31")), in.progress = ip)
    } else if (unit.observation == "month") {
      unit <- format(dat$date, "%Y-%m")
      unit.ct <- tapply(dat$count, unit, sum)
      last.day.mo <- lastDayMonth(dat$date)
      out <- data.frame(unit.obs = names(unit.ct),
                        count = unname(unit.ct),
                        date = as.Date(paste0(names(unit.ct), "-01")),
                        cumulative = cumsum(unname(unit.ct)),
                        in.progress = last.day.mo$in.progress,
                        end.date = last.day.mo$date)
    } else if (unit.observation == "week") {
      unit <- as.Date(cut(dat$date, breaks = "week", start.on.monday = FALSE))
      unit.ct <- tapply(dat$count, unit, sum)
      unit.date <- as.Date(names(unit.ct))
      partial <- rep(FALSE, length(unit.ct))
      if (x$from != unit.date[1]) partial[1] <- TRUE
      if (x$to == unit.date[length(unit.date)] + 7 - 1) {
        partial[length(partial)] <- FALSE
      } else {
        partial[length(partial)] <- TRUE
      }
      out <- data.frame(unit.obs = names(unit.ct),count = unname(unit.ct),
        cumulative = cumsum(unname(unit.ct)), date = unique(unit),
        partial = partial)
    }
  }
  out
}

lastDayMonth <- function(dates) {
  max.obs.date <- max(dates)
  max.mo <- as.numeric(format(max.obs.date, "%m"))
  max.yr <- as.numeric(format(max.obs.date, "%Y"))
  max.yr.mo <- format(max.obs.date, "%Y-%m")
  if (max.mo < 12) {
    max.date <- as.Date(paste0(max.yr, "-", max.mo + 1, "-", 1)) - 1
  } else {
    max.date <- as.Date(paste0(max.yr + 1, "-01-01")) - 1
  }
  obs.yr.mo <- unique(format(dates, "%Y-%m"))

  if (length(obs.yr.mo) == 1) {
    data.frame(date = max.date, in.progress = TRUE)
  } else {
    obs.yr.mo <- obs.yr.mo[obs.yr.mo != max.yr.mo]
    ldm <- lapply(obs.yr.mo, function(dt) {
      parts <- as.numeric(unlist(strsplit(dt, "-")))
      if (parts[2] < 12) {
        next.mo <- parts[2] + 1
        as.Date(paste0(parts[1], "-", next.mo, "-01")) - 1
      } else {
        as.Date(paste0(parts[1] + 1, "-", "01-01")) - 1
      }
    })
    ldm <- do.call(c, ldm)
    ip <- c(rep(FALSE, length(ldm)),
            ifelse(max.obs.date != max.date, TRUE, FALSE))
    data.frame(date = c(ldm, max.date), in.progress = ip)
  }
}

packageLifeFilter <- function(out, packages, first.published) {
  dat <- out$cranlogs.data
  birth.data <- lapply(seq_along(packages), function(i) {
    tmp <- dat[dat$package == packages[i], ]
    if (any(tmp$date < first.published[i])) {
      tmp <- tmp[tmp$date >= first.published[i], ]
    }
    tmp
  })
  do.call(rbind, birth.data)
}

inProgressEstimate <- function(x, unit.observation) {
  dat <- x$cranlogs.data
  ip.data <- dat[dat$in.progress == TRUE, ]
  
  if (unit.observation == "year") {
    yr <- format(x$last.obs.date, "%Y")
    from <-  as.Date(paste0(yr, "-01-01"))
    to <-  as.Date(paste0(yr, "-12-31"))
    elapsed <- length(seq.Date(from = from, to = x$last.obs.date, by = "day"))
    total <- length(seq.Date(from = from, to = to, by = "day"))
  } else if (unit.observation == "month") {
    yr <- format(x$last.obs.date, "%Y")
    mo <- format(x$last.obs.date, "%m")
    from <- as.Date(paste0(yr, "-", mo, "-01"))
    end.of.month <- as.Date(paste0(yr, "-", as.numeric(mo) + 1, "-01")) - 1
    elapsed <- length(seq.Date(from = from, to = x$last.obs.date, by = "day"))
    total <- length(seq.Date(from = from, to = end.of.month, by = "day"))
  } else if (unit.observation == "week") {
    wk <- levels(cut(x$last.obs.date, breaks = "week", start.on.monday = FALSE))
    sunday <- as.Date(wk)
    elapsed <- length(seq.Date(from = sunday, to = x$last.obs.date, by = "day"))
    total <- 7L
  }

  ip.data$count * total / elapsed
}
