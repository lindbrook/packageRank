# Auxiliary functions for plot.cranDownloads() #

aggregateData <- function(unit.observation, dat, cores) {
  if ("package" %in% names(dat) | "platform" %in% names(dat)) {
    if ("package" %in% names(dat)) {
      grp <- unique(dat$package)
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
      do.call(rbind, out)
    } else if (unit.observation == "month") {
      out <- parallel::mclapply(grp, function(g) {
        if ("package" %in% names(dat)) {
          tmp <- dat[dat$package == g, ]
        } else if ("platform" %in% names(dat)) {
          tmp <- dat[dat$platform == g, ]
        }
        unit <- format(tmp$date, "%Y-%m")
        unit.ct <- tapply(tmp$count, unit, sum)
        grp.data <- data.frame(unit.obs = names(unit.ct),
          count = unname(unit.ct), cumulative = cumsum(unname(unit.ct)),
          lastDayMonth(tmp$date), group = g)
        if ("package" %in% names(dat)) {
          names(grp.data)[names(grp.data) == "group"] <- "package"
        } else if ("platform" %in% names(dat)) {
          names(grp.data)[names(grp.data) == "group"] <- "platform"
        }
        grp.data
      }, mc.cores = cores)
    }
    do.call(rbind, out)
  } else {
    if (unit.observation == "year") {
      unit <- format(dat$date, "%Y")
      unit.ct <- tapply(dat$count, unit, sum)
      max.obs <- max(dat$date)
      max.exp <- as.Date(paste0(max(as.numeric(names(unit.ct))), "-12-31"))
      if (max.obs < max.exp) ip <- c(rep(FALSE, length(unit.ct) - 1), TRUE)
      else ip <- rep(FALSE, length(unit.ct))
      data.frame(unit.obs = names(unit.ct), count = unname(unit.ct),
        cumulative = cumsum(unname(unit.ct)),
        date = as.Date(paste0(names(unit.ct), "-12-31")), in.progress = ip)
    } else if (unit.observation == "month") {
      unit <- format(dat$date, "%Y-%m")
      unit.ct <- tapply(dat$count, unit, sum)
      data.frame(unit.obs = names(unit.ct), count = unname(unit.ct),
        cumulative = cumsum(unname(unit.ct)), lastDayMonth(dat$date))
    }
  }
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
