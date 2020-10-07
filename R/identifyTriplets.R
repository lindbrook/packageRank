#' Extract triplets from logs (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. CRAN log data frame
#' @param output Character. "list" or "dataframe".
#' @param time.window Numeric. Seconds.
#' @param time.sort Logical. Sort output by time.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

identifyTriplets <- function(dat, output = "dataframe", time.window = 2,
  time.sort = TRUE, multi.core = TRUE) {

  if (all(dat$size > 1000L)) {
    out <- NULL
  } else {
    cores <- multiCore(multi.core)
    out <- parallel::mclapply(unique(dat$version), function(v) {
      v.data <- dat[dat$version == v, ]
      v.data$machine <- paste0(v.data$ip_id, "-", v.data$r_version, "-",
        v.data$r_arch, "-", v.data$r_os)
      v.data$id <- paste0(v.data$time, "-", v.data$machine)

      if (all(v.data$size > 1000L)) {
        NULL
      } else {
        if (nrow(v.data) >= 3) {
          if (nrow(v.data) == 3) {
            if (length(unique(v.data$ip_id)) == 1) {
              identify_triplets(v.data, time.window, time.sort)
            } else NULL
          } else if (nrow(v.data) > 3) {
            identify_triplets(v.data, time.window, time.sort)
          } else NULL
        }
      }
    }, mc.cores = cores)
  }

  if (output == "dataframe") {
    do.call(rbind, output)
  } else if (output == "list") {
    out
  } else {
    stop('output must be "list" or "dataframe".')
  }
}

identify_triplets <- function(v.data, time.window, time.sort) {
  freqtab <- table(v.data$id)
  triplets <- names(freqtab[freqtab == 3])
  small.id <- unique(v.data[v.data$size < 1000, "id"])

  size.test <- vapply(triplets, function(id) {
    tmp <- v.data[v.data$id == id, ]
    sz <- trunc(log10(tmp$size))
    three.different <- length(unique(sz)) == 3
    two.small <- sum(sz == min(sz)) == 2 & sum(sz == max(sz)) == 1

    large.test <- sum(sz == max(sz)) == 2 & sum(sz == min(sz)) == 1
    if (large.test) {
      lg.sizes <- unique(round(tmp$size[sz == max(sz)] / 10^max(sz)))
      two.large <- ifelse(length(lg.sizes) == 1, TRUE, FALSE)
    } else {
      two.large <- FALSE
    }

    three.different | two.small | two.large
  }, logical(1L))


  triplets <- triplets[size.test]

  if (length(triplets) != 0 & length(small.id) != 0) {
    if (identical(triplets, small.id)) {
      sel <- v.data$id %in% triplets
    } else {
      possible.triplets <- setdiff(small.id, triplets)
      leftovers <- v.data$id %in% possible.triplets

      if (sum(leftovers) < 3) {
        sel <- v.data$id %in% triplets
      } else {
        time.fix <- timeFix(possible.triplets, v.data, time.window)

        if (!is.null(time.fix)) {
          if (any(duplicated(time.fix$err))) {
            time.fix <- fixDuplicates(time.fix, v.data)
          }

          fixed.ids <- isTriplet(time.fix, v.data)
          unfixed.ids <- time.fix[time.fix$fix %in% fixed.ids, "err"]
          fixed.triplets <- c(unfixed.ids, fixed.ids)

          if (length(fixed.triplets) != 0) {
            sel <- v.data$id %in% c(triplets, fixed.triplets)
          } else sel <- NULL
        } else sel <- NULL
      }
    }
  } else if (length(triplets) != 0 & length(small.id) == 0) {
    sel <- v.data$id %in% triplets
  } else if (length(triplets) == 0 & length(small.id) != 0) {
    possible.triplets <- small.id
    time.fix <- timeFix(possible.triplets, v.data, time.window)

    if (any(duplicated(time.fix$err))) {
      time.fix <- fixDuplicates(time.fix, v.data)
    }

    if (!is.null(time.fix)) {
      fixed.ids <- isTriplet(time.fix, v.data)
      unfixed.ids <- time.fix[time.fix$fix %in% fixed.ids, "err"]
      fixed.triplets <- c(unfixed.ids, fixed.ids)

      if (length(fixed.triplets) != 0) {
        sel <- v.data$id %in% fixed.triplets
      } else {
        sel <- NULL
      }
    } else sel <- NULL
  }

  if (!is.null(sel)) {
    out <- v.data[sel, ]
    if (time.sort & nrow(out) != 0) {
      out$t2 <- as.POSIXlt(paste(out$date, out$time), tz = "Europe/Vienna")
      out <- out[order(out$t2), ]
      out[, c("machine", "id", "id2", "t2")] <- NULL
    } else if (!time.sort & nrow(out) != 0) {
      out[, c("machine", "id", "id2")] <- NULL
    }
    out
  }
}

timeFix <- function(possible.triplets, v.data, time.window) {
  out <- lapply(possible.triplets, function(x) {
    obs.data <- v.data[v.data$id %in% x, ]
    obs.time <- unique(as.POSIXlt(paste(obs.data$date, obs.data$time),
      tz = "Europe/Vienna"))
    tm.window <- c(obs.time + 1:time.window, obs.time - 1:time.window)
    candidate.hms <- strftime(tm.window, format = "%H:%M:%S",
      tz = "Europe/Vienna")
    candidate.id <- paste0(candidate.hms, "-", obs.data$machine)
    candidate <- v.data$id %in% candidate.id

    if (any(candidate)) {
      candidate.data <- v.data[v.data$id %in% candidate.id, ]
      majority.rule <- c(nrow(obs.data), nrow(candidate.data))
      err.id <- which.min(majority.rule)
      if (err.id == 1) {
        data.frame(err = unique(obs.data$id), fix = unique(candidate.data$id))
      } else if (err.id == 2) {
        data.frame(err = unique(candidate.data$id), fix = unique(obs.data$id))
      }
    }
  })
  unique(do.call(rbind, out))
}

fixDuplicates <- function(time.fix, v.data) {
  duplicates <- time.fix$err[duplicated(time.fix$err)]
  soln <- vapply(duplicates, function(x) {
    tmp <- time.fix[time.fix$err == x, ]
    tmp$err.time <- str2Time(tmp, time.fix, v.data)
    tmp$fix.time <- str2Time(tmp, time.fix, v.data, "fix")
    id.change <- which.max(abs(tmp$err.time - tmp$fix.time))
    row.names(time.fix[time.fix$err == x, ][id.change, ])
  }, character(1L))
  time.fix[!row.names(time.fix) %in% soln, ]
}

str2Time <- function(tmp, time.fix, v.data, var = "err", tz = "Europe/Vienna") {
  data.time <- vapply(strsplit(tmp[, var], "-"), function(x) {
    paste(v.data$date[1], x[1])
  }, character(1L))
  as.POSIXlt(data.time, tz = tz)
}

isTriplet <- function(time.fix, v.data) {
  test.data <- v.data
  for (i in seq_len(nrow(time.fix))) {
    test.data[test.data$id == time.fix[i, "err"], "id"] <- time.fix[i, "fix"]
  }

  count.test <- vapply(time.fix$fix, function(x) {
    nrow(test.data[test.data$id == x, ])
  }, integer(1L)) == 3

  candidates <- names(count.test[count.test])

  size.test <- vapply(candidates, function(id) {
    tmp <- test.data[test.data$id == id, ]
    sz <- trunc(log10(tmp$size))
    three.different <- length(unique(sz)) == 3
    two.small <- sum(sz == min(sz)) == 2 & sum(sz == max(sz)) == 1

    large.test <- sum(sz == max(sz)) == 2 & sum(sz == min(sz)) == 1
    if (large.test) {
      lg.sizes <- unique(round(tmp$size[sz == max(sz)] / 10^max(sz)))
      two.large <- ifelse(length(lg.sizes) == 1, TRUE, FALSE)
    } else {
      two.large <- FALSE
    }

    three.different | two.small | two.large
  }, logical(1L))

  candidates[size.test]
}
