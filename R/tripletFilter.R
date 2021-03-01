#' Filter out small downloads triplets (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @param time.window Numeric. Seconds.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

tripletFilter <- function(dat, time.window = 2, multi.core = TRUE) {
  triplets <- identifyTriplets(dat, time.window = time.window,
    multi.core = multi.core)
  if (!is.null(triplets)) {
    delete <- row.names(triplets[seq_len(nrow(triplets)) %% 3 != 0, ])
    if (!is.null(delete)) {
      dat[row.names(dat) %in% delete == FALSE, ]
    } else dat
  } else dat
}

#' Extract triplets from logs (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. CRAN log data frame
#' @param output Character. "list" or "data.frame".
#' @param time.window Numeric. Seconds.
#' @param time.sort Logical. Sort output by time.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @noRd

identifyTriplets <- function(dat, output = "data.frame", time.window = 2,
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

    if (output == "data.frame") {
      do.call(rbind, out)
    } else if (output == "list") {
      out
    } else {
      stop('output must be "list" or "dataframe".')
    }
  }
}

identify_triplets <- function(v.data, time.window, time.sort) {
  freqtab <- table(v.data$id)
  triplets <- names(freqtab[freqtab == 3])
  size.test <- sizeTest(triplets, v.data)
  triplets <- triplets[size.test]
  small.id <- unique(v.data[v.data$size < 1000L, "id"])

  if (length(triplets) != 0 & length(small.id) != 0) {
    if (!identical(triplets, small.id)) {
      potential.triplets <- setdiff(small.id, triplets)

      if (length(potential.triplets) > 0) {
        # select "correct" time by majority rule
        time.fix <- timeFix(potential.triplets, v.data, time.window)

        if (!is.null(time.fix)) {
          if (any(duplicated(time.fix$minority))) {
            time.fix <- fixDuplicates(time.fix, v.data)
          }

          root <- isTriplet(time.fix, v.data)
          complement <- time.fix[time.fix$majority %in% root, "minority"]
          latent.triplets <- c(root, complement)

          if (length(latent.triplets) != 0) {
            sel <- v.data$id %in% c(triplets, latent.triplets)
          } else sel <- v.data$id %in% triplets
        } else sel <- v.data$id %in% triplets
      } else sel <- v.data$id %in% triplets
    } else sel <- v.data$id %in% triplets

  } else if (length(triplets) != 0 & length(small.id) == 0) {
    sel <- v.data$id %in% triplets

  } else if (length(triplets) == 0 & length(small.id) != 0) {
    potential.triplets <- small.id
    time.fix <- timeFix(potential.triplets, v.data, time.window)
    if (!is.null(time.fix)) {
      if (any(duplicated(time.fix$minority))) {
        time.fix <- fixDuplicates(time.fix, v.data)
      }

      root <- isTriplet(time.fix, v.data)
      complement <- time.fix[time.fix$majority %in% root, "minority"]
      latent.triplets <- c(root, complement)

      if (length(latent.triplets) != 0) {
        sel <- v.data$id %in% latent.triplets
      } else sel <- v.data$id %in% triplets
    } else sel <- v.data$id %in% triplets
  }

  out <- v.data[sel, ]

  if (nrow(out) != 0) {
    out$t2 <- dateTime(out$date, out$time)
    if ("time.fix" %in% ls()) {
      t.adj <- time.fix[time.fix$minority %in% out$id, "majority"]
      t.adj <- vapply(t.adj, function(x) x[1], character(1L))
      t.adj <- dateTime(unique(out$date), t.adj)
      t.delta <- out[out$id %in% time.fix$minority, "t2"] - t.adj
      sel <- out$id %in% time.fix$minority
      out[sel, "t2"] <- out[sel, "t2"] - t.delta

      id.adj <- strsplit(out[sel, "id"], "-")
      id.adj <- lapply(id.adj, function(x) x[-1])
      t.fix <- strftime(t.adj, format = "%H:%M:%S", tz = "GMT")

      id2 <- vapply(seq_along(id.adj), function(i) {
        paste0(t.fix[i], "-", paste(id.adj[[i]], collapse = "-"))
      }, character(1L))

      out[out$id %in% time.fix$minority, "id"] <- id2
    }

    if (time.sort) out <- out[order(out$t2, out$size), ]
  }

  out[, c("machine", "id")] <- NULL
  out
}

timeFix <- function(potential.triplets, v.data, time.window) {
  out <- lapply(potential.triplets, function(x) {
    obs.data <- v.data[v.data$id %in% x, ]
    obs.time <- dateTime(obs.data$date, obs.data$time)
    tm.window <- c(obs.time + 1:time.window, obs.time - 1:time.window)
    candidate.hms <- strftime(tm.window, format = "%H:%M:%S",
      tz = "GMT")
    candidate.id <- paste0(candidate.hms, "-", obs.data$machine)
    candidate <- v.data$id %in% candidate.id

    if (any(candidate) & sum(nrow(obs.data), sum(candidate)) == 3) {
      candidate.data <- v.data[v.data$id %in% candidate.id, ]
      minority <- which.min(c(nrow(obs.data), nrow(candidate.data)))
      if (minority == 1) {
        data.frame(minority = unique(obs.data$id),
                   majority = unique(candidate.data$id))
      } else if (minority == 2) {
        data.frame(minority = unique(candidate.data$id),
                   majority = unique(obs.data$id))
      }
    }
  })
  unique(do.call(rbind, out))
}

fixDuplicates <- function(time.fix, v.data) {
  duplicates <- time.fix$minority[duplicated(time.fix$minority)]
  soln <- vapply(duplicates, function(x) {
    tmp <- time.fix[time.fix$minority == x, ]
    tmp$minority.time <- str2Time(tmp, time.fix, v.data)
    tmp$majority.time <- str2Time(tmp, time.fix, v.data, "majority")
    id.change <- which.max(abs(tmp$minority.time - tmp$majority.time))
    row.names(time.fix[time.fix$minority == x, ][id.change, ])
  }, character(1L))
  time.fix[!row.names(time.fix) %in% soln, ]
}

str2Time <- function(tmp, time.fix, v.data, var = "minority", tz = "GMT") {
  data.time <- vapply(strsplit(tmp[, var], "-"), function(x) {
    paste(v.data$date[1], x[1])
  }, character(1L))
  as.POSIXlt(data.time, tz = tz)
}

isTriplet <- function(time.fix, v.data) {
  test.data <- v.data

  for (i in seq_len(nrow(time.fix))) {
    sel <- test.data$id == time.fix[i, "minority"]
    test.data[sel, "id"] <- time.fix[i, "majority"]
  }

  count.test <- vapply(time.fix$majority, function(x) {
    nrow(test.data[test.data$id == x, ])
  }, integer(1L)) == 3

  candidates <- names(count.test[count.test])
  size.test <- sizeTest(candidates, test.data)
  candidates[size.test]
}

sizeTest <- function(candidates, v.data) {
  vapply(candidates, function(id) {
    tmp <- v.data[v.data$id == id, ]
    sum(tmp$size < 1000L) == 1
  }, logical(1L))
}
