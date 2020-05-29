#' Filter out small downloads triplets (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @export

tripletFilter2 <- function(dat) {
  ver <- unique(dat$version)

  out <- lapply(ver, function(v) {
    v.data <- dat[dat$version == v, ]

    if (nrow(v.data) == 3) {
      if (any(v.data$size < 1000)) {
        size.heterogeneity <- length(unique(ceiling(log10(v.data$size)))) == 3
        if (size.heterogeneity) {
           obs <- v.data[v.data$size != max(v.data$size), ]
           tri.delete <- as.numeric(row.names(obs))
         } else {
         tri.delete <- NULL
        }
      }
    } else {
      v.data$machine <- paste0(v.data$ip_id, "-",
                               v.data$r_version, "-",
                               v.data$r_os)

      v.data$id <- paste0(v.data$time, "-", v.data$machine)

      crosstab <- table(v.data$id)
      triplets <- names(crosstab[crosstab == 3])

      tri.delete <- unlist(lapply(triplets, function(id) {
        tmp <- v.data[v.data$id %in% id, ]

        if (any(tmp$size < 1000)) {
          size.heterogeneity <- length(unique(ceiling(log10(tmp$size)))) == 3
          sz <- ceiling(log10(tmp$size))
          sm.pkg.triA <- any(sz == 6)
          sm.pkg.triB <- sum(sz == max(sz)) == 2
          small.package.triplet <- sm.pkg.triA & sm.pkg.triB

          if (size.heterogeneity) {
            obs <- tmp[tmp$size != max(tmp$size), ]
            as.numeric(row.names(obs))
          } else if (small.package.triplet) {
            as.numeric(row.names(tmp[tmp$size != max(tmp$size), ]))
          } else NULL
        }
      }))
    }

    v.data <- v.data[row.names(v.data) %in% tri.delete == FALSE, ]

    possible.triplets <- v.data[v.data$size < 1000, "id"]

    if (!is.null(possible.triplets)) {
      time.shift.triplet <- lapply(possible.triplets, function(x) {
        id.components <- unlist(strsplit(x, "-"))
        machine.id <- paste(id.components[-1], collapse = "-")
        before.after <- packageRank::timeWindow(id.components[1])
        candidate <- paste0(before.after, "-", machine.id)
        neighbor.test <- lapply(candidate, function(x) v.data$id %in% x)

        if (any(unlist(neighbor.test))) {
          err <- unlist(lapply(candidate, function(x) {
            v.data[v.data$id %in% x, "id"]
          }))
          data.frame(fix = x, err = unique(err), stringsAsFactors = FALSE)
        }
      })

      other.triplets <- do.call(rbind, time.shift.triplet)
    }

    if (!is.null(other.triplets)) {
      time.shift.select <- vapply(seq_len(nrow(other.triplets)), function(i) {
        window <- other.triplets[i, ]
        sum(v.data$id %in% window[, c("fix", "err")]) == 3
      }, logical(1L))

      candidates <- other.triplets[time.shift.select, ]

      delete <- lapply(seq_len(nrow(candidates)), function(i) {
        window <- candidates[i, ]
        window.select <- v.data$id %in% window[, c("fix", "err")]
        tmp <- v.data[window.select, ]
        sz <- ceiling(log10(tmp$size))
        three.different <- length(unique(sz)) == 3
        two.different <- sum(sz == max(sz)) == 2
        sm.pkg <- max(floor(log10(tmp$size))) <= 5

        if (three.different | (two.different & sm.pkg)) {
          as.numeric(row.names(tmp[tmp$size != max(tmp$size), ]))
        }
      })

      delete <- do.call(c, delete)

      if (!is.null(delete)) {
        v.data <- v.data[row.names(v.data) %in% delete == FALSE, ]
      }
    }

    v.data <- v.data[v.data$size >= 1000, ]
    v.data[, c("machine", "id")] <- NULL
    v.data
  })

  out
}
