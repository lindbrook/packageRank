#' Filter out small downloads triplets (prototype).
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @export

tripletFilter <- function(dat) {
  dat2 <- dat

  ## machine ID and time stamp ID

  dat2$machine.id <- paste0(dat2$ip_id, "-",
                           dat2$r_version, "-",
                           dat2$r_os, "-",
                           dat2$version)

  dat2$id <- paste0(dat2$time, "-", dat2$machine.id)

  ## identify ID stamps with a ~500 B entry ##

  id_500 <- dat2$size < 1000
  crosstab <- table(dat2[id_500, "id"])

  ## test/identity complete triplets ##

  triplet.test <- unlist(lapply(names(crosstab), function(x) {
    test1 <- sum(dat2$id %in% x) == 3
    if (test1) {
      orders.magniutde <- ceiling(log10(dat[dat2$id %in% x, "size"]))
      # test2 <- sum(orders.magniutde == max(orders.magniutde)) == 1
      test2 <- length(unique(orders.magniutde)) > 1
      test1 & test2
    } else FALSE
  }))

  if (any(triplet.test)) {
    complete.triplets <- names(crosstab)[triplet.test]
  } else {
    complete.triplets <- NULL
  }

  ## test/identify potential triplets ##

  sel <- dat2$id %in% names(crosstab)[!triplet.test]
  triplet.candidate <- dat2[id_500 & sel, "id"]

  candidate.neighbor <- lapply(triplet.candidate, function(x) {
    id.components <- unlist(strsplit(x, "-"))
    machine.id <- paste(id.components[-1], collapse = "-")
    before.after <- packageRank::timeWindow(id.components[1])
    candidate <- paste0(before.after, "-", machine.id)
    neighbor.test <- lapply(candidate, function(x) dat2$id %in% x)

    if (any(unlist(neighbor.test))) {
      err <- unlist(lapply(candidate, function(x) dat2[dat2$id %in% x, "id"]))
      data.frame(fix = x, err = unique(err), stringsAsFactors = FALSE)
    }
  })

  candidate.neighbor <- do.call(rbind, candidate.neighbor)

  dat2$id2 <- dat2$id

  ## fix potential triplet id stamps ##

  if (!is.null(candidate.neighbor)) {
    for (i in seq_len(nrow(candidate.neighbor))) {
      sel <- dat2$id2 %in% candidate.neighbor[i, "err"]
      dat2[sel, "id2"] <- candidate.neighbor[i, "fix"]
    }

    ## test candidate triplets ##

    candidate.test <- vapply(triplet.candidate, function(x) {
      tri.data <- dat[dat2$id2 %in% x, ]
      if (nrow(tri.data) == 3) {
        size <- ceiling(log10(tri.data$size))
        any(size != max(size))
      } else {
        FALSE
      }
    }, logical(1L))

    fixed.triplets <- names(candidate.test[candidate.test])
  } else {
    fixed.triplets <- NULL
  }

  ##  ##

  triplets <- c(complete.triplets, fixed.triplets)

  deletions <- lapply(triplets, function(tri) {
    size.data <- dat2[dat2$id2 %in% tri, ]
    out <- size.data[which(size.data$size != max(size.data$size)), ]
    as.numeric(row.names(out))
  })

  dat[unlist(deletions), ]
}
