#' Filter out small downloads (prototype).
#'
#' @param dat Object. Package log entries.
#' @param filter Logical or Numeric. If Logical, TRUE filters out downloads less than 1000 bytes. If Numeric, a positive value sets the minimum download size (in bytes) to consider; a negative value sets the maximum download size to consider.
#' @export

smallFilter <- function(dat, filter = TRUE) {
  if (is.numeric(filter)) {
    if (filter >= 0) {
        dat[dat$size >= filter, ]
      } else if (filter < 0) {
        dat[dat$size < -filter, ]
      }
  } else if (is.logical(filter)) {
    dat[dat$size >= 1000, ]
  } else {
    stop("'filter' must be Logical or Numeric.")
  }
}

#' Filter out small downloads (k-means prototype helper).
#'
#' @param dat Object. Package log entries.
#' @param centers Numeric. Number of k's for k-means clustering.
#' @param nstart Numeric. Number of random sets.
#' @export

smallFilter0 <- function(dat, centers = 2L, nstart = 25L) {
  dat <- dat[!is.na(dat$package), ]
  dat0 <- dat[!duplicated(dat$size), ]
  vers <- unique(dat0$version)
  crosstab <- table(dat0$version)

  if (any(crosstab <= 2)) {
    too.few.obs <- names(crosstab[crosstab <= 2])
    vers <- setdiff(vers, too.few.obs)
    leftover <- row.names(dat[dat$version %in% too.few.obs, ])
  }

  if (length(vers) != 0) {
    # median package size is 97000 via median(packageInfo()$byte)
    size.audit <- vapply(vers, function(v) {
      length(unique(round(log10(dat0[dat0$version == v , "size"]))))
    }, integer(1L))

    vers <- vers[size.audit >= 2]

    classified <- lapply(vers, function(v) {
      tmp <- dat0[dat0$version == v , ]
      km <- stats::kmeans(stats::dist(tmp$size), centers = centers,
        nstart = nstart)
      clusters <- data.frame(size = tmp$size, group = km$cluster)
      size <- tapply(clusters$size, clusters$group, mean)
      large.id <- as.numeric(names(which.max(size)))
      large.size <- tmp[clusters$group %in% large.id, "size"]
      row.names(tmp[tmp$size %in% large.size, ])
    })
  }

  if (any(crosstab <= 2) & length(vers) != 0) {
    dat[row.names(dat) %in% c(unlist(classified), leftover), ]
  } else if (any(crosstab <= 2) & length(vers) == 0) {
    dat[row.names(dat) %in% leftover, ]
  } else if (all(crosstab >= 2 & length(vers) != 0)){
    dat[row.names(dat) %in% unlist(classified), ]
  }
}

# packageLog("cholera", date = "2020-07-23")
