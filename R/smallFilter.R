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
  sm.entry <- dat$size < 1000
  vers <- unique(dat[!sm.entry, "version"])
  crosstab <- table(dat[!sm.entry, "version"])

  if (any(crosstab <= 2)) {
    too.few.obs <- names(crosstab[crosstab <= 2])
    vers <- setdiff(vers, too.few.obs)
    # leftover <- row.names(dat[dat$version %in% too.few.obs & !sm.entry, ])
    leftover <- row.names(dat[dat$version %in% too.few.obs, ])
  }

  if (length(vers) != 0) {
    # median package size is 97000 via median(packageInfo()$byte)
    classified <- lapply(vers, function(v) {
      tmp <- dat[dat$version == v , ]
      km <- stats::kmeans(stats::dist(tmp$size), centers = centers,
        nstart = nstart)
      clusters <- data.frame(size = tmp$size, group = km$cluster)
      size <- tapply(clusters$size, clusters$group, mean)
      large.id <- as.numeric(names(which.max(size)))
      row.names(tmp[clusters$group %in% large.id, ])
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
