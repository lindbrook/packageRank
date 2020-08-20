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
  vers <- unique(dat$version)
  crosstab <- table(dat$version)

  if (any(crosstab <= 2)) {
    too.few.obs <- names(crosstab[crosstab <= 2])
    vers <- setdiff(vers, too.few.obs)
    leftover <- row.names(dat[dat$version %in% too.few.obs, ])
  }

  if (length(vers) != 0) {
    size.audit <- vapply(vers, function(v) {
      length(unique(round(log10(dat[dat$version == v , "size"]))))
    }, integer(1L))

    if (any(size.audit == 1)) {
      no.variance <- names(size.audit[size.audit == 1])
      unclassified <- row.names(dat[dat$version %in% no.variance, ])
    } else if (any(size.audit >= 2)) {
      vers <- vers[size.audit >= 2]
      classified <- unlist(lapply(vers, function(v) {
        tmp <- dat[dat$version == v , ]
        km <- stats::kmeans(stats::dist(tmp$size), centers = centers,
          nstart = nstart)
        clusters <- data.frame(size = tmp$size, group = km$cluster)
        size <- tapply(clusters$size, clusters$group, mean)
        large.id <- as.numeric(names(which.max(size)))
        large.size <- tmp[clusters$group %in% large.id, "size"]
        row.names(tmp[tmp$size %in% large.size, ])
      }))
    }

    l.test <- ifelse("leftover" %in% ls(),length(leftover) > 0, FALSE)
    u.test <- ifelse("unclassified" %in% ls(), length(unclassified) > 0, FALSE)
    c.test <- ifelse("classified" %in% ls(), length(classified) > 0, FALSE)

    if (l.test & u.test & c.test) {
      sel <- c(leftover, unclassified, classified)
    } else if (l.test & u.test & !c.test) {
      sel <- c(leftover, unclassified)
    } else if (l.test & !u.test & c.test) {
      sel <- c(leftover, classified)
    } else if (!l.test & u.test & c.test) {
      sel <- c(unclassified, classified)
    } else if (l.test & !u.test & !c.test) {
      sel <- leftover
    } else if (!l.test & u.test & !c.test) {
      sel <- unclassified
    } else if (!l.test & !u.test & c.test) {
      sel <- classified
    }

    dat[row.names(dat) %in% sel, ]
  }
}
