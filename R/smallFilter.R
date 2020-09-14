#' Filter out small downloads (hierachical cluster prototype helper).
#'
#' @param dat Object. Package log entries.
#' @param filter Logical or Numeric. If Logical, TRUE filters out downloads less than 1000 bytes. If Numeric, a positive value sets the minimum download size (in bytes) to consider; a negative value sets the maximum download size to consider.
#' @export

smallFilter <- function(dat, filter = TRUE) {
  vers <- unique(dat$version)
  crosstab <- table(dat$version)

  if (any(crosstab <= 2)) {
    too.few.obs <- names(crosstab[crosstab <= 2])
    vers <- setdiff(vers, too.few.obs)
    leftover <- row.names(dat[dat$version %in% too.few.obs, ])
  }

  if ("leftover" %in% ls()) {
    if (length(leftover) == nrow(dat)) {
      sel <- "leftover"
    } else {
      if (length(vers) != 0) {
        size.audit <- vapply(vers, function(v) {
          length(unique(trunc(log10(dat[dat$version == v , "size"]))))
        }, integer(1L))

        obs.ct.audit <- vapply(vers, function(v) {
          length(unique(dat[dat$version == v , "size"]))
        }, integer(1L))

        if (any(size.audit == 1)) {
          no.variance <- names(size.audit[size.audit == 1])
          size.null <- row.names(dat[dat$version %in% no.variance, ])
        }

        if (any(obs.ct.audit <= 2)) {
          two.obs <- names(obs.ct.audit[obs.ct.audit <= 2])
          obs.null <- row.names(dat[dat$version %in% two.obs, ])
        }

        if (any(size.audit >= 2 & obs.ct.audit > 2)) {
          classified <- hcClassifier(vers, dat, size.audit, obs.ct.audit)
        }

        l.test <- ifelse("leftover" %in% ls(), length(leftover) > 0, FALSE)
        s.test <- ifelse("size.null" %in% ls(), length(size.null) > 0, FALSE)
        o.test <- ifelse("obs.null" %in% ls(), length(obs.null) > 0, FALSE)
        c.test <- ifelse("classified" %in% ls(), length(classified) > 0, FALSE)

        if (l.test & s.test & o.test & c.test) {
          sel <- c(leftover, size.null, obs.null, classified)

        } else if (l.test & s.test & o.test & !c.test) {
          sel <- c(leftover, size.null, obs.null)
        } else if (l.test & s.test & !o.test & c.test) {
          sel <- c(leftover, size.null, classified)
        } else if (l.test & !s.test & o.test & c.test) {
          sel <- c(leftover, obs.null, classified)
        } else if (!l.test & s.test & o.test & c.test) {
          sel <- c(size.null, obs.null, classified)

        } else if (l.test & s.test & !o.test & !c.test) {
          sel <- c(leftover, size.null)
        } else if (l.test & !s.test & o.test & !c.test) {
          sel <- c(leftover, obs.null)
        } else if (l.test & !s.test & !o.test & c.test) {
          sel <- c(leftover, classified)
        } else if (!l.test & s.test & o.test & !c.test) {
          sel <- c(size.null, obs.null)
        } else if (!l.test & s.test & !o.test & c.test) {
          sel <- c(size.null, classified)
        } else if (!l.test & !s.test & o.test & c.test) {
          sel <- c(obs.null, classified)

        } else if (l.test & !s.test & !o.test & !c.test) {
          sel <- leftover
        } else if (!l.test & s.test & !o.test & !c.test) {
          sel <- size.null
        } else if (!l.test & !s.test & o.test & !c.test) {
          sel <- obs.null
        } else if (!l.test & !s.test & !o.test & c.test) {
          sel <- classified
        }
      }
    }
  } else {
    if (length(vers) != 0) {
      size.audit <- vapply(vers, function(v) {
        length(unique(round(log10(dat[dat$version == v , "size"]))))
      }, integer(1L))

      obs.ct.audit <- vapply(vers, function(v) {
        length(unique(dat[dat$version == v , "size"]))
      }, integer(1L))

      if (any(size.audit == 1)) {
        no.variance <- names(size.audit[size.audit == 1])
        size.null <- row.names(dat[dat$version %in% no.variance, ])
      }

      if (any(obs.ct.audit <= 2)) {
        two.obs <- names(obs.ct.audit[obs.ct.audit <= 2])
        obs.null <- row.names(dat[dat$version %in% two.obs, ])
      }

      if (any(size.audit >= 2 & obs.ct.audit > 2)) {
        classified <- hcClassifier(vers, dat, size.audit, obs.ct.audit)
      }

      s.test <- ifelse("size.null" %in% ls(), length(size.null) > 0, FALSE)
      o.test <- ifelse("obs.null" %in% ls(), length(obs.null) > 0, FALSE)
      c.test <- ifelse("classified" %in% ls(), length(classified) > 0, FALSE)

      if (s.test & o.test & c.test) {
        sel <- c(size.null, obs.null, classified)

      } else if (s.test & o.test & !c.test) {
        sel <- c(size.null, obs.null)
      } else if (s.test & !o.test & c.test) {
        sel <- c(size.null, classified)
      } else if (!s.test & o.test & c.test) {
        sel <- c(obs.null, classified)

      } else if (s.test & !o.test & !c.test) {
        sel <- size.null
      } else if (!s.test & o.test & !c.test) {
        sel <- obs.null
      } else if (!s.test & !o.test & c.test) {
        sel <- classified
      }
    }
  }

  out <- dat[row.names(dat) %in% sel, ]
  smallFilter(out, filter = filter)
}

hcClassifier <- function(vers, dat, size.audit, obs.ct.audit) {
   vers <- vers[size.audit >= 2 & obs.ct.audit > 2]
   unlist(lapply(vers, function(v) {
      v.data <- dat[dat$version == v, ]
      tmp <- v.data[!duplicated(v.data$size), ]
      # tmp <- tmp[tmp$size > 0, ] # exclude downloads of size 0.
      orders.magnitude <- trunc(log10(tmp$size))
      ds <- stats::dist(orders.magnitude)
      ca <- stats::hclust(ds)
      cut.ca <- stats::cutree(ca, h = max(ds) - 1)
      clusters <- data.frame(size = orders.magnitude, id = cut.ca)
      mean.cluster.size <- tapply(clusters$size, clusters$id, mean)
      cluster.to.select <- as.numeric(names(which.max(mean.cluster.size)))
      sizes.to.select <- tmp[clusters$id %in% cluster.to.select, "size"]
      row.names(v.data[v.data$size %in% sizes.to.select, ])
   }))
}

#' Filter out small downloads (prototype).
#'
#' @param dat Object. Package log entries.
#' @param filter Logical or Numeric. If Logical, TRUE filters out downloads less than 1000 bytes. If Numeric, a positive value sets the minimum download size (in bytes) to consider; a negative value sets the maximum download size to consider.
#' @export

smallFilter0 <- function(dat, filter = TRUE) {
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
