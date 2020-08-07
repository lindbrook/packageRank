#' Identify IP's that are mirroring CRAN (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param dat Object. Package log entries.
#' @param cutpoint Numeric. Threshold number of unique packages downloaded.
#' @export

ipFilter <- function(dat, cutpoint = 15000L) {
  # number of unique packages downloaded by ip address.
  crosstab <- tapply(dat$package, dat$ip_id, function(x) length(unique(x)))
  df <- data.frame(ip = names(crosstab), count = c(crosstab), row.names = NULL)
  as.numeric(df[df$count >= cutpoint, "ip"])
}

#' Identify IP's that are mirroring CRAN (standalone prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date.
#' @param cutpoint Numeric. Threshold of unique packages downloaded.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

ipFilter0 <- function(date = Sys.Date() - 1, cutpoint = 5000L,
  memoization = TRUE) {

  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  sel <- !is.na(cran_log$package) & !is.na(cran_log$size)
  cran_log <- cran_log[sel, ]

  crosstab <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  df <- data.frame(ip = names(crosstab), count = c(crosstab), row.names = NULL)
  as.numeric(df[df$count >= cutpoint, "ip"])
}

#' Identify IP's that are mirroring CRAN (k-means standalone prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param date Character. Date.
#' @param output Character. "ip" vector of ip address; "df" data frame.
#' @param centers Numeric. Number of k's for k-means clustering.
#' @param nstart Numeric. Number of random sets.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

ipFilter2 <- function(date = Sys.Date() - 1, output = "ip", centers = 2L,
  nstart = 25L, memoization = TRUE) {

  date <- check10CharDate(date)
  ymd <- fixDate_2012(date)
  cran_log <- fetchCranLog(date = ymd, memoization = memoization)
  sel <- !is.na(cran_log$package) & !is.na(cran_log$size)
  cran_log <- cran_log[sel, ]

  crosstab <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  df <- data.frame(ip = names(crosstab), count = c(crosstab), row.names = NULL)
  df <- df[!duplicated(df$count), ]
  km <- stats::kmeans(stats::dist(df$count), centers = centers, nstart = nstart)
  out <- data.frame(ip = df$ip, size = df$count, group = km$cluster)

  if (length(unique(km$cluster)) == 1) {
    stop("No IP outliers!")
  } else if (length(unique(km$cluster)) > 1) {
    if (output == "ip") {
      grp <- as.numeric(names(which.min(table(out$group))))
      as.numeric(out[out$group == grp, "ip"])
    } else if (output == "df") {
      out[order(out$size, decreasing = TRUE), ]
    } else {
      stop('"output" must be "ip" or "df".')
    }
  }
}

#' Identify IP's that are mirroring CRAN (k-means helper prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param cran_log Object. cran log.
#' @param floor Character or Numeric. "auto" or floor of number of unique packages downloaded by an IP address.
#' @param output Character. "ip" vector of ip address; "df" data frame.
#' @param centers Numeric. Number of k's for k-means clustering.
#' @param nstart Numeric. Number of random sets.
#' @export

ipFilter3 <- function(cran_log, floor = "auto", output = "ip", centers = 2L,
  nstart = 25L) {

  if (!output %in% c("df", "ip")) stop('"output" must be "ip" or "df".')

  crosstab <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  if (floor == "auto") {
    df <- data.frame(ip = names(crosstab), count = c(crosstab),
      row.names = NULL)
    df <- df[!duplicated(df$count), ]
    km <- stats::kmeans(stats::dist(df$count), centers = centers,
      nstart = nstart)

    out <- data.frame(ip = df$ip, downloads = df$count, group = km$cluster)
    ip.country <- cran_log[!duplicated(cran_log$ip), c("ip_id", "country")]
    out <- merge(out, ip.country, by.x = "ip", by.y = "ip_id")
    out <- out[, c("ip", "country", "downloads", "group")]

    if (length(unique(km$cluster)) == 1) {
      stop("No IP outliers!")
    } else if (length(unique(km$cluster)) > 1) {
      if (output == "ip") {
        grp <- as.numeric(names(which.min(table(out$group))))
        out <- as.numeric(out[out$group == grp, "ip"])
      } else if (output == "df") {
        out <- out[order(out$downloads, decreasing = TRUE), ]
      }
    }
  } else if (is.numeric(floor)) {
    if (floor < 0) {
      stop('"floor" must be >= 0.')
    } else {
      if (output == "ip") {
        out <- as.numeric(names(crosstab[crosstab >= floor]))
      } else if (output == "df") {
        df <- data.frame(ip = names(crosstab), count = c(crosstab),
          row.names = NULL)
        df$group <- ifelse(df$count >= floor, 1, 2)
        out <- df[order(df$count, decreasing = TRUE), ]
      }
    }
  }
  out
}
