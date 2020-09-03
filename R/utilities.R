#' Extract Package Logs.
#'
#' @param i Numeric. Day/ID.
#' @param pkg Character.
#' @param dat Object. List of logs.
#' @export

pkgLog0 <- function(i = 1, pkg = "cholera", dat) {
  cran_log <- dat[[i]]
  tmp <- cran_log[!is.na(cran_log$package) & cran_log$package == pkg, ]
  tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "Europe/Vienna")
  tmp[order(tmp$t2), c(1:6, 8:10)]
}


#' Extract Package Logs.
#'
#' @param i Numeric. Day/ID.
#' @param triplet.filter Logical.
#' @param ip.filter Logical.
#' @param use.campaigns Logical.
#' @param small.filter Logical.
#' @param sequence.filter Logical.
#' @param pkg Character.
#' @param dat Object. List of logs.
#' @param multi.core Logical or Numeric.
#' @export

pkgLog <- function(i = 1, triplet.filter = TRUE, ip.filter = TRUE,
  use.campaigns = TRUE, small.filter = TRUE, sequence.filter = TRUE,
  pkg = "cholera", dat, multi.core = TRUE) {

  cores <- multiCore(multi.core)
  cran_log <- dat[[i]]
  tmp <- cran_log[!is.na(cran_log$package) & cran_log$package == pkg, ]

  if (triplet.filter) tmp <- do.call(rbind, tripletFilter(tmp))

  if (ip.filter) {
    ip.outliers <- ipFilter3(cran_log)
    if (use.campaigns) {
      row.delete <- unlist(parallel::mclapply(ip.outliers, function(x) {
        campaigns(x, cran_log)
      }, mc.cores = cores))
      tmp <- tmp[!row.names(tmp) %in% row.delete, ]
    } else {
      tmp <- tmp[!tmp$ip_id %in% ip.outliers, ]
    }
  }

  if (small.filter) {
    size.audit <- length(unique(round(log10(tmp$size))))
    if (size.audit > 1) tmp <- smallFilter0(tmp)
  }

  if (sequence.filter) tmp <- sequenceFilter(tmp)

  tmp$t2 <- as.POSIXlt(paste(tmp$date, tmp$time), tz = "Europe/Vienna")
  tmp[order(tmp$t2), !names(tmp) %in% "t2"]
}
