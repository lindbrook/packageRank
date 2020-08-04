#' Compute Top N Downloads by Country Code.
#'
#' @param month_cran_log Object.
#' @param top.n Integer.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores to use. Note that due to performance considerations, the number of cores defaults to one on Windows.
#' @export

topCountryCodes <- function(month_cran_log, top.n = 5L, multi.core = TRUE) {
  cores <- multiCore(multi.core)

  top_n <- parallel::mclapply(month_cran_log, function(x) {
    tmp <- x[!is.na(x$package) & !is.na(x$country), ]
    crosstab <- sort(table(tmp$country), decreasing = TRUE)
    data.frame(country = names(crosstab)[1:top.n],
      downloads = c(crosstab)[1:top.n], stringsAsFactors = FALSE)
  }, mc.cores = cores)

  out <- do.call(rbind, top_n)
  out$date <- rep(names(month_cran_log), each = top.n)
  out$id <- rep(1:top.n, times = length(month_cran_log))
  row.names(out) <- NULL
  out
}

#' Plot Top N Downloads by Country Code.
#'
#' @param dat Object.
#' @param log.downloads Logical. Logarithm of package downloads.
#' @export

plotTopCountryCodes <- function(dat = packageRank::blog.data$top.n,
  log.downloads = FALSE) {

  # p <- ggplot(data = dat, aes(x = id, y = downloads, label = country)) +
  p <- ggplot(data = dat, aes_string(x = "id", y = "downloads", label = "country")) +
    geom_line(color = "red", size = 1/3) +
    geom_text(size = 3.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    facet_wrap(~ date, ncol = 7) +
    scale_x_continuous(limits = c(0.5, max(dat$id) + 0.5))
  if (log.downloads) p + scale_y_log10() else p
}
