#' Compute Top N Downloads by Country Code.
#'
#' @param month_cran_log Object.
#' @param top.n Integer.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores to use. Note that due to performance considerations, the number of cores defaults to one on Windows.
#' @export

topCountryCodes <- function(month_cran_log, top.n = 5L, multi.core = FALSE) {
  cores <- multiCore(multi.core)

  top_n <- parallel::mclapply(month_cran_log, function(x) {
    tmp <- x[!is.na(x$package) & !is.na(x$country), ]
    freqtab <- sort(table(tmp$country), decreasing = TRUE)
    data.frame(country = names(freqtab)[1:top.n],
      downloads = c(freqtab)[1:top.n], stringsAsFactors = FALSE)
  }, mc.cores = cores)

  out <- do.call(rbind, top_n)
  out$date <- rep(names(month_cran_log), each = top.n)
  out$id <- rep(1:top.n, times = length(month_cran_log))
  row.names(out) <- NULL
  out
}

#' Plot Top N Downloads by Country Code.
#'
#' @param dataset Character.
#' @param second.place Logical. Annotate second place country.
#' @importFrom sugrrants facet_calendar
#' @export

plotTopCountryCodes <- function(dataset = "october", second.place = FALSE) {
  dataset <- tolower(dataset)
  if (dataset == "october") {
    dat <- packageRank::blog.data$top.n.oct2019
  } else if (dataset == "july") {
    dat <- packageRank::blog.data$top.n.jul2020
  } else stop('dataset must be "july" or "october".')

  dat$downloads <- dat$downloads / 10^6

  p <- ggplot2::ggplot(data = dat,
    ggplot2::aes(x = .data$id, y = .data$downloads, label = .data$country)) +
    ggplot2::geom_line(linewidth = 0.125) +
    ggplot2::geom_point(data = dat[dat$id == 1, ], color = "red") +
    ggplot2::geom_text(data = dat[dat$id == 1, ], nudge_x = 0.8, size = 3, 
      color = "red")

    if (second.place) {
      p <- p + ggplot2::geom_point(data = dat[dat$id == 2, ],
                                   color = "dodgerblue",
                                   shape = 15) +
               ggplot2::geom_text(data = dat[dat$id == 2, ],
                                  nudge_x = 0.8,
                                  nudge_y = 0.4,
                                  size = 3,
                                  color = "dodgerblue") +
               ggplot2::geom_point(data = dat[!dat $id %in% 1:2, ], size = 1)
    } else {
      p <- p + ggplot2::geom_point(data = dat[dat$id != 1, ], size = 1)
    }

    p + ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    sugrrants::facet_calendar(~ as.Date(date), week_start = 7) +
    ggplot2::scale_x_continuous(limits = c(0.5, max(dat$id) + 0.5)) +
    ggplot2::scale_y_continuous(breaks = c(0, 1, 2), limits = c(0, 3)) +
    ggplot2::xlab("Rank") +
    ggplot2::ylab("Downloads (millions)")
}

#' Compute Downloads by Country Code.
#'
#' @param month_cran_log Object.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores to use. Note that due to performance considerations, the number of cores defaults to one on Windows.
#' @export

downloadsCountry <- function(month_cran_log, multi.core = FALSE) {
  dl.data <- parallel::mclapply(month_cran_log, function(x) {
    tmp <- x[!is.na(x$package) & !is.na(x$country), ]
    freqtab <- sort(c(table(tmp$country)), decreasing = TRUE)
    data.frame(country = names(freqtab), downloads = freqtab,
      stringsAsFactors = FALSE)
  }, mc.cores = multiCore(multi.core))

  ct <- vapply(month_cran_log, function(x) {
    x <- x[!is.na(x$package) & !is.na(x$country), ]
    length(unique(x$country))
  }, integer(1L))

  dts <- unlist(lapply(seq_along(ct), function(i) rep(names(ct[i]), ct[i])))
  id <- unlist(lapply(ct, function(x) 1:x))
  out <- do.call(rbind, dl.data)
  out$date <- dts
  out$id <- id
  row.names(out) <- NULL
  out
}

#' Plot Compute Downloads by Country Code.
#'
#' @importFrom sugrrants facet_calendar
#' @export

plotDownloadsCountry <- function() {
  dat <- packageRank::blog.data$download.country
  dat$downloads <- dat$downloads / 10^6
  ggplot2::ggplot(data = dat, ggplot2::aes(x = .data$id, y = .data$downloads, label = .data$country)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    sugrrants::facet_calendar(~ as.Date(date), week_start = 7) +
    ggplot2::geom_point(data = dat[dat$id %in% 1:2, ], shape = 1, color = "red",
      stroke = 1) +
    ggplot2::geom_text(data = dat[dat$id %in% 1:2, ], nudge_x = 45, size = 3,
      color = "red") +
    ggplot2::geom_point(size = 0.5) +
    ggplot2::scale_x_continuous(breaks = c(0, 100, 200)) +
    ggplot2::scale_y_continuous(breaks = c(0, 1, 2), limits = c(-0.25, 2.75)) +
    ggplot2::xlab("Rank") +
    ggplot2::ylab("Downloads (millions)")
}
