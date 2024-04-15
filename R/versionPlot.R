#' Version Plot.
#'
#' Document code for blog graph.
#' @importFrom sugrrants facet_calendar
#' @export

versionPlot <- function() {
  cran.pkg_ver <- packageRank::blog.data$cran.pkg_ver

  for (i in seq_along(cran.pkg_ver)) {
    cran.pkg_ver[[i]]$pkg.id <- seq_len(nrow(cran.pkg_ver[[i]]))
  }

  pkg.dataA <- do.call(rbind, cran.pkg_ver)
  archive.pkg_ver <- packageRank::blog.data$archive.pkg_ver

  for (i in seq_along(archive.pkg_ver)) {
    archive.pkg_ver[[i]]$pkg.id <- 100:199
  }

  pkg.dataB <- do.call(rbind, archive.pkg_ver)
  pkg.data <- rbind(pkg.dataA, pkg.dataB)
  pkg.data$pct.obs.exp <- 100 * pkg.data$pct.obs.exp

  titleA <- "Percent of Package-Versions Downloaded:"
  titleB <- "Active & Inactive Packages"

  geom.col <- grDevices::adjustcolor("red", alpha.f = 0.5)

  ggplot2::ggplot(data = pkg.data, ggplot2::aes(x = .data$pkg.id, y = .data$pct.obs.exp)) +
    ggplot2::geom_line(col = geom.col, linewidth = 0.5) +

    # https://github.com/tidyverse/ggplot2/issues/2963
    # geom_vline(xintercept) --> geom_vline(aes(xintercept))
    ggplot2::geom_vline(aes(xintercept = 99.5), col = "black", 
      linetype = "dashed") +

    ggplot2::xlab("Sample Package ID") +
    ggplot2::ylab("Percent") +
    sugrrants::facet_calendar(~ as.Date(date), week_start = 7) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                  plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_y_continuous(breaks = c(0, 50, 100), limits = c(-10, 110)) +
    ggplot2::scale_x_continuous(breaks = 100) +
    ggplot2::ggtitle(paste(titleA, titleB))
}
