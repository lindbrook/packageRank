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

  ggplot(data = pkg.data, aes_string(x = "pkg.id", y = "pct.obs.exp")) +
    geom_line(col = grDevices::adjustcolor("red", alpha.f = 0.5), size = 0.5) +

    # https://github.com/tidyverse/ggplot2/issues/2963
    # geom_vline(xintercept) --> geom_vline(aes(xintercept))
    geom_vline(aes(xintercept = 99.5), col = "black", linetype = "dashed") +

    xlab("Sample Package ID") +
    ylab("Percent") +
    sugrrants::facet_calendar(~ as.Date(date), week_start = 7) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(breaks = c(0, 50, 100), limits = c(-10, 110)) +
    scale_x_continuous(breaks = 100) +
    ggtitle(paste(titleA, titleB))
}
