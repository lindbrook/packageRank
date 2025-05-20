#' Download plot with annual and weekly data.
#'
#' @param package Character Package name, "R" (for R application) or NULL (for total CRAN package downloads). 
#' @param from  Numeric or Integer Year.
#' @param to Numeric or Integer Year.
#' @param check.package Logical. Validate and "spell check" package.
#' @export
#' @note Adapted from Vesuvius plot at https://github.com/nrennie/tidytuesday/blob/main/2025/2025-05-13/20250513.R

annualPlot <- function(package = "packageRank", from = 2019, to = 2024, 
  check.package = TRUE) {

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)

  ttl <- "Downloads (Year and Week)"

  if (!is.null(package)) {
    if (!"R" %in% package) {
      if (check.package) {
        package <- checkPackage(package)
        title <- paste0("'", package, "' ", ttl)
      }
    } else title <- paste("R", ttl)
  } else title <- paste("CRAN Package", ttl)

  pkg.daily <- cranDownloads(package, from = from, to = to, pro.mode = TRUE)

  tmp <- pkg.daily$cranlogs.data
  tmp$week <- as.numeric(format(tmp$date, format = "%V"))
  tmp$year <- as.numeric(format(tmp$date, format = "%Y"))

  week <- tapply(tmp$count, paste0(tmp$year, "-", tmp$week), sum)
  annual <- tapply(tmp$count, tmp$year, sum)

  week.med <- tapply(tmp$count, paste0(tmp$year, "-", tmp$week), stats::median)
  annual.med <- tapply(tmp$count, tmp$year, stats::median)

  yr <- vapply(strsplit(names(week), "-"), function(x) {
    as.integer(x[1])
  }, integer(1L))

  yr.wk <- do.call(rbind, strsplit(names(week), "-"))
  yr.wk <- as.data.frame(yr.wk)
  yr.wk <- data.frame(year = as.numeric(yr.wk$V1), week = as.numeric(yr.wk$V2),
    count = week)
  yr.wk <- yr.wk[order(yr.wk$year, yr.wk$week), ]

  tmp.wk <- unique(tmp[, c("year", "week")])
  wk <- data.frame(tmp.wk, tot = week, med = week.med, row.names = NULL)

  yr <- data.frame(year = as.numeric(names(annual)), tot = annual, 
    med = annual.med, row.names = NULL)
  
  g_bottom <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = wk, 
      mapping = ggplot2::aes(x = "1", y = .data$week, fill = .data$med)) +
    ggplot2::annotate("rect", 
             xmin = 0.5, 
             xmax = 1.5, 
             ymin = 0.5, 
             ymax = 53.5,
             fill = "transparent", 
             linewidth = 0.5) +
    ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
    ggplot2::scale_y_reverse(breaks = c(1, 14, 27, 40),
                             labels = c("Jan", "Apr", "Jul", "Oct")) +
    ggplot2::facet_wrap(ggplot2::vars(.data$year), nrow = 1, 
      strip.position = "top") +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 5), 
        size = ggplot2::rel(0.8)),
      legend.position = "none",
      strip.text = ggplot2::element_text(margin = ggplot2::margin(b = 5)),
      strip.background = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank())

  g_top <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = yr,
      mapping = ggplot2::aes(x = "1", y = "1", fill = .data$med),
      linewidth = 0.5) +
    ggplot2::scale_y_discrete(breaks = c("1"), labels = c("Jan")) +
    ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
    ggplot2::facet_wrap(ggplot2::vars(.data$year), nrow = 1, 
      strip.position = "top") +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(colour = "white",
        margin = ggplot2::margin(r = 5), size = ggplot2::rel(0.8)),
      strip.text = ggplot2::element_blank(),
      legend.position = "none",
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank())

  patchwork::wrap_plots(g_top, g_bottom) +
    patchwork::plot_layout(ncol = 1, heights = c(1, 5)) +
    patchwork::plot_annotation(title = title) &
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.margin = ggplot2::margin(5, 5, 5, 5))
}
