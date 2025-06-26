#' Download plot with annual and weekly data.
#'
#' @param package Character Package name, "R" (for R application) or NULL (for total CRAN package downloads). 
#' @param from  Numeric or Integer Year. "yyyy".
#' @param to Numeric or Integer. Year. "yyyy"; NULL uses current year.
#' @param check.package Logical. Validate and "spell check" package.
#' @param pro.mode Logical.
#' @param sunday.week Logical. TRUE: week starts on Sunday. FALSE: week starts on Monday.
#' @export
#' @note Adapted from Vesuvius plot at https://github.com/nrennie/tidytuesday/blob/main/2025/2025-05-13/20250513.R

annualPlot <- function(package = "packageRank", from = 2019, to = NULL,
  check.package = TRUE, pro.mode = FALSE, sunday.week = TRUE) {

  if (is.null(to)) to <- as.numeric(format(Sys.Date(), "%Y"))
  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)

  ttl <- "Downloads (Year and Week)"

  if (!is.null(package)) {
    if (!"R" %in% package) {
      if (check.package) package <- checkPackage(package)
      title <- paste0("'", package, "' ", ttl)
    } else title <- paste("R", ttl)
  } else title <- paste("CRAN Package", ttl)

  if (pro.mode) {
    pkg.daily <- cranDownloads(package, from = from, to = to, pro.mode = TRUE)
  } else {
    pkg.daily <- cranDownloads(package, from = from, to = to)
  }

  dat <- pkg.daily$cranlogs.data
  dat$cumulative <- NULL
  dat$year <- as.numeric(format(dat$date, format = "%Y"))
  
  if (sunday.week) {
    dat$week <- as.numeric(format(dat$date, format = "%U"))  
  } else {
    dat$week <- as.numeric(format(dat$date, format = "%W"))  
  }
  
  obs.yr <- unique(dat$year)
  
  overflow <- lapply(obs.yr, function(yr) dat[dat$year == yr & dat$week == 0, ])
  data.ok <- lapply(obs.yr, function(yr) dat[dat$year == yr & dat$week != 0, ])
  
  names(overflow) <- obs.yr
  names(data.ok) <- obs.yr
  
  fix.yr <- obs.yr[-length(obs.yr)]
  
  overflow.data <- lapply(fix.yr, function(yr) {
    tmp <- overflow[[paste(yr + 1)]]
    if (nrow(tmp) > 0) {
      tmp$year <- tmp$year - 1
      tmp$week <- 52L
      rbind(data.ok[[paste(yr)]], tmp)   
    } else data.ok[[paste(yr)]]
  })
    
  data.ok <- c(overflow.data, data.ok[length(data.ok)])
  names(data.ok) <- obs.yr
  
  tmp <- do.call(rbind, data.ok)
  
  week <- tapply(tmp$count, paste0(tmp$year, "-", tmp$week), sum)
  annual <- tapply(tmp$count, tmp$year, sum)

  week.med <- tapply(tmp$count, paste0(tmp$year, "-", tmp$week), stats::median)
  annual.med <- tapply(tmp$count, tmp$year, stats::median)

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
    ggplot2::scale_y_reverse(breaks = seq(from = 1, to = 52, by = 13),
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
