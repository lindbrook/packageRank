#' Annotate top axis (side = 3) with ChatGPT release and R version.
#' @importFrom ggplot2 dup_axis scale_x_date 
#' @noRd

gg_axis.annotation_polygon <- function(dat, p, log.y, chatgpt, r.version, 
  r_date, r_v, chatgpt.release, axis.package, axis.package.version) {

  date.range <- range(dat$date)
  exp.dates <- seq.Date(from = date.range[1], to = date.range[2])
  obs.chatgpt <- chatgpt.release %in% exp.dates

  if (isTRUE(r.version) | isTRUE(r.version == "line")) {
    sel <- r_date %in% exp.dates
    if (any(sel)) {
      r_v <- r_v[sel]
      r_date <- r_date[sel]
      obs.r <- TRUE
    }
  } else obs.r <- FALSE

  axis.pkg.test <- is.character(axis.package) & length(axis.package) == 1

  if (isFALSE(axis.pkg.test)) {
    p_date <- NULL
    p_v <- NULL
    obs.p <- FALSE
  } else {
    pkgsearch.data <- pkgsearch::cran_package_history(axis.package)
    p_date <- as.Date(pkgsearch.data$`crandb_file_date`)
    sel <- p_date %in% exp.dates
    if (any(sel)) {
      p_date <- p_date[sel]
      p_v <- pkgsearch.data$Version[sel]
      obs.p <- TRUE   
    }
  }

  if ((isTRUE(chatgpt) | isTRUE(chatgpt == "line")) &
      (isTRUE(r.version) | isTRUE(r.version == "line")) &
      (axis.pkg.test | isTRUE(axis.package.version == "line"))) {
  
    brk <- c(chatgpt.release, r_date, p_date)
    lbl <- c("ChatGPT", r_v, p_v)
  
  } else if ((isFALSE(chatgpt) | isFALSE(chatgpt == "line")) &
             (isTRUE(r.version) | isTRUE(r.version == "line")) &
             (axis.pkg.test | isTRUE(axis.package.version == "line"))) {
    
    brk <- c(r_date, p_date)
    lbl <- c(r_v, p_v)
  
  } else if ((isTRUE(chatgpt) | isTRUE(chatgpt == "line")) &
             (isFALSE(r.version) | isFALSE(r.version == "line")) &
             (axis.pkg.test | isTRUE(axis.package.version == "line"))) {
    
    brk <- c(chatgpt.release, p_date)
    lbl <- c("ChatGPT", p_v)
  
  } else if ((isFALSE(chatgpt) | isFALSE(chatgpt == "line")) &
             (isFALSE(r.version) | isFALSE(r.version == "line")) &
             (axis.pkg.test | isTRUE(axis.package.version == "line"))) {
    
    brk <- p_date
    lbl <- p_v
  
  } else if ((isTRUE(chatgpt) | isTRUE(chatgpt == "line")) &
             (isTRUE(r.version) | isTRUE(r.version == "line")) &
             (axis.pkg.test | isFALSE(axis.package.version == "line"))) {

    brk <- c(chatgpt.release, r_date)
    lbl <- c("ChatGPT", r_v)

  } else if ((isFALSE(chatgpt) | isFALSE(chatgpt == "line")) &
             (isTRUE(r.version) | isTRUE(r.version == "line")) &
             (axis.pkg.test | isFALSE(axis.package.version == "line"))) {

    brk <- r_date
    lbl <- r_v

  } else if ((isTRUE(chatgpt) | isTRUE(chatgpt == "line")) &
             (isFALSE(r.version) | isFALSE(r.version == "line")) &
             (axis.pkg.test | isFALSE(axis.package.version == "line"))) {

    brk <- chatgpt.release
    lbl <- "ChatGPT"

  } else if ((isFALSE(chatgpt) | isFALSE(chatgpt == "line")) &
             (isFALSE(r.version) | isFALSE(r.version == "line")) &
             (axis.pkg.test | isFALSE(axis.package.version == "line"))) {
  
    brk <- NULL
    lbl <- NULL
  }

  p <- p + ggplot2::scale_x_date(sec.axis =
             ggplot2::dup_axis(name = NULL, breaks = brk, labels = lbl))

  if (obs.chatgpt & chatgpt == "line") {
    p <- p + ggplot2::geom_vline(xintercept = chatgpt.release, 
      colour = "blue", linetype = "solid", linewidth = 0.125)
  }

  if (obs.p & axis.package.version == "line") {
    p <- p + ggplot2::geom_vline(xintercept = p_date, 
      colour = "red", linetype = "solid", linewidth = 0.125)
  }

  if (obs.r & r.version == "line") {
    p <- p + ggplot2::geom_vline(xintercept = r_date, 
      colour = "gray", linetype = "solid", linewidth = 0.125)
  }

  p
}