#' Annotate top axis (side = 3) with ChatGPT relase, R version, and missing.dates and add missing dates polygons.
#' @noRd

gg_axis.annotation_polygon <- function(dat, p, log.y, chatgpt, r.version,
  chatgpt.release) {

  null.set <- expression(symbol("\306"))
  date.range <- range(dat$date)
  exp.dates <- seq.Date(from = date.range[1], to = date.range[2])
  obs.chatgpt <- chatgpt.release %in% exp.dates
  obs.missing <- packageRank::missing.dates %in% exp.dates

  if (isTRUE(r.version) | isTRUE(r.version == "line")) {  
    rvers.data <- rversions::r_versions()
    r_date <- as.Date(rvers.data$date)
    r_v <- rvers.data$version
    sel <- r_date %in% exp.dates
    obs.r <- any(sel)
    r_v <- if (obs.r) paste("R", r_v[sel])
    r_date <- r_date[sel]
  } else if (isFALSE(r.version) | isFALSE(r.version == "line")) {
    r_v <- NULL
    r_date <- NULL
    obs.r <- FALSE
  }
  
  if (all(obs.missing)) {
    # https://stackoverflow.com/questions/70370189/annotating-a-rectangle-in-r-with-ggplot2-for-a-graph-in-log-scale
    ymin <- ifelse(log.y, 0, -Inf)
    
    p <- gg_missingDatesPolygons(p, ymin)

    if ((isTRUE(chatgpt) | isTRUE(chatgpt == "line")) &
        (isTRUE(r.version) | isTRUE(r.version == "line"))) {
      brk <- c(chatgpt.release,
               mean(packageRank::missing.dates[1:2]),
               packageRank::missing.dates[5],
               r_date)
      lbl <- c("ChatGPT", rep(null.set, 2), r_v)
    } else if ((isTRUE(chatgpt) | isTRUE(chatgpt == "line")) &
               (isFALSE(r.version) | isFALSE(r.version == "line"))) {
      brk <- c(chatgpt.release,
               mean(packageRank::missing.dates[1:2]),
               packageRank::missing.dates[5])
      lbl <- c("ChatGPT", rep(null.set, 2))
    } else if ((isFALSE(chatgpt) | isFALSE(chatgpt == "line")) &
               (isTRUE(r.version) | isTRUE(r.version == "line"))) {
      brk <- c(mean(packageRank::missing.dates[1:2]),
               packageRank::missing.dates[5],
               r_date)
      lbl <- c(rep(null.set, 2), r_v)
    } else if ((isFALSE(chatgpt) | isFALSE(chatgpt == "line")) |
               (isFALSE(r.version) | isFALSE(r.version == "line"))) {
      brk <- c(mean(packageRank::missing.dates[1:2]),
               packageRank::missing.dates[5])
      lbl <- rep(null.set, 2)
    }
    
  } else if (all(obs.missing == FALSE)) {
    if ((isTRUE(chatgpt) | isTRUE(chatgpt == "line")) &
        (isTRUE(r.version) | isTRUE(r.version == "line"))) {
      brk <- c(chatgpt.release, r_date)
      lbl <- c("ChatGPT", r_v)
    } else if ((isTRUE(chatgpt) | isTRUE(chatgpt == "line")) &
               (isFALSE(r.version) | isFALSE(r.version == "line"))) {
      brk <- chatgpt.release
      lbl <- "ChatGPT"
    } else if ((isFALSE(chatgpt) | isFALSE(chatgpt == "line")) & 
               (isTRUE(r.version) | isTRUE(r.version == "line"))) {
      brk <- r_date
      lbl <- r_v
    }
  
  } else if (any(obs.missing == FALSE)) {
    missingA <- obs.missing[1:2]
    missingB <- obs.missing[3:7]
    polygon.adjustment <- list(missingA = missingA, missingB = missingB)
    
    ymin <- ifelse(log.y, 0, -Inf)
    p <- gg_missingDatesPolygons(p, ymin, polygon.adjustment)
        
    if (all(missingA) & all(missingB == FALSE)) {
      brk <- mean(packageRank::missing.dates[1:2])
      lbl <- null.set
    } else if (all(missingA == FALSE) & all(missingB)) {
      brk <- packageRank::missing.dates[5]
      lbl <- null.set
    } else if (all(missingA) & any(missingB == FALSE)) {
      if (all(missingB)) {
        brkB <- packageRank::missing.dates[5]
      } else {
        brkB <- mean(packageRank::missing.dates[3:7][missingB])
      }
      brk <- c(mean(packageRank::missing.dates[1:2]), brkB)
      lbl <- rep(null.set, 2)
    } else if (any(missingA == FALSE) & all(missingB)) {
      if (all(missingA)) {
        brkA <- mean(packageRank::missing.dates[1:2])
      } else {
        brkA <- packageRank::missing.dates[1:2][missingA]
      }
      brk <- c(brkA, packageRank::missing.dates[5])
      lbl <- rep(null.set, 2)
    } else if (all(missingA == FALSE) & any(missingB == FALSE)) {
      if (all(missingB)) {
        brk <- packageRank::missing.dates[5]
      } else {
        brk <- mean(packageRank::missing.dates[3:7][missingB])
      }
      lbl <- null.set
    } else if (any(missingA == FALSE) & all(missingB == FALSE)) {
      if (all(missingA)) {
        brk <- mean(packageRank::missing.dates[1:2])
      } else {
        brk <- packageRank::missing.dates[1:2][missingA]
      }
      lbl <- null.set
    } else if (any(missingA == FALSE) & any(missingB == FALSE)) {
      if (all(missingA)) {
        brkA <- mean(packageRank::missing.dates[1:2])
      } else {
        brkA <- packageRank::missing.dates[1:2][missingA]
      }
      if (all(missingB)) {
        brkB <- packageRank::missing.dates[5]
      } else {
        brkB <- mean(packageRank::missing.dates[3:7][missingB])
      }
      brk <- c(brkA, brkB)
      lbl <- rep(null.set, 2)
    }
  }

  p <- p + ggplot2::scale_x_date(sec.axis =
             ggplot2::dup_axis(name = NULL, breaks = brk, labels = lbl))

  if (obs.chatgpt & chatgpt == "line") {
    p <- p + ggplot2::geom_vline(xintercept = chatgpt.release, 
      colour = "blue", linetype = "solid", linewidth = 0.125)
  }

  if (obs.r & r.version == "line") {
    p <- p + ggplot2::geom_vline(xintercept = r_date, 
      colour = "gray", linetype = "solid", linewidth = 0.125)
  }

  p
}
