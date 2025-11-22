#' Annotate top axis (side = 3) with ChatGPT relase, R version, and missing.dates and add missing dates polygons.
#' @importFrom ggplot2 dup_axis scale_x_date 
#' @noRd

gg_axis.annotation_polygon <- function(dat, p, log.y, chatgpt, r.version, 
  r_date, r_v, chatgpt.release, axis.package, axis.package.version) {

  null.set <- expression(symbol("\306"))
  date.range <- range(dat$date)
  exp.dates <- seq.Date(from = date.range[1], to = date.range[2])
  obs.chatgpt <- chatgpt.release %in% exp.dates
  obs.missing <- packageRank::missing.dates %in% exp.dates

  missing.breaks <- c(mean(packageRank::missing.dates[1:2]), 
                      packageRank::missing.dates[5])

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

  if (any(obs.missing)) {
    ymin <- ifelse(log.y, 0, -Inf)

    if (all(obs.missing)) {
      p <- gg_missingDatesPolygons(p, ymin)
      brk <- c(missing.breaks, brk)
      lbl <- c(rep(null.set, 2), lbl)
    
    } else if (any(obs.missing == FALSE)) {
      missingA <- obs.missing[1:2]
      missingB <- obs.missing[3:7]
      polygon.adjustment <- list(missingA = missingA, missingB = missingB)
      p <- gg_missingDatesPolygons(p, ymin, polygon.adjustment)
          
      if (all(missingA) & all(missingB == FALSE)) {
        missing.brk <- mean(packageRank::missing.dates[1:2])
        missing.lbl <- null.set
      
      } else if (all(missingA == FALSE) & all(missingB)) {
        missing.brk <- packageRank::missing.dates[5]
        missing.lbl <- null.set
      
      } else if (all(missingA) & any(missingB == FALSE)) {
        if (all(missingB)) {
          brkB <- packageRank::missing.dates[5]
        } else {
          brkB <- mean(packageRank::missing.dates[3:7][missingB])
        }
        missing.brk <- c(mean(packageRank::missing.dates[1:2]), brkB)
        missing.lbl <- rep(null.set, 2)
      
      } else if (any(missingA == FALSE) & all(missingB)) {
        if (all(missingA)) {
          brkA <- mean(packageRank::missing.dates[1:2])
        } else {
          brkA <- packageRank::missing.dates[1:2][missingA]
        }
        missing.brk <- c(brkA, packageRank::missing.dates[5])
        missing.lbl <- rep(null.set, 2)
      
      } else if (all(missingA == FALSE) & any(missingB == FALSE)) {
        if (all(missingB)) {
          missing.brk <- packageRank::missing.dates[5]
        } else {
          missing.brk <- mean(packageRank::missing.dates[3:7][missingB])
        }
        missing.lbl <- null.set
      
      } else if (any(missingA == FALSE) & all(missingB == FALSE)) {
        if (all(missingA)) {
          missing.brk <- mean(packageRank::missing.dates[1:2])
        } else {
          missing.brk <- packageRank::missing.dates[1:2][missingA]
        }
        missing.lbl <- null.set
      
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
        missing.brk <- c(brkA, brkB)
        missing.lbl <- rep(null.set, 2)
      }

      brk <- c(missing.brk, brk)
      lbl <- c(missing.lbl, lbl)
    }
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