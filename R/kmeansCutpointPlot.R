#' Plot k-means cutpoint.
#'
#' @param lst Object. List of download log dataframes.
#' @param i Numeric. Day/ID.
#' @param centers Numeric. Number of k's for k-means clustering.
#' @param nstart Numeric. Number of random sets.
#' @export

kmeansCutpointPlot <- function(lst, i = 1, centers = 2L, nstart = 25L) {
  cran_log <- cleanLog(lst[[i]])
  cran_log <- smallFilter0(cran_log)

  freqtab <- tapply(cran_log$package, cran_log$ip_id, function(x) {
    length(unique(x))
  })

  d2 <- unique(freqtab)
  km <- stats::kmeans(stats::dist(d2), centers = centers, nstart = nstart)
  out <- data.frame(ct = d2, group = km$cluster)

  group.one <- mean(out[out$group == 1, "ct"])
  group.two <- mean(out[out$group == 2, "ct"])
  LR <- which.min(c(group.one, group.two))

  if (LR == 1) {
    cutpoint <- sum(min(out[out$group == 2, "ct"]),
                    max(out[out$group == 1, "ct"])) / 2
  } else {
    cutpoint <- sum(min(out[out$group == 1, "ct"]),
                    max(out[out$group == 2, "ct"])) / 2
  }

  midpt2 <- (cutpoint + max(out$ct)) / 2

  plot(d2, rep(1, length(d2)), yaxt = "n", xlab = "Unique Packages Downloaded",
    ylab = NA, pch = NA)
  abline(v = cutpoint, col = "red")
  axis(3, at = cutpoint, labels = format(cutpoint, big.mark = ","), col = "red",
    col.axis = "red", padj = 0.9)

  if (LR == 1) {
    points(out[out$group == 1, "ct"], rep(1, sum(out$group == 1)))
    points(out[out$group == 2, "ct"], rep(1, sum(out$group == 2)), col = "red")
    text(cutpoint / 2, 0.65, labels = paste("N =", sum(out$group == 1)))
    text(midpt2, 0.65, labels = paste("N = ", sum(out$group == 2)), col = "red")
    min.outlier <- min(out[out$group == 2, "ct"])
  } else {
    points(out[out$group == 2, "ct"], rep(1, sum(out$group == 2)))
    points(out[out$group == 1, "ct"], rep(1, sum(out$group == 1)), col = "red")
    text(cutpoint / 2, 0.65, labels = paste("N =", sum(out$group == 2)))
    text(midpt2, 0.65, labels = paste("N =", sum(out$group == 1)), col = "red")
    min.outlier <- min(out[out$group == 1, "ct"])
  }

  text(min.outlier, 1, labels = format(min.outlier, big.mark = ","), pos = 3,
    cex = 0.75, col = "red")
  date <- as.Date(names(lst[i]))
  wday <- weekdays(date)
  title(main = paste(wday, date))
}
