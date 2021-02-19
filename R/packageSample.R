#' Stratified random sample of packages.
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param cran_log Object. CRAN log.
#' @param sample.pct Numeric.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @noRd

packageSample <- function(cran_log, sample.pct = 1, multi.core = TRUE) {
  init.pkgs <- unique(cran_log$package) # remove duplicated pkgs (diff versions)
  init.pkgs <- stats::na.omit(init.pkgs)

  pkgs <- cran_log[cran_log$package %in% init.pkgs, ]
  freqtab <- table(pkgs$package)
  cores <- multiCore(multi.core)

  rank.percentile <- parallel::mclapply(names(freqtab), function(nm) {
    mean(freqtab < freqtab[nm])
  }, mc.cores = cores)

  rank.percentile <- unlist(rank.percentile)

  pct <- data.frame(pkg = names(freqtab), percentile = rank.percentile,
    stringsAsFactors = FALSE)
  pct <- pct[order(pct$percentile, decreasing = TRUE), ]
  row.names(pct) <- NULL

  # bins #

  breaks <- seq(1, 0, -0.05)

  bin.id <- lapply(2:length(breaks), function(i) {
    which(pct$percentile > breaks[i] & pct$percentile <= breaks[i - 1])
  })

  # set seed for random sampling
  set.seed(as.numeric(Sys.Date()))
  sample.id <- lapply(seq_along(bin.id), function(i) {
     sample(bin.id[[i]], round(sample.pct / 100 * length(bin.id[[i]])))
  })

  names(sample.id) <- paste(round(breaks[-1], 2))

  pct[unlist(sample.id), "pkg"]
}

#' Stratified random sample of packages for versionPlot().
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param lst Object. List of CRAN download logs data frames.
#' @param repository Character. "cran" or "archive".
#' @param strata.samples Numeric. Number of samples from each stratum.
#' @param package.samples Numeric. Number of packages to sample from across strata for use in versionPlot().
#' @param use.seed Logical. Use today's date as seed.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @note July benchmarks: cran = 61.684; archive = 35.597.
#' @noRd

packageSample2 <- function(lst, repository = "cran", strata.samples = 20,
  package.samples = 100, use.seed = TRUE, multi.core = TRUE) {

  cores <- multiCore(multi.core)
  dts <- as.Date(names(lst))
  # seq(as.Date("2020-07-01"), as.Date("2019-07-31"), by = "day")

  first <- lst[[1]]
  last <- lst[[length(lst)]]

  first.wed <- which(weekdays(dts, abbreviate = TRUE) == "Wed")[1]
  wed.pkgs <- unique(lst[[first.wed]]$package)

  # estimate for packages based on current (now) CRAN and Archive
  cran.pkgs <- cranPackages(multi.core = cores)
  all.archive <- archivePackages(multi.core = cores)
  archive.pkgs <- all.archive[!all.archive %in% cran.pkgs$package]

  wed.cran <- wed.pkgs[wed.pkgs %in% cran.pkgs$package]
  wed.not_cran <- wed.pkgs[!wed.pkgs %in% cran.pkgs$package]

  if (repository == "archive") {
    tmp <- wed.not_cran[wed.not_cran %in% archive.pkgs]
  } else if (repository == "cran") {
    tmp <- wed.cran[wed.cran %in% cran.pkgs$package]
  } else stop('"respository" must be "archive" or "cran".')

  tmp <- tmp[tmp %in% unique(first$package)]
  pkgs <- tmp[tmp %in% unique(last$package)]

  p.data <- first[first$package %in% pkgs, ]
  freqtab <- table(p.data$package)

  rank.percentile <- parallel::mclapply(names(freqtab), function(nm) {
    mean(freqtab < freqtab[nm])
  }, mc.cores = cores)

  rank.percentile <- unlist(rank.percentile)

  pct <- data.frame(pkg = names(freqtab), percentile = rank.percentile,
    stringsAsFactors = FALSE)
  pct <- pct[order(pct$percentile, decreasing = TRUE), ]
  row.names(pct) <- NULL

  # bins for stratification #
  breaks <- seq(1, 0, -0.05)

  bin.id <- lapply(2:length(breaks), function(i) {
    which(pct$percentile > breaks[i] & pct$percentile <= breaks[i - 1])
  })

  # use seed for random sampling
  if (use.seed) set.seed(as.numeric(Sys.Date()))

  # vapply(bin.id, length, integer(1L))
  sample.id <- lapply(bin.id, function(x) {
    if (length(x) == 0) NA
    else sample(x, strata.samples)
  })

  names(sample.id) <- paste(round(breaks[-1], 2))

  sel <- vapply(sample.id, function(x) all(!is.na(x)), logical(1L))
  sample.id <- sample.id[sel]

  sel <- sample(unlist(sample.id), package.samples)
  pct[sel, "pkg"]
}
