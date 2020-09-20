#' Stratified random sample of packages
#'
#' Logs from RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param cran_log Object. CRAN log.
#' @param sample.pct Numeric.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @export

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
