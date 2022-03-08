#' Filter out small downloads (prototype).
#'
#' @param dat Object. Package log entries.
#' @param threshold Numeric. Bytes.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. Mac and Unix only.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @export

smallFilter <- function(dat, threshold = 1000L, multi.core = TRUE,
  dev.mode = dev.mode) {

  cores <- multiCore(multi.core)
  # win.exception <- .Platform$OS.type == "windows" & cores > 1

  # if (dev.mode | win.exception) {
  if (dev.mode) {
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(),
      varlist = "threshold")
    out <- parallel::parLapply(cl, dat, function(x) {
      x[x$size >= threshold, ]
    })
    parallel::stopCluster(cl)
  } else {
    if (.Platform$OS.type == "windows") cores <- 1L    
    out <- parallel::mclapply(dat, function(x) {
      x[x$size >= threshold, ]
    }, mc.cores = cores)
  }
  out
}
