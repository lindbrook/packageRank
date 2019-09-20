#' Package dependencies graph.
#'
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores.
#' @note Uses 'igraph' package.
#' @export

cranDependenciesNetwork <- function(multi.core = TRUE) {
  cores <- multiCore(multi.core)
  edges <- parallel::mclapply(packageRank::cran.dependencies, function(x) {
    df <- expand.grid(x[1], x[-1], stringsAsFactors = FALSE)
    stats::setNames(df, c("node1", "node2"))
  }, mc.cores = cores)
  edge.list <- do.call(rbind, edges)
  igraph::graph_from_data_frame(edge.list, directed = TRUE)
}

# plot(cranDependenciesNetwork(), vertex.label = NA, vertex.size = 2)