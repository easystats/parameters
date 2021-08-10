#' Find the cluster centers in your data
#'
#' For each cluster, computes the mean (or other indices) of the variables. Can be used
#' to retrieve the centers of clusters. Also returns the within Sum of Squares.
#'
#' @param data A data.frame.
#' @param clusters A vector with clusters assignments (must be same length as rows in data).
#' @param fun What function to use, \code{mean} by default.
#'
#' @examples
#' k <- kmeans(iris[1:4], 3)
#' cluster_centers(iris[1:4], clusters = k$cluster)
#' cluster_centers(iris[1:4], clusters = k$cluster, fun = median)
#' @importFrom stats aggregate
#' @export
cluster_centers <- function(data, clusters, fun = mean) {

  # Get n obs
  params <- data.frame(table(clusters))
  names(params) <- c("Cluster", "n_Obs")

  # Get Within clusters sum of squares (WCSS)
  ss <- .cluster_analysis_SS(data, clusters)
  params$Sum_Squares <- ss$WSS

  # Get Cluster Centers
  centers <- aggregate(data, list(Cluster=clusters), fun)
  params <- merge(params, centers, by = "Cluster")

  attr(params, "Sum_Squares_Total") <- ss$TSS
  attr(params, "Sum_Squares_Between") <- ss$BSS
  attr(params, "variance") <- ss$BSS / ss$TSS

  params
}



# Performance -------------------------------------------------------------

#' @importFrom stats dist
#' @keywords internal
.cluster_analysis_SS <- function(data, clusters) {
  # https://stackoverflow.com/questions/68714612/compute-between-clusters-sum-of-squares-bcss-and-total-sum-of-squares-manually
  # total sum of squares
  TSS <- sum(scale(data, scale=FALSE)^2)
  # Within clusters sum of squares (WCSS)
  WSS <- sapply(split(data, clusters), function(x) sum(scale(x, scale=FALSE)^2))
  # Between clsuters sum of squares
  BSS <- TSS - sum(WSS)

  # Compute BSS directly (without TSS to double check)
  gmeans <- sapply(split(data, clusters), colMeans)
  means <- colMeans(data)
  BSS2 <- sum(colSums((gmeans - means)^2) * table(clusters))

  # Double check
  if(BSS2 - BSS > 1e-05) stop("The between sum of squares computation went wrong. Please open an issue at https://github.com/easystats/parameters/issues so we can fix the bug (provide an example and mention that 'BSS != BSS2').")

  list(WSS = WSS, BSS = BSS, TSS = TSS)
}
