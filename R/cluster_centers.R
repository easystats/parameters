#' Find the cluster centers in your data
#'
#' For each cluster, computes the mean (or other indices) of the variables. Can be used
#' to retrieve the centers of clusters. Also returns the within Sum of Squares.
#'
#' @param data A data.frame.
#' @param clusters A vector with clusters assignments (must be same length as rows in data).
#' @param fun What function to use, \code{mean} by default.
#' @param ... Other arguments to be passed to or from other functions.
#'
#' @return A dataframe containing the cluster centers. Attributes include performance statistics and distance between each observation and its respective cluster centre.
#'
#'
#' @examples
#' k <- kmeans(iris[1:4], 3)
#' cluster_centers(iris[1:4], clusters = k$cluster)
#' cluster_centers(iris[1:4], clusters = k$cluster, fun = median)
#' @export
cluster_centers <- function(data, clusters, fun = mean, ...) {

  # Get n obs
  params <- data.frame(table(clusters))
  names(params) <- c("Cluster", "n_Obs")

  # Get Within clusters sum of squares (WCSS)
  ss <- .cluster_centers_SS(data, clusters)
  params$Sum_Squares <- ss$WSS

  # Get Cluster Centers
  centers <- stats::aggregate(data, list(Cluster=clusters), fun)
  params <- merge(params, centers, by = "Cluster")

  # Get distance of observations from cluster

  # Add attributes
  attr(params, "Sum_Squares_Total") <- ss$TSS
  attr(params, "Sum_Squares_Between") <- ss$BSS
  attr(params, "variance") <- ss$BSS / ss$TSS
  attr(params, "scale") <- sapply(data, stats::sd)
  attr(params, "distance") <- .cluster_centers_distance(data, clusters, centers, attributes(params)$scale)

  params
}


# Performance -------------------------------------------------------------

#' @keywords internal
.cluster_centers_params <- function(data, clusters, ...) {
  # This function actually wraps *around* the exported cluster_centers()
  # to be used within the different model_parameters() functions for clusters

  params <- cluster_centers(data = data, clusters = clusters, ...)

  # Long means
  means <- datawizard::reshape_longer(params,
                                      cols = 4:ncol(params),
                                      values_to = "Mean",
                                      names_to = "Variable")

  attr(params, "variance") <- attributes(params)$variance
  attr(params, "Sum_Squares_Between") <- attributes(params)$Sum_Squares_Between
  attr(params, "Sum_Squares_Total") <- attributes(params)$Sum_Squares_Total
  attr(params, "scale") <- attributes(params)$scale
  attr(params, "distance") <- attributes(params)$distance
  attr(params, "scores") <- attributes(params)$scores
  attr(params, "means") <- means
  class(params) <- c("parameters_clusters", class(params))

  params
}


# Distance ----------------------------------------------------------------

#' @keywords internal
.cluster_centers_distance <- function(data, clusters, centers, scale) {

  dis <- c()
  for(c in unique(clusters)) {
    center <- centers[centers$Cluster == c, ]
    center$Cluster <- NULL # Remove column

    d <- apply(data[clusters == c, ], 1, function(x) {
      z <- x - center[names(data)]
      z <- z / scale
      sqrt(sum((z) ^ 2))
    })
    dis <- c(dis, d)
  }
  dis
}


# Performance -------------------------------------------------------------

#' @keywords internal
.cluster_centers_SS <- function(data, clusters) {
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
