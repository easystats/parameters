#' @title Compute cluster analysis and return group indices
#' @name cluster_analysis
#' @description Compute hierarchical or kmeans cluster analysis and return the group
#'                assignment for each observation as vector.
#'
#' @references Maechler M, Rousseeuw P, Struyf A, Hubert M, Hornik K (2014) cluster: Cluster Analysis Basics and Extensions. R package.
#'
#' @param x A data frame.
#' @param n_clusters Number of clusters used for the cluster solution. By default,
#'   the number of clusters to extract is determined by calling \code{\link{n_clusters}}.
#' @param method Method for computing the cluster analysis. By default (\code{"hclust"}), a
#'   hierarchical cluster analysis, will be computed. Use \code{"kmeans"} to
#'   compute a kmeans cluster analysis. You can specify the initial letters only.
#' @param distance Distance measure to be used when \code{method = "hclust"} (for hierarchical
#'   clustering). Must be one of \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"},
#'   \code{"canberra"}, \code{"binary"} or \code{"minkowski"}. See \code{\link{dist}}.
#'   If is \code{method = "kmeans"} this argument will be ignored.
#' @param agglomeration Agglomeration method to be used when \code{method = "hclust"} (for hierarchical
#'   clustering). This should be one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"},
#'   \code{"mcquitty"}, \code{"median"} or \code{"centroid"}. Default is \code{"ward"} (see \code{\link{hclust}}).
#'   If \code{method = "kmeans"} this argument will be ignored.
#' @param iterations Maximum number of iterations allowed. Only applies, if
#'   \code{method = "kmeans"}. See \code{\link{kmeans}} for details on this argument.
#' @param algorithm Algorithm used for calculating kmeans cluster. Only applies, if
#'   \code{method = "kmeans"}. May be one of \code{"Hartigan-Wong"} (default),
#'   \code{"Lloyd"} (used by SPSS), or \code{"MacQueen"}. See \code{\link{kmeans}}
#'   for details on this argument.
#'
#' @inheritParams equivalence_test.lm
#' @inheritParams n_clusters
#'
#' @return The group classification for each observation as vector. The
#'   returned vector includes missing values, so it has the same length
#'   as \code{nrow(x)}.
#'
#' @seealso \code{\link{n_clusters}} to determine the number of clusters to extract,
#'
#' @examples
#' # Hierarchical clustering of mtcars-dataset
#' groups <- cluster_analysis(iris[, 1:4], 3)
#'
#' # K-means clustering of mtcars-dataset, auto-detection of cluster-groups
#' \dontrun{
#' groups <- cluster_analysis(iris[, 1:4], method = "k")}
#' @importFrom stats dist na.omit hclust kmeans cutree complete.cases
#' @export
cluster_analysis <- function(x, n_clusters = NULL, method = c("hclust", "kmeans"),
                             distance = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                             agglomeration = c("ward", "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                             iterations = 20,
                             algorithm = c("Hartigan-Wong", "Lloyd", "MacQueen"),
                             package = c("NbClust", "mclust"),
                             verbose = TRUE) {
  # match arguments
  distance <- match.arg(distance)
  method <- match.arg(method)
  agglomeration <- match.arg(agglomeration)
  algorithm <- match.arg(algorithm)


  # check number of clusters
  if (is.null(n_clusters)) {
    nc <- n_clusters(x)
    ncs <- attributes(nc)$summary
    n_clusters <- ncs$n_Clusters[which.max(ncs$n_Methods)][1]
    if (verbose) {
      insight::print_color(sprintf("Using solution with %i clusters, supported by %i out of %i methods.\n", n_clusters, max(ncs$n_Methods), sum(ncs$n_Methods)), "blue")
    }
  }

  # create NA-vector of same length as data frame
  complete.groups <- rep(NA, times = nrow(x))
  # save IDs from non-missing data
  non_missing <- stats::complete.cases(x)
  x <- stats::na.omit(x)

  # Ward Hierarchical Clustering
  if (method == "hclust") {
    # check for argument and R version
    if (agglomeration == "ward") agglomeration <- "ward.D2"
    # distance matrix
    d <- stats::dist(x, method = distance)
    # hierarchical clustering, using ward
    hc <- stats::hclust(d, method = agglomeration)
    # cut tree into x clusters
    groups <- stats::cutree(hc, k = n_clusters)
  } else {
    km <- stats::kmeans(x, centers = n_clusters, iter.max = iterations, algorithm = algorithm)
    # return cluster assignment
    groups <- km$cluster
  }

  # create vector with cluster group classification,
  # including missings

  complete.groups[non_missing] <- groups
  complete.groups
}
