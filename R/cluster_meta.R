#' Metaclustering
#'
#' One of the core "issue" of statistical clustering is that, in many cases,
#' different methods will give different results. The **metaclustering** approach
#' proposed by *easystats* (that finds echoes in *consensus clustering*; see Monti
#' et al., 2003) consists of treating the unique clustering solutions as a ensemble,
#' from which we can derive a probability matrix. This matrix contains, for each
#' pair of observations, the probability of being in the same cluster. For instance,
#' if the 6th and the 9th row of a dataframe has been assigned to a similar cluster
#' by 5 our of 10 clustering methods, then its probability of being grouped together
#' is 0.5.
#'
#' Metaclustering is based on the hypothesis that, as each clustering algorithm
#' embodies a different prism by which it sees the data, running an infinite
#' amount of algorithms would result in the emergence of the "true" clusters.
#' As the number of algorithms and parameters is finite, the probabilistic
#' perspective is a useful proxy. This method is interesting where there is no
#' obvious reasons to prefer one over another clustering method, as well as to
#' investigate how robust some clusters are under different algorithms.
#'
#' This metaclustering probability matrix can be transformed into a dissimilarity
#' matrix (such as the one produced by `dist()`) and submitted for instance to
#' hierarchical clustering (`hclust()`). See the example below.
#'
#'
#' `predict()` extracts `n` clusters from this matrix. It runs a hierarchical
#' clustering of `1 - object` (the share of solutions that put two observations
#' in different clusters), and cuts the tree into `n` clusters. The default
#' linkage is `"average"`, the usual choice for consensus matrices (Monti et
#' al., 2003). When the matrix comes from a few solutions, it has few distinct
#' values (e.g., 0, 1/3, 2/3 and 1 with three solutions). Many distances are
#' then tied, and with `"complete"` linkage, where the tree is cut can depend
#' on the order of the observations.
#'
#' @param list_of_clusters A list of vectors with the clustering assignments from various methods.
#' @param rownames An optional vector of row.names for the matrix.
#' @param object A `cluster_meta` matrix, as returned by `cluster_meta()`.
#' @param n The number of clusters to extract.
#' @param method The linkage (agglomeration) method of the hierarchical
#'   clustering, passed to [`hclust()`]. Default is `"average"`.
#' @param ... Currently not used.
#'
#' @return A symmetric matrix containing all the pairwise (between each
#' observation) probabilities of being clustered together by the methods, i.e.,
#' the share of solutions that put the two observations in the same cluster.
#' Observations that are not assigned to a cluster (`NA` or `0`, e.g., noise
#' in DBSCAN) are ignored for that solution, so the share is computed over the
#' solutions that assign both observations. The diagonal is 1.
#'
#' `predict()` returns a vector with the cluster of each observation. The
#' tree (the [`hclust()`] object) is in its `"tree"` attribute, for instance to
#' order the rows of a heatmap.
#'
#' @references
#' Monti, S., Tamayo, P., Mesirov, J., & Golub, T. (2003). Consensus
#' clustering: A resampling-based method for class discovery and visualization
#' of gene expression microarray data. Machine Learning, 52(1), 91-118.
#'
#'
#' @examples
#' \donttest{
#' data <- iris[1:4]
#'
#' rez1 <- cluster_analysis(data, n = 2, method = "kmeans")
#' rez2 <- cluster_analysis(data, n = 3, method = "kmeans")
#' rez3 <- cluster_analysis(data, n = 6, method = "kmeans")
#'
#' list_of_clusters <- list(rez1, rez2, rez3)
#'
#' m <- cluster_meta(list_of_clusters)
#'
#' # Visualize matrix without reordering
#' heatmap(m, Rowv = NA, Colv = NA, scale = "none") # Without reordering
#' # Reordered heatmap
#' heatmap(m, scale = "none")
#'
#' # Extract 3 clusters
#' clusters <- predict(m, n = 3)
#' clusters
#'
#' # The tree of the hierarchical clustering
#' tree <- attributes(clusters)$tree
#' plot(tree, hang = -1)
#'
#' # Heatmap ordered by this tree
#' heatmap(m, Rowv = as.dendrogram(tree), symm = TRUE, scale = "none")
#' }
#' @export
cluster_meta <- function(list_of_clusters, rownames = NULL, ...) {
  x <- list()

  # Sanitize output
  for (i in seq_along(list_of_clusters)) {
    # Get name
    name <- names(list_of_clusters[i])
    if (is.null(name)) name <- paste0("Solution", i)

    solution <- list_of_clusters[[i]]
    if (inherits(solution, "cluster_analysis")) {
      if (name == paste0("Solution", i)) {
        name <- paste0(name, "_", attributes(solution)$method)
      }
      solution <- stats::predict(solution, ...)
    }
    solution[solution == "0"] <- NA
    x[[name]] <- solution
  }

  # validation check
  if (length(unique(lengths(x))) != 1) {
    insight::format_error("The clustering solutions are not of equal lengths.")
  }

  # Convert to dataframe
  cluster_data <- as.data.frame(x)
  if (!is.null(names(solution))) row.names(cluster_data) <- names(solution)
  if (!is.null(rownames)) row.names(cluster_data) <- rownames

  # Get probability matrix
  m <- .cluster_meta_matrix(cluster_data)
  class(m) <- c("cluster_meta", class(m))
  m
}


#' @keywords internal
.cluster_meta_matrix <- function(data) {
  # For each solution, a one-hot matrix of cluster membership (with a row of
  # zeros for unassigned observations) gives, by its cross-product, the pairs
  # that are in the same cluster. Summed over solutions, and divided by the
  # number of solutions that assign both observations, this gives the share of
  # solutions that put each pair together.
  n <- nrow(data)
  together <- matrix(0, nrow = n, ncol = n)
  assigned <- matrix(0, nrow = n, ncol = n)

  for (solution in data) {
    solution <- factor(solution)
    valid <- !is.na(solution)
    membership <- matrix(0, nrow = n, ncol = nlevels(solution))
    membership[cbind(which(valid), as.integer(solution)[valid])] <- 1
    together <- together + tcrossprod(membership)
    assigned <- assigned + tcrossprod(as.numeric(valid))
  }

  # pairs that no solution assigns are NaN (0 / 0)
  m <- together / assigned
  diag(m) <- 1
  dimnames(m) <- list(row.names(data), row.names(data))
  m
}


# Methods ----------------------------------------------------------------

#' @rdname cluster_meta
#' @export
predict.cluster_meta <- function(object, n = NULL, method = "average", ...) {
  if (is.null(n)) {
    insight::format_error("The number of clusters to extract `n` must be entered.")
  }
  d <- stats::as.dist(1 - unclass(object))
  tree <- stats::hclust(d, method = method)
  out <- stats::cutree(tree, k = n)
  attr(out, "tree") <- tree
  out
}
