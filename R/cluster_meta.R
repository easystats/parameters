#' Metaclustering
#'
#' One of the core "issue" of statistical clustering is that, in many cases, different methods will give different results. The **metaclustering** approach proposed by *easystats* (that finds echoes in *consensus clustering*; see Monti et al., 2003) consists of treating the unique clustering solutions as a ensemble, from which we can derive a probability matrix. This matrix contains, for each pair of observations, the probability of being in the same cluster. For instance, if the 6th and the 9th row of a dataframe has been assigned to a similar cluster by 5 our of 10 clustering methods, then its probability of being grouped together is 0.5.
#' \cr\cr
#' Metaclustering is based on the hypothesis that, as each clustering algorithm embodies a different prism by which it sees the data, running an infinite amount of algorithms would result in the emergence of the "true" clusters. As the number of algorithms and parameters is finite, the probabilistic perspective is a useful proxy. This method is interesting where there is no obvious reasons to prefer one over another clustering method, as well as to investigate how robust some clusters are under different algorithms.
#'
#' @param list_of_clusters A list of vectors with the clustering assignments from various methods.
#' @param rownames An optional vector of row.names for the matrix.
#' @inheritParams cluster_analysis
#'
#' @return A matrix containing all the pairwise (between each observation) probabilities of being clustered together by the methods.
#'
#'
#' @examples
#' data <- iris[1:4]
#'
#' rez1 <- cluster_analysis(data, n = 2, method = "hkmeans")
#' rez2 <- cluster_analysis(data, n = 3, method = "hkmeans")
#' rez3 <- cluster_analysis(data, n = 6, method = "hkmeans")
#' rez4 <- cluster_analysis(data, method = "pamk")
#' rez5 <- cluster_analysis(data, method = "hdbscan")
#' rez6 <- cluster_analysis(data, method = "dbscan", dbscan_eps = 1.45)
#'
#' list_of_clusters <- list(rez1, rez2, rez3, rez4, rez5, rez6)
#'
#' m <- cluster_meta(list_of_clusters)
#'
#' # Visualize matrix without reordering
#' heatmap(m, Rowv = NA, Colv = NA, scale = "none") # Without reordering
#' # Reordered heatmap
#' heatmap(m, scale = "none")
#' @export
cluster_meta <- function(list_of_clusters, rownames = NULL, ...) {
  x <- list()

  # Sanitize output
  for (i in 1:length(list_of_clusters)) {
    # Get name
    name <- names(list_of_clusters[i])
    if (is.null(name)) name <- paste0("Solution", i)

    solution <- list_of_clusters[[i]]
    if (inherits(solution, "cluster_analysis")) {
      if (name == paste0("Solution", i)) {
        name <- paste0(name, "_", attributes(solution)$method)
      }
      solution <- predict(solution, ...)
    }
    solution[solution == "0"] <- NA
    x[[name]] <- solution
  }

  # Sanity check
  if (length(unique(sapply(x, length))) != 1) {
    stop("The clustering solutions are not of equal lengths.")
  }

  # Convert to dataframe
  data <- as.data.frame(x)
  if (!is.null(names(solution))) row.names(data) <- names(solution)
  if (!is.null(rownames)) row.names(data) <- rownames

  # Get probability matrix
  .cluster_meta_matrix(data)
}








#' @keywords internal
.cluster_meta_matrix <- function(data) {

  # Internal function
  .get_prob <- function(x) {
    if (any(is.na(x))) {
      NA
    } else {
      ifelse(length(unique(x[!is.na(x)])) == 1, 0, 1)
    }
  }

  # Initialize matrix
  m <- matrix(data = NA, nrow = nrow(data), ncol = nrow(data), dimnames = list(rev(row.names(data)), row.names(data)))

  for (row in row.names(m)) {
    for (col in colnames(m)) {
      if (row == col) {
        m[row, col] <- 0
        next
      }
      subset <- data[row.names(data) %in% c(row, col), ]
      rez <- sapply(subset[2:ncol(subset)], .get_prob)
      m[row, col] <- sum(rez, na.rm = TRUE) / length(stats::na.omit(rez))
    }
  }

  m
}
