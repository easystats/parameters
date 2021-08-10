#' Cluster Analysis
#'
#' Compute hierarchical or kmeans cluster analysis and return the group
#' assignment for each observation as vector.
#'
#' @references
#' - Maechler M, Rousseeuw P, Struyf A, Hubert M, Hornik K (2014) cluster: Cluster
#' Analysis Basics and Extensions. R package.
#'
#' @param x A data frame.
#' @param n Number of clusters used for supervised cluster methods. If \code{NULL},
#' the number of clusters to extract is determined by calling [n_clusters()]. Note
#' that this argument does not apply for unsupervised clustering methods like
#' DBSCAN.
#' @param method Method for computing the cluster analysis. By default
#'   (`"hclust"`), a hierarchical cluster analysis, will be computed. Use
#'   `"kmeans"` to compute a kmeans cluster analysis. You can specify the
#'   initial letters only.
#' @param distance_method Distance measure to be used when `method = "hclust"` (for
#'   hierarchical clustering). Must be one of `"euclidean"`, `"maximum"`,
#'   `"manhattan"`, `"canberra"`, `"binary"` or `"minkowski"`. See [dist()]. If
#'   is `method = "kmeans"` this argument will be ignored.
#' @param hclust_method Agglomeration method to be used when `method = "hclust"`
#'   (for hierarchical clustering). This should be one of `"ward"`, `"single"`,
#'   `"complete"`, `"average"`, `"mcquitty"`, `"median"` or `"centroid"`.
#'   Default is `"ward"` (see [hclust()]). If `method = "kmeans"` this argument
#'   will be ignored.
#' @param kmeans_method Algorithm used for calculating kmeans cluster. Only applies,
#'   if `method = "kmeans"`. May be one of `"Hartigan-Wong"` (default),
#'   `"Lloyd"` (used by SPSS), or `"MacQueen"`. See [kmeans()] for details on
#'   this argument.
#'
#' @inheritParams equivalence_test.lm
#' @inheritParams n_clusters
#'
#' @return The group classification for each observation as vector. The
#'   returned vector includes missing values, so it has the same length
#'   as `nrow(x)`.
#'
#' @note
#' There is also a [`plot()`-method](https://easystats.github.io/see/articles/parameters.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details
#' The `print()` and `plot()` methods show the (standardized) mean value for
#' each variable within each cluster. Thus, a higher absolute value indicates
#' that a certain variable characteristic is more pronounced within that
#' specific cluster (as compared to other cluster groups with lower absolute
#' mean values).
#'
#' @seealso
#' [n_clusters()] to determine the number of clusters to extract,
#' [cluster_discrimination()] to determine the accuracy of cluster group
#' classification and [check_clusterstructure()] to check suitability of data
#' for clustering.
#'
#' @examples
#' # K-Means ====================================================
#' rez <- cluster_analysis(iris[, 1:4], n = 3, method = "kmeans")
#' rez  # Show results
#' predict(rez)  # Get clusters
#' plot(rez)
#'
#'
#' # Hierarchical Clustering (hclust) ===========================
#' rez <- cluster_analysis(iris[, 1:4], n = 3, method = "hclust")
#' rez  # Show results
#' predict(rez)  # Get clusters
#' plot(rez)
#'
#' @export
cluster_analysis <- function(x,
                             n = NULL,
                             method = "kmeans",
                             include_factors = FALSE,
                             standardize = TRUE,
                             verbose = TRUE,
                             distance_method = "euclidean",
                             hclust_method = "complete",
                             kmeans_method = "Hartigan-Wong",
                             ...) {


  # Sanity checks -----------------------------------------------------------
  insight::check_if_installed("performance")

  # match arguments
  method <- match.arg(method, choices = c("kmeans", "hclust", "dbscan"), several.ok = TRUE)

  # Preparation -------------------------------------------------------------

  # Preprocess data
  data <- .prepare_data_clustering(x, include_factors = include_factors, standardize = standardize, ...)

  # Get number of clusters
  if (is.null(n) && any(method %in% c("kmeans"))) {
    n <- tryCatch(
      {
        nc <- n_clusters(data, preprocess = FALSE, ...)
        n <- attributes(nc)$n
        if (verbose) {
          insight::print_color(sprintf("Using solution with %i clusters, supported by %i out of %i methods.\n", n, max(summary(nc)$n_Methods), sum(summary(nc)$n_Methods)), "blue")
        }
        n
      },
      error = function(e) {
        if (isTRUE(verbose)) {
          stop(insight::format_message("Could not extract number of cluster. Please provide argument 'n'."), call. = FALSE)
        }
        2
      }
    )
  }



  # Apply clustering --------------------------------------------------------


  if (any(method == "kmeans")) {
    rez <- .cluster_analysis_kmeans(data, n = n, ...)
  } else if(any(method %in% c("hclust"))) {
    rez <- .cluster_analysis_hclust(data, n = n, distance_method = distance_method, ...)
  } else if(any(method == "dbscan")) {
    rez <- .cluster_analysis_dbscan(dist = dist, n = n, ...)
  }

  # Assign clusters to observations
  # Create NA-vector of same length as original data frame
  clusters <- rep(NA, times = nrow(x))
  # Create vector with cluster group classification (with missing)
  complete_cases <- stats::complete.cases(x[names(data)])
  clusters[complete_cases] <- rez$clusters

  # Get clustering parameters
  out <- model_parameters(rez$model, data = data, clusters = clusters)
  performance <-  cluster_performance(out)

  attr(out, "method") <- method
  attr(out, "clusters") <- clusters
  attr(out, "data") <- data
  attr(out, "performance") <- performance

  class(out) <- c("cluster_analysis", class(out))
  out

}



# Clustering Methods --------------------------------------------------------

#' @keywords internal
.cluster_analysis_kmeans <- function(data, n = 2, ...) {
  model <- stats::kmeans(data, centers = n, ...)
  list(model = model, clusters = model$cluster)
}

#' @keywords internal
.cluster_analysis_hclust <- function(data, n = 2, distance_method = "euclidean", hclust_method = "complete", ...) {
  if(is.null(n)) {
    rez <- n_clusters_hclust(data, preprocess = FALSE, distance_method = distance_method, ...)
    out <- list(model = attributes(rez)$fit$clust, clusters = rez$Cluster)
  } else {
    dist <- dist(data, method = distance_method, ...)
    model <- stats::hclust(dist, method = hclust_method, ...)
    out <- list(model = model, clusters = stats::cutree(model, k = n))
  }
  out
}

#' @keywords internal
.cluster_analysis_dbscan <- function(data = NULL, dist = NULL, eps = 0.15, min_size = 0.1, ...) {
  insight::check_if_installed("fpc")

  if(!is.null(dist)) {
    if(min_size < 1) min_size <- round(min_size * dim(as.matrix(dist))[1])
    model <- fpc::dbscan(dist, eps = eps, MinPts = min_size, method = "dist", ...)
  } else {
    if(min_size < 1) min_size <- round(min_size * nrow(data))
    model <- fpc::dbscan(data, eps = eps, MinPts = min_size, method = "hybrid", ...)
  }

  list(model = model, clusters = model$cluster)
}

# Methods ----------------------------------------------------------------

#' @export
predict.cluster_analysis <- function(object, newdata = NULL, ...) {
  if(is.null(newdata)) {
    attributes(object)$clusters
  } else {
    NextMethod()
  }
}


#' @export
print.cluster_analysis <- function(x, ...) {
  NextMethod()

  cat("\n")
  print(attributes(x)$performance)

  insight::print_color("\n# You can access the predicted clusters via 'predict()'.", "yellow")
  invisible(x)
}




# Plotting ----------------------------------------------------------------

#' @importFrom stats predict
#' @export
visualisation_recipe.cluster_analysis <- function(x, ...) {
  ori_data <- attributes(x)$data
  # Get 2 PCA Components
  pca <- principal_components(ori_data, n = 2)
  data <- predict(pca)
  names(data) <- c("x", "y")
  data$Cluster <- as.factor(attributes(x)$clusters)

  # Centers data (also on the PCA scale)
  data_centers <- predict(pca, newdata = as.data.frame(x)[names(ori_data)], names = c("x", "y"))
  data_centers$Cluster <- as.data.frame(x)$Cluster

  layers <- list()

  # Layers -----------------------
  layers[["l1"]] <- list(geom = "point",
                         data = data,
                         aes = list(x = "x", y = "y", color = "Cluster"))
  layers[["l2"]] <- list(geom = "point",
                         data = data_centers,
                         aes = list(x = "x", y = "y", color = "Cluster"),
                         shape = "+", size = 10)
  layers[["l3"]] <- list(geom = "labs",
                         x = "PCA - 1",
                         y = "PCA - 2",
                         title = "Clustering Solution")

  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}

#' @export
plot.cluster_analysis <- function(x, ...) {
  plot(visualisation_recipe(x, ...))
}