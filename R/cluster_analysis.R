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
#' @param n Number of clusters used for supervised cluster methods. If `NULL`,
#' the number of clusters to extract is determined by calling [n_clusters()]. Note
#' that this argument does not apply for unsupervised clustering methods like
#' `dbscan`, `mixture`, `pvclust`, or `pamk`.
#' @param method Method for computing the cluster analysis. Can be `"kmeans"`
#'   (default; k-means using `kmeans()`), `"hkmeans"` (hierarchical k-means
#'   using `factoextra::hkmeans()`), `pam` (K-Medoids using `cluster::pam()`),
#'   `pamk` (K-Medoids that finds out the number of clusters), `"hclust"`
#'   (hierarchical clustering using `hclust()` or `pvclust::pvclust()`),
#'   `dbscan` (DBSCAN using `dbscan::dbscan()`), `hdbscan` (Hierarchical DBSCAN
#'   using `dbscan::hdbscan()`), or `mixture` (Mixture modelling using
#'   `mclust::Mclust()`, which requires the user to run `library(mclust)`
#'   before).
#' @param distance_method Distance measure to be used for methods based on
#'   distances (e.g., when `method = "hclust"` for hierarchical clustering. For
#'   other methods, such as `"kmeans"`, this argument will be ignored). Must be
#'   one of `"euclidean"`, `"maximum"`, `"manhattan"`, `"canberra"`, `"binary"`
#'   or `"minkowski"`. See [dist()] and `pvclust::pvclust()` for more
#'   information.
#' @param hclust_method Agglomeration method to be used when `method = "hclust"`
#'   or `method = "hkmeans"` (for hierarchical clustering). This should be one
#'   of `"ward"`, `"ward.D2"`, `"single"`, `"complete"`, `"average"`,
#'   `"mcquitty"`, `"median"` or `"centroid"`. Default is `"complete"` (see
#'   [hclust()]).
#' @param kmeans_method Algorithm used for calculating kmeans cluster. Only applies,
#'   if `method = "kmeans"`. May be one of `"Hartigan-Wong"` (default),
#'   `"Lloyd"` (used by SPSS), or `"MacQueen"`. See [kmeans()] for details on
#'   this argument.
#' @param iterations The number of replications.
#' @param dbscan_eps The 'eps' argument for DBSCAN method. See [n_clusters_dbscan()].
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
#' - [n_clusters()] to determine the number of clusters to extract,
#' [cluster_discrimination()] to determine the accuracy of cluster group
#' classification via linear discriminant analysis (LDA) and
#' [check_clusterstructure()] to check suitability of data
#' for clustering.
#' - https://www.datanovia.com/en/lessons/
#'
#' @examples
#' set.seed(33)
#' # K-Means ====================================================
#' rez <- cluster_analysis(iris[1:4], n = 3, method = "kmeans")
#' rez # Show results
#' predict(rez) # Get clusters
#' summary(rez) # Extract the centers values (can use 'plot()' on that)
#' cluster_discrimination(rez) # Perform LDA
#'
#' # Hierarchical k-means (more robust k-means)
#' if (require("factoextra", quietly = TRUE)) {
#'   rez <- cluster_analysis(iris[1:4], n = 3, method = "hkmeans")
#'   rez # Show results
#'   predict(rez) # Get clusters
#' }
#'
#' # Hierarchical Clustering (hclust) ===========================
#' rez <- cluster_analysis(iris[1:4], n = 3, method = "hclust")
#' rez # Show results
#' predict(rez) # Get clusters
#'
#' # K-Medoids (pam) ============================================
#' if (require("cluster", quietly = TRUE)) {
#'   rez <- cluster_analysis(iris[1:4], n = 3, method = "pam")
#'   rez # Show results
#'   predict(rez) # Get clusters
#' }
#'
#' # PAM with automated number of clusters
#' if (require("fpc", quietly = TRUE)) {
#'   rez <- cluster_analysis(iris[1:4], method = "pamk")
#'   rez # Show results
#'   predict(rez) # Get clusters
#' }
#'
#' # DBSCAN ====================================================
#' if (require("dbscan", quietly = TRUE)) {
#'   # Note that you can assimilate more outliers (cluster 0) to neighbouring
#'   # clusters by setting borderPoints = TRUE.
#'   rez <- cluster_analysis(iris[1:4], method = "dbscan", dbscan_eps = 1.45)
#'   rez # Show results
#'   predict(rez) # Get clusters
#' }
#'
#' # Mixture ====================================================
#' if (require("mclust", quietly = TRUE)) {
#'   library(mclust) # Needs the package to be loaded
#'   rez <- cluster_analysis(iris[1:4], method = "mixture")
#'   rez # Show results
#'   predict(rez) # Get clusters
#' }
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
                             dbscan_eps = 15,
                             iterations = 100,
                             ...) {


  # Sanity checks -----------------------------------------------------------
  insight::check_if_installed("performance")

  # match arguments
  method <- match.arg(method, choices = c("kmeans", "hkmeans", "pam", "pamk", "hclust", "dbscan", "hdbscan", "mixture"), several.ok = TRUE)

  # Preparation -------------------------------------------------------------

  # Preprocess data
  data <- .prepare_data_clustering(x, include_factors = include_factors, standardize = standardize, ...)

  # Get number of clusters
  if (is.null(n) && any(method %in% c("kmeans", "hkmeans", "pam"))) {
    n <- tryCatch(
      {
        nc <- n_clusters(data, standardize = FALSE, ...)
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
    rez <- .cluster_analysis_kmeans(data, n = n, kmeans_method = kmeans_method, iterations = iterations, ...)
  } else if (any(method %in% c("hkmeans"))) {
    rez <- .cluster_analysis_hkmeans(data, n = n, kmeans_method = kmeans_method, hclust_method = hclust_method, iterations = iterations, ...)
  } else if (any(method %in% c("pam"))) {
    rez <- .cluster_analysis_pam(data, n = n, distance_method = distance_method, ...)
  } else if (any(method %in% c("pamk"))) {
    rez <- .cluster_analysis_pamk(data, distance_method = distance_method, ...)
  } else if (any(method %in% c("hclust"))) {
    rez <- .cluster_analysis_hclust(data, n = n, distance_method = distance_method, hclust_method = hclust_method, iterations = iterations, ...)
  } else if (any(method == "dbscan")) {
    rez <- .cluster_analysis_dbscan(data, dbscan_eps = dbscan_eps, ...)
  } else if (any(method == "hdbscan")) {
    rez <- .cluster_analysis_hdbscan(data, ...)
  } else if (any(method %in% c("mixture", "mclust"))) {
    rez <- .cluster_analysis_mixture(data, ...)
  } else {
    stop("Did not find 'method' argument. Could be misspecified.")
  }

  # Assign clusters to observations
  # Create NA-vector of same length as original data frame
  clusters <- rep(NA, times = nrow(x))
  # Create vector with cluster group classification (with missing)
  complete_cases <- stats::complete.cases(x[names(data)])
  clusters[complete_cases] <- rez$clusters

  # Get clustering parameters
  out <- model_parameters(rez$model, data = data, clusters = clusters, ...)
  performance <- cluster_performance(out)

  attr(out, "model") <- rez$model
  attr(out, "method") <- method
  attr(out, "clusters") <- clusters
  attr(out, "data") <- data
  attr(out, "performance") <- performance

  class(out) <- c("cluster_analysis", class(out))
  out
}



# Clustering Methods --------------------------------------------------------

#' @keywords internal
.cluster_analysis_kmeans <- function(data, n = 2, kmeans_method = "Hartigan-Wong", iterations = 100, ...) {
  model <- stats::kmeans(data, centers = n, algorithm = kmeans_method, iter.max = iterations, ...)
  list(model = model, clusters = model$cluster)
}

#' @keywords internal
.cluster_analysis_hkmeans <- function(data, n = 2, kmeans_method = "Hartigan-Wong", hclust_method = "complete", iterations = 100, ...) {
  insight::check_if_installed("factoextra")
  model <- factoextra::hkmeans(data, k = n, km.algorithm = kmeans_method, iter.max = iterations, hc.method = hclust_method, ...)
  list(model = model, clusters = model$cluster)
}

#' @keywords internal
.cluster_analysis_pam <- function(data = NULL, n = 2, distance_method = "euclidean", ...) {
  insight::check_if_installed("cluster")

  model <- cluster::pam(data, k = n, metric = distance_method, ...)

  list(model = model, clusters = model$clustering)
}

#' @keywords internal
.cluster_analysis_pamk <- function(data = NULL, distance_method = "euclidean", pamk_method = "ch", ...) {
  insight::check_if_installed("fpc")

  model <- fpc::pamk(data, metric = distance_method, criterion = pamk_method, ...)

  list(model = model$pamobject, clusters = model$pamobject$clustering)
}

#' @keywords internal
.cluster_analysis_hclust <- function(data, n = 2, distance_method = "euclidean", hclust_method = "complete", iterations = 100, ...) {
  if (is.null(n)) {
    rez <- n_clusters_hclust(data, preprocess = FALSE, distance_method = distance_method, hclust_method = hclust_method, iterations = iterations, ...)
    out <- list(model = attributes(rez)$model, clusters = rez$Cluster)
  } else {
    if (distance_method %in% c("correlation", "uncentered", "abscor")) {
      warning(paste0("method '", distance_method, "' not supported by regular hclust(). Please specify another one or set n = NULL to use pvclust."))
    }
    dist <- dist(data, method = distance_method, ...)
    model <- stats::hclust(dist, method = hclust_method, ...)
    out <- list(model = model, clusters = stats::cutree(model, k = n))
  }
  out
}

#' @keywords internal
.cluster_analysis_dbscan <- function(data = NULL, dbscan_eps = 0.15, min_size = 0.1, borderPoints = FALSE, ...) {
  insight::check_if_installed("dbscan")

  if (min_size < 1) min_size <- round(min_size * nrow(data))
  model <- dbscan::dbscan(data, eps = dbscan_eps, minPts = min_size, borderPoints = borderPoints, ...)

  list(model = model, clusters = model$cluster)
}


#' @keywords internal
.cluster_analysis_hdbscan <- function(data = NULL, min_size = 0.1, ...) {
  insight::check_if_installed("dbscan")

  if (min_size < 1) min_size <- round(min_size * nrow(data))
  model <- dbscan::hdbscan(data, minPts = min_size, ...)

  list(model = model, clusters = model$cluster)
}


#' @keywords internal
.cluster_analysis_mixture <- function(data = NULL, ...) {
  insight::check_if_installed("mclust")

  model <- mclust::Mclust(data, verbose = FALSE, ...)

  list(model = model, clusters = model$classification)
}



# Methods ----------------------------------------------------------------

#' @export
predict.cluster_analysis <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
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

  insight::print_color("\n# You can access the predicted clusters via 'predict()'.\n", "yellow")
  invisible(x)
}

#' @export
summary.cluster_analysis <- function(object, ...) {
  data <- as.data.frame(object)
  cols <- names(attributes(object)$data)
  data <- data[names(data) %in% c(cols, "Cluster")] # Keep only data

  class(data) <- c("cluster_analysis_summary", class(data))
  data
}


# Plotting ----------------------------------------------------------------

#' @export
visualisation_recipe.cluster_analysis_summary <- function(x, ...) {
  data <- datawizard::data_to_long(
    x,
    cols = names(x)[-1], # skip 'Cluster' column
    names_to = "Group",
    values_to = "Center"
  )

  layers <- list()

  # Layers -----------------------

  layers[["l1"]] <- list(
    geom = "bar",
    data = data,
    aes = list(x = "Cluster", y = "Center", fill = "Group"),
    position = "dodge"
  )
  layers[["l2"]] <- list(
    geom = "hline",
    data = data,
    aes = list(yintercept = 0),
    linetype = "dotted"
  )
  layers[["l3"]] <- list(
    geom = "labs",
    x = "Cluster Group",
    y = "Center",
    fill = "Variable",
    title = "Cluster Centers"
  )

  # Out
  class(layers) <- c("visualisation_recipe", "see_visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}


#' @export
visualisation_recipe.cluster_analysis <- function(x, show_data = "text", ...) {
  ori_data <- stats::na.omit(attributes(x)$data)
  # Get 2 PCA Components
  pca <- principal_components(ori_data, n = 2)
  data <- stats::predict(pca)
  names(data) <- c("x", "y")
  data$Cluster <- as.character(stats::na.omit(attributes(x)$clusters))

  data$label <- row.names(ori_data)
  if (!is.null(show_data) && show_data %in% c("label", "text")) {
    label <- "label"
  } else {
    label <- NULL
  }

  # Centers data (also on the PCA scale)
  data_centers <- stats::predict(pca, newdata = as.data.frame(x)[names(ori_data)], names = c("x", "y"))
  data_centers$Cluster <- as.character(as.data.frame(x)$Cluster)

  # Outliers
  data$Cluster[data$Cluster == "0"] <- NA
  data_centers <- data_centers[data_centers$Cluster != "0", ]

  layers <- list()

  # Layers -----------------------

  layers[["l1"]] <- list(
    geom = show_data,
    data = data,
    aes = list(x = "x", y = "y", label = label, color = "Cluster")
  )

  layers[["l2"]] <- list(
    geom = "point",
    data = data_centers,
    aes = list(x = "x", y = "y", color = "Cluster"),
    shape = "+", size = 10
  )
  layers[["l3"]] <- list(
    geom = "labs",
    x = "PCA - 1",
    y = "PCA - 2",
    title = "Clustering Solution"
  )

  # Out
  class(layers) <- c("visualisation_recipe", "see_visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}

#' @export
plot.cluster_analysis <- function(x, ...) {
  plot(visualisation_recipe(x, ...))
}
