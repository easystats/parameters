#' Parameters from Cluster Models (k-means, ...)
#'
#' Format cluster models obtained for example by [kmeans()].
#'
#' @param model Cluster model.
#' @param data A data frame.
#' @param clusters A vector with clusters assignments (must be same length as
#' rows in data).
#' @param ... Arguments passed to or from other methods.
#'
#' @examplesIf require("factoextra", quietly = TRUE) && require("dbscan", quietly = TRUE) && require("cluster", quietly = TRUE) && require("fpc", quietly = TRUE)
#' \donttest{
#' #
#' # K-means -------------------------------
#' model <- kmeans(iris[1:4], centers = 3)
#' rez <- model_parameters(model)
#' rez
#'
#' # Get clusters
#' predict(rez)
#'
#' # Clusters centers in long form
#' attributes(rez)$means
#'
#' # Between and Total Sum of Squares
#' attributes(rez)$Sum_Squares_Total
#' attributes(rez)$Sum_Squares_Between
#'
#' #
#' # Hierarchical clustering (hclust) ---------------------------
#' data <- iris[1:4]
#' model <- hclust(dist(data))
#' clusters <- cutree(model, 3)
#'
#' rez <- model_parameters(model, data, clusters)
#' rez
#'
#' # Get clusters
#' predict(rez)
#'
#' # Clusters centers in long form
#' attributes(rez)$means
#'
#' # Between and Total Sum of Squares
#' attributes(rez)$Total_Sum_Squares
#' attributes(rez)$Between_Sum_Squares
#'
#' #
#' # Hierarchical K-means (factoextra::hkclust) ----------------------
#' data <- iris[1:4]
#' model <- factoextra::hkmeans(data, k = 3)
#'
#' rez <- model_parameters(model)
#' rez
#'
#' # Get clusters
#' predict(rez)
#'
#' # Clusters centers in long form
#' attributes(rez)$means
#'
#' # Between and Total Sum of Squares
#' attributes(rez)$Sum_Squares_Total
#' attributes(rez)$Sum_Squares_Between
#'
#' # K-Medoids (PAM and HPAM) ==============
#' model <- cluster::pam(iris[1:4], k = 3)
#' model_parameters(model)
#'
#' model <- fpc::pamk(iris[1:4], criterion = "ch")
#' model_parameters(model)
#'
#' # DBSCAN ---------------------------
#' model <- dbscan::dbscan(iris[1:4], eps = 1.45, minPts = 10)
#'
#' rez <- model_parameters(model, iris[1:4])
#' rez
#'
#' # Get clusters
#' predict(rez)
#'
#' # Clusters centers in long form
#' attributes(rez)$means
#'
#' # Between and Total Sum of Squares
#' attributes(rez)$Sum_Squares_Total
#' attributes(rez)$Sum_Squares_Between
#'
#' # HDBSCAN
#' model <- dbscan::hdbscan(iris[1:4], minPts = 10)
#' model_parameters(model, iris[1:4])
#' }
#' @export
model_parameters.hclust <- function(model, data = NULL, clusters = NULL, ...) {
  if (is.null(data)) {
    insight::format_error(
      "This function requires the data used to compute the clustering to be provided via `data` as it is not accessible from the clustering object itself."
    )
  }
  if (is.null(clusters)) {
    insight::format_error(
      "This function requires a vector of clusters assignments of same length as data to be passed, as it is not contained in the clustering object itself."
    )
  }

  params <- cluster_centers(data, clusters, ...)

  # Long means
  means <- datawizard::reshape_longer(params,
    select = 4:ncol(params),
    values_to = "Mean",
    names_to = "Variable"
  )

  attr(params, "variance") <- attributes(params)$variance
  attr(params, "Sum_Squares_Between") <- attributes(params)$Sum_Squares_Between
  attr(params, "Sum_Squares_Total") <- attributes(params)$Sum_Squares_Total
  attr(params, "means") <- means
  attr(params, "model") <- model
  attr(params, "scores") <- clusters
  attr(params, "type") <- "hclust"

  class(params) <- c("parameters_clusters", class(params))
  params
}


#' @export
model_parameters.pvclust <- function(model, data = NULL, clusters = NULL, ci = 0.95, ...) {
  if (is.null(data)) {
    insight::format_error(
      "This function requires the data used to compute the clustering to be provided via `data` as it is not accessible from the clustering object itself."
    )
  }

  if (is.null(clusters)) {
    clusters <- .model_parameters_pvclust_clusters(model, data, ci)$Cluster
  }

  params <- .cluster_centers_params(data, clusters, ...)

  attr(params, "model") <- model
  attr(params, "type") <- "pvclust"
  attr(params, "title") <- "Bootstrapped Hierarchical Clustering (PVCLUST)"

  params
}


# Utils -------------------------------------------------------------------


#' @keywords internal
.model_parameters_pvclust_clusters <- function(model, data, ci = 0.95) {
  insight::check_if_installed("pvclust")
  rez <- pvclust::pvpick(model, alpha = ci, pv = "si")

  # Assign clusters
  out <- data.frame()
  for (cluster in seq_along(rez$clusters)) {
    out <- rbind(out, data.frame(Cluster = cluster, Row = rez$clusters[[cluster]], stringsAsFactors = FALSE), make.row.names = FALSE, stringsAsFactors = FALSE)
  }

  # Add points not in significant clusters
  remaining_rows <- row.names(data)[!row.names(data) %in% out$Row]
  if (length(remaining_rows) > 0) {
    out <- rbind(out, data.frame(Cluster = 0, Row = remaining_rows, stringsAsFactors = FALSE), make.row.names = FALSE, stringsAsFactors = FALSE)
  }

  # Reorder according to original order of rows
  out <- out[order(match(out$Row, row.names(data))), ]
  row.names(out) <- NULL
  out
}
