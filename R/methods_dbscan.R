#' @rdname model_parameters.kmeans
#' @inheritParams cluster_centers
#'
#' @examples
#'
#' # DBSCAN ---------------------------
#' if (require("dbscan", quietly = TRUE)) {
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
#'
#' @export
model_parameters.dbscan <- function(model, data = NULL, clusters = NULL, ...) {
  if(is.null(data)) {
    stop("This function requires the data used to compute the clustering to be provided via 'data' as it is not accessible from the clustering object itself.")
  }

  if(is.null(clusters)) {
    clusters <- model$cluster
  }

  params <- .cluster_centers_params(data, clusters, ...)

  attr(params, "model") <- model
  attr(params, "type") <- "dbscan"
  attr(params, "title") <- ifelse(inherits(model, "hdbscan"), "HDBSCAN", "DBSCAN")

  params
}

#' @export
model_parameters.hdbscan <- model_parameters.dbscan