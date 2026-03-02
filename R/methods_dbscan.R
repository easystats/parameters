#' @export
model_parameters.dbscan <- function(model, data = NULL, clusters = NULL, ...) {
  if (is.null(data)) {
    insight::format_error("This function requires the data used to compute the clustering to be provided via `data` as it is not accessible from the clustering object itself.")
  }

  if (is.null(clusters)) {
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
