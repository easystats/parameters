#' @export
model_parameters.pam <- function(model, data = NULL, clusters = NULL, ...) {
  if (is.null(data)) data <- as.data.frame(model$data)
  if (is.null(clusters)) clusters <- model$clustering

  params <- .cluster_centers_params(data, clusters, ...)

  attr(params, "model") <- model
  attr(params, "type") <- "pam"
  attr(params, "title") <- "K-Medoids"

  params
}
