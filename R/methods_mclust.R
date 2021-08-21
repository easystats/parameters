#' @rdname model_parameters.kmeans
#'
#' @examples
#' if (require("mclust", quietly = TRUE)) {
#'   model <- mclust::Mclust(iris[1:4], verbose = FALSE)
#'   model_parameters(model)
#' }
#' @export
model_parameters.Mclust <- function(model, data = NULL, clusters = NULL, ...) {

  if(is.null(data)) data <- as.data.frame(model$data)
  if(is.null(clusters)) clusters <- model$classification

  params <- .cluster_centers_params(data, clusters, ...)

  attr(params, "model") <- model
  attr(params, "type") <- "mixture"
  attr(params, "title") <- "Gaussian finite mixture model fitted by EM algorithm"

  params
}
