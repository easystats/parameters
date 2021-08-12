#' @rdname model_parameters.kmeans
#'
#' @examples
#' if (require("mclust", quietly = TRUE)) {
#'   model <- mclust::Mclust(iris[1:4], verbose = FALSE)
#'   model_parameters(model)
#' }
#' @export
model_parameters.Mclust <- function(model, ...) {

  data <- as.data.frame(model$data)
  clusters <- model$classification

  params <- cluster_centers(data, clusters)

  # Long means
  means <- .long_loadings(params, loadings_columns = 4:ncol(params))
  means <- means[c("Cluster", "Loading", "Component")]
  names(means) <- c("Cluster", "Mean", "Variable")

  attr(params, "variance") <- attributes(params)$variance
  attr(params, "Sum_Squares_Between") <- attributes(params)$Sum_Squares_Between
  attr(params, "Sum_Squares_Total") <- attributes(params)$Sum_Squares_Total
  attr(params, "means") <- means
  attr(params, "model") <- model
  attr(params, "scores") <- clusters
  attr(params, "type") <- "mixture"
  attr(params, "title") <- "Gaussian finite mixture model fitted by EM algorithm"

  class(params) <- c("parameters_clusters", class(params))
  params
}
