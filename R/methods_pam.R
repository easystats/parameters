#' @rdname model_parameters.kmeans
#'
#' @examples
#' # K-Medoids (PAM and HPAM) ==============
#' if (require("cluster", quietly = TRUE)) {
#'   model <- cluster::pam(iris[1:4], k = 3)
#'   model_parameters(model)
#' }
#' if (require("fpc", quietly = TRUE)) {
#'   model <- fpc::pamk(iris[1:4], criterion = "ch")
#'   model_parameters(model)
#' }
#' @export
model_parameters.pam <- function(model, ...) {

  data <- as.data.frame(model$data)
  clusters <- model$clustering

  params <- cluster_centers(data, clusters)

  # Long means
  means <- datawizard::reshape_longer(params,
                                      cols = 4:ncol(params),
                                      values_to = "Mean",
                                      names_to = "Variable")

  attr(params, "variance") <- attributes(params)$variance
  attr(params, "Sum_Squares_Between") <- attributes(params)$Sum_Squares_Between
  attr(params, "Sum_Squares_Total") <- attributes(params)$Sum_Squares_Total
  attr(params, "means") <- means
  attr(params, "model") <- model
  attr(params, "scores") <- clusters
  attr(params, "type") <- "mixture"
  attr(params, "title") <- "K-Medoids"

  class(params) <- c("parameters_clusters", class(params))
  params
}

