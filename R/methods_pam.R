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
model_parameters.pam <- function(model, data = NULL, clusters = NULL, ...) {

  if(is.null(data)) data <- as.data.frame(model$data)
  if(is.null(clusters)) clusters <- model$clustering

  params <- .cluster_centers_params(data, clusters, ...)

  attr(params, "model") <- model
  attr(params, "type") <- "pam"
  attr(params, "title") <- "K-Medoids"

  params
}

