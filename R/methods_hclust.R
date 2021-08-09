#' @rdname model_parameters.kmeans
#' @examples
#' # hclust ---------------------------------
#' model <- hclust(dist(iris[1:4]))
#' rez <- model_parameters(model)
#' rez
#' @export
model_parameters.hclust <- function(model, verbose = TRUE, ...) {
  print("COMING SOON.")
  # library(pvclust)
  # fit <- pvclust(mat, method.hclust="ward.D", method.dist="canberra")
  #
  # params <- cbind(
  #   data.frame(
  #     Cluster = row.names(model$centers),
  #     n_Obs = model$size,
  #     Sum_Squares = model$withinss
  #   ),
  #   model$centers
  # )
}