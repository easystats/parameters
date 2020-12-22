#' Parameters from Mixture Models
#'
#' Format mixture models obtained for example by \code{mclust::Mclust}.
#'
#' @param model Mixture model.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_parameters.default
#'
#' @examples
#' library(parameters)
#' if (require("mclust")) {
#'   model <- mclust::Mclust(iris[1:4], verbose = FALSE)
#'   model_parameters(model)
#' }
#' @export
model_parameters.Mclust <- function(model, verbose = TRUE, ...) {
  params <- cbind(
    data.frame(
      Cluster = as.data.frame(table(model$classification))$Var1,
      n_Obs = as.data.frame(table(model$classification))$Freq
    ),
    t(model$parameters$mean)
  )


  # Long means
  means <- .long_loadings(params, loadings_columns = 3:ncol(params))
  means <- means[c("Cluster", "Loading", "Component")]
  names(means) <- c("Cluster", "Mean", "Variable")

  attr(params, "means") <- means
  attr(params, "scores") <- model$classification
  attr(params, "model") <- model
  attr(params, "type") <- "mixture"
  attr(params, "title") <- "Gaussian finite mixture model fitted by EM algorithm"

  class(params) <- c("parameters_model", "parameters_clusters", class(params))
  params
}
