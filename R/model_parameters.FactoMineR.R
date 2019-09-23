#' FactoMineR objects Parameters
#'
#' Parameters of FactoMineR objects.
#'
#' @param model Object of class \code{FactoMineR}.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(FactoMineR)
#' model <- FactoMineR::PCA(iris[, 1:4], ncp = 2)
#'
#' @return A data.frame of indices related to the model's parameters.
#' @export
model_parameters.PCA <- function(model, ...) {

  model$eig
  scores <- model$var$contrib


  # Add attributes
  # attr(loadings, "summary") <- data_summary
  attr(loadings, "model") <- model
  attr(loadings, "rotation") <- "none"
  attr(loadings, "scores") <- scores
  attr(loadings, "additional_arguments") <- list(...)
  attr(loadings, "n") <- model$call$ncp
  attr(loadings, "type") <- "principal"
  # attr(loadings, "loadings_columns") <- loading_cols

  class(loadings) <- c("parameters_pca", class(loadings))
  loadings
}

