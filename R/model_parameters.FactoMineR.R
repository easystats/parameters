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
model_parameters.FactoMineR <- function(model, ...) {

  out <- NA

  class(out) <- c("parameters_model", class(out))
  out
}
