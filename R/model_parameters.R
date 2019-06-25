#' Model Parameters
#'
#' Compute and extract model parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=model_parameters.htest]{Correlations and t-tests}}
#'  \item{\link[=model_parameters.aov]{ANOVAs}}
#'  \item{\link[=model_parameters.lm]{Frequentist regressions}}
#'  \item{\link[=model_parameters.stanreg]{Bayesian regressions}}
#'  \item{\link[=model_parameters.principal]{PCA and FA}}
#'  }
#'
#' @param model Statistical Model.
#' @param ... Arguments passed to or from other methods.
#'
#' @export
model_parameters <- function(model, ...) {
  UseMethod("model_parameters")
}
