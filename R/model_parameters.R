#' Model Parameters
#'
#' Compute and extract model parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=model_parameters.htest]{Correlations and t-tests}}
#'  \item{\link[=model_parameters.aov]{ANOVAs}}
#'  \item{\link[=model_parameters.lm]{Frequentist regressions} (\code{lme4})}
#'  \item{\link[=model_parameters.stanreg]{Bayesian regressions} (\code{rstanarm}, \code{brms})}
#'  \item{\link[=model_parameters.principal]{PCA and FA} (\code{psych})}
#'  \item{\link[=model_parameters.lavaan]{CFA and SEM} (\code{lavaan})}
#'  }
#'
#' @param model Statistical Model.
#' @param ... Arguments passed to or from other methods.
#'
#' @export
model_parameters <- function(model, ...) {
  UseMethod("model_parameters")
}
