#' Model Parameters
#'
#' Compute and extract model parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=model_parameters.htest]{Correlations and t-tests}}
#'  \item{\link[=model_parameters.aov]{ANOVAs}}
#'  \item{\link[=model_parameters.lm]{Frequentist regressions} (\code{lm}, \code{glm})}
#'  \item{\link[=model_parameters.merMod]{Frequentist mixed models} (\code{lme4})}
#'  \item{\link[=model_parameters.stanreg]{Bayesian models} (\code{rstanarm}, \code{brms})}
#'  \item{\link[=model_parameters.principal]{PCA and FA} (\code{psych})}
#'  \item{\link[=model_parameters.lavaan]{CFA and SEM} (\code{lavaan})}
#'  }
#'
#' @param model Statistical Model.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data.frame of indices related to the model's parameters.
#' @export
model_parameters <- function(model, ...) {
  UseMethod("model_parameters")
}


#' @export
parameters <- model_parameters
