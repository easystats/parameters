#' Model Parameters
#'
#' Compute and extract model parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=model_parameters.htest]{Correlations and t-tests}}
#'  \item{\link[=model_parameters.aov]{ANOVAs}}
#'  \item{\link[=model_parameters.lm]{Regression models} (\code{lm}, \code{glm}, \pkg{survey}, ...)}
#'  \item{\link[=model_parameters.gam]{Additive models} (\code{gam}, \code{gamm}, ...)}
#'  \item{\link[=model_parameters.zeroinfl]{Zero-inflated models} (\code{hurdle}, \code{zeroinfl}, \code{zerocount})}
#'  \item{\link[=model_parameters.merMod]{Mixed models} (\pkg{lme4}, \pkg{nlme}, \pkg{glmmTMB}, ...)}
#'  \item{\link[=model_parameters.BFBayesFactor]{Bayesian tests} (\pkg{BayesFactor})}
#'  \item{\link[=model_parameters.stanreg]{Bayesian models} (\pkg{rstanarm}, \pkg{brms}, \pkg{MCMCglmm})}
#'  \item{\link[=model_parameters.principal]{PCA and FA} (\pkg{psych})}
#'  \item{\link[=model_parameters.lavaan]{CFA and SEM} (\pkg{lavaan})}
#'   \item{\link[=model_parameters.kmeans]{Cluster models (k-means, ...)}}
#'  }
#'
#' @param model Statistical Model.
#' @param ... Arguments passed to or from other methods.
#'
#' @seealso \code{\link[=standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @note The \code{\link[=print.parameters_model]{print()}} method has several arguments to tweak the output.
#'
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters <- function(model, ...) {
  UseMethod("model_parameters")
}


#' @rdname model_parameters
#' @export
parameters <- model_parameters
