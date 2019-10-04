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
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters <- function(model, ...) {
  UseMethod("model_parameters")
}


#' @rdname model_parameters
#' @export
parameters <- model_parameters


# Templates for adding new object classes to model_paramaters.
#
# model_parameters.lm() is suitable for the generic lm and glm classes. It
# should _not_ be used as a template, because it's likely to fail for other
# model classes.
#
# model_parameters.merMod() is speficic to lme4, because it allows for KR-approximation
# of the df for p-values.
#
# model_parameters.lme() can be used for (mixed) models that have a working "update()"
# method, so standardization is possible. Can be used for mixed or "normal" models.
#
# model_parameters.glmmTMB() can be used for (mixed) models where standardization
# is not possible due to non-working "update()". The "component" argument may be removed,
# but can also be used for non-mixed models.
#
# model_parameters.polr() is the most generic method for non-mixed models w/o
# standardization, where "update()" does not work.
#
# Summary
# -------
# In most cases model_parameters.polr() or model_parameters.lme() can be called
# for new model objects. A ".get_statistic()" function might be added.
