#' Model Parameters
#'
#' Compute and extract model parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=model_parameters.htest]{Correlations and t-tests}}
#'  \item{\link[=model_parameters.aov]{ANOVAs}}
#'  \item{\link[=model_parameters.lm]{Frequentist regressions} (\code{lm}, \code{glm}, ...)}
#'  \item{\link[=model_parameters.merMod]{Frequentist mixed models} (\code{lme4}, \code{nlme}, \code{glmmTMB}, ...)}
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
# for new model objects.