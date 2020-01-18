#' Parameters from Mixed Models
#'
#' Parameters from (linear) mixed models.
#'
#' @param model A mixed model.
#' @param effects Should parameters for fixed effects, random effects or both be returned? Only applies to mixed models. May be abbreviated.
#' @inheritParams model_parameters.default
#' @param df_method Method for computing degrees of freedom for p values, standard errors and confidence intervals (CI). May be \code{"wald"} (default, see \code{\link{degrees_of_freedom}}), \code{"ml1"} (see \code{\link{dof_ml1}}), \code{"satterthwaite"} (see \code{\link{dof_satterthwaite}}) or \code{"kenward"} (see \code{\link{dof_kenward}}).
#'
#' @seealso \code{\link[=standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' library(lme4)
#' library(glmmTMB)
#'
#' model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
#' model_parameters(model)
#'
#' model <- glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' model_parameters(model)
#' \donttest{
#' model <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
#' model_parameters(model, bootstrap = TRUE, iterations = 50)
#' }
#'
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.merMod <- function(model, ci = .95, bootstrap = FALSE, df_method = "wald", iterations = 1000, standardize = NULL, exponentiate = FALSE, ...) {
  # p-values, CI and se might be based of wald, or KR
  df_method <- match.arg(df_method, choices = c("wald", "ml1", "satterthwaite", "kenward"))

  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_mixed(model, ci = ci, df_method = df_method, ...)
  }


  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(parameters, model, ci, exponentiate, ...)
  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}




# Mixed Models with zero inflation ------------------------------------

#' @inheritParams simulate_model
#' @rdname model_parameters.merMod
#' @export
model_parameters.glmmTMB <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated"), standardize = NULL, exponentiate = FALSE, ...) {
  component <- match.arg(component)

  # fix argument, if model has no zi-part
  if (!insight::model_info(model)$is_zero_inflated && component != "conditional") {
    component <- "conditional"
  }

  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_generic(model, ci = ci, component = component, standardize = standardize, robust = FALSE, ...)
  }


  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(parameters, model, ci, exponentiate, ...)
  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}

#' @export
model_parameters.MixMod <- model_parameters.glmmTMB
