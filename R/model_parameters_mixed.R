#' Parameters from Mixed Models
#'
#' Parameters from (linear) mixed models.
#'
#' @param model A mixed model.
#' @param effects Should parameters for fixed effects, random effects or both be returned? Only applies to mixed models. May be abbreviated.
#' @param details Logical, if \code{TRUE}, a summary of the random effects is included. See \code{\link{random_parameters}} for details.
#' @inheritParams model_parameters.default
#' @param df_method Method for computing degrees of freedom for p values, standard errors and confidence intervals (CI). May be \code{"wald"} (default, see \code{\link{degrees_of_freedom}}), \code{"ml1"} (see \code{\link{dof_ml1}}), \code{"betwithin"} (see \code{\link{dof_betwithin}}), \code{"satterthwaite"} (see \code{\link{dof_satterthwaite}}) or \code{"kenward"} (see \code{\link{dof_kenward}}). Note that when \code{df_method} is not \code{"wald"}, robust standard errors etc. cannot be computed.
#'
#' @seealso \code{\link[=standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' if (require("lme4")) {
#'   data(mtcars)
#'   model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
#'   model_parameters(model)
#' }
#'
#' \donttest{
#' if (require("glmmTMB")) {
#'   data(Salamanders)
#'   model <- glmmTMB(
#'     count ~ spp + mined + (1 | site),
#'     ziformula = ~mined,
#'     family = poisson(),
#'     data = Salamanders
#'   )
#'   model_parameters(model, details = TRUE)
#' }
#'
#' if (require("lme4")) {
#'   model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
#'   model_parameters(model, bootstrap = TRUE, iterations = 50)
#' }
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.merMod <- function(model, ci = .95, bootstrap = FALSE, df_method = "wald", iterations = 1000, standardize = NULL, exponentiate = FALSE, robust = FALSE, details = FALSE, ...) {
  # p-values, CI and se might be based of wald, or KR
  df_method <- tolower(df_method)
  df_method <- match.arg(df_method, choices = c("wald", "ml1", "betwithin", "satterthwaite", "kenward"))

  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_mixed(model, ci = ci, df_method = df_method, robust = robust, standardize = standardize, ...)
  }


  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(parameters, model, ci, exponentiate, ...)

  if (isTRUE(details)) {
    attr(parameters, "details") <- .randomeffects_summary(model)
  }

  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}

#' @export
model_parameters.lme <- model_parameters.merMod





# Mixed Models with zero inflation ------------------------------------

#' @inheritParams simulate_model
#' @rdname model_parameters.merMod
#' @export
model_parameters.glmmTMB <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated"), standardize = NULL, exponentiate = FALSE, df_method = NULL, details = FALSE, ...) {
  component <- match.arg(component)

  # p-values, CI and se might be based on differen df-methods
  if (!is.null(df_method)) {
    df_method <- tolower(df_method)
    df_method <- match.arg(df_method, choices = c("wald", "ml1", "betwithin"))
  }

  # fix argument, if model has no zi-part
  if (!insight::model_info(model)$is_zero_inflated && component != "conditional") {
    component <- "conditional"
  }

  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_generic(model, ci = ci, component = component, standardize = standardize, robust = FALSE, df_method = df_method, ...)
  }


  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(parameters, model, ci, exponentiate, ...)

  if (isTRUE(details)) {
    attr(parameters, "details") <- .randomeffects_summary(model)
  }

  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}

#' @export
model_parameters.MixMod <- model_parameters.glmmTMB




# other mixed models -------------------------------


#' @rdname model_parameters.merMod
#' @export
model_parameters.mixor <- function(model, ci = .95, effects = c("all", "fixed", "random"), bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, details = FALSE, ...) {
  effects <- match.arg(effects)
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Effects"),
    standardize = standardize,
    exponentiate = exponentiate,
    effects = effects,
    robust = FALSE,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)

  if (isTRUE(details)) {
    attr(out, "details") <- .randomeffects_summary(model)
  }

  out
}


#' @rdname model_parameters.merMod
#' @export
model_parameters.clmm <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, details = FALSE, df_method = NULL, ...) {
  # p-values, CI and se might be based on differen df-methods
  if (!is.null(df_method)) {
    df_method <- tolower(df_method)
    df_method <- match.arg(df_method, choices = c("wald", "ml1", "betwithin"))
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    effects = "fixed",
    robust = FALSE,
    df_method = df_method,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)

  if (isTRUE(details)) {
    attr(out, "details") <- .randomeffects_summary(model)
  }

  out
}


#' @export
model_parameters.cpglmm <- model_parameters.clmm

#' @export
model_parameters.rlmerMod <- model_parameters.clmm
