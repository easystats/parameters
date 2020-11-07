#' Parameters from Mixed Models
#'
#' Parameters from (linear) mixed models.
#'
#' @param model A mixed model.
#' @param effects Should parameters for fixed effects, random effects or both be returned? Only applies to mixed models. May be abbreviated.
#' @param details Logical, if \code{TRUE}, a summary of the random effects is included. See \code{\link{random_parameters}} for details.
#' @param df_method Method for computing degrees of freedom for p values, standard errors and confidence intervals (CI). May be \code{"wald"} (default, see \code{\link{degrees_of_freedom}}), \code{"ml1"} (see \code{\link{dof_ml1}}), \code{"betwithin"} (see \code{\link{dof_betwithin}}), \code{"satterthwaite"} (see \code{\link{dof_satterthwaite}}) or \code{"kenward"} (see \code{\link{dof_kenward}}). Note that when \code{df_method} is not \code{"wald"}, robust standard errors etc. cannot be computed.
#' @param wb_component Logical, if \code{TRUE} and models contains within- and between-effects (see \code{\link{demean}}), the \code{Component} column will indicate which variables belong to the within-effects, between-effects, and cross-level interactions. By default, the \code{Component} column indicates, which parameters belong to the conditional or zero-inflated component of the model.
#' @inheritParams model_parameters.default
#'
#' @seealso \code{\link[insight:standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @note There is also a \href{https://easystats.github.io/see/articles/parameters.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examples
#' library(parameters)
#' if (require("lme4")) {
#'   data(mtcars)
#'   model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
#'   model_parameters(model)
#' }
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
model_parameters.merMod <- function(model, ci = .95, bootstrap = FALSE, df_method = "wald", iterations = 1000, standardize = NULL, exponentiate = FALSE, robust = FALSE, details = FALSE, p_adjust = NULL, wb_component = TRUE, ...) {
  # p-values, CI and se might be based of wald, or KR
  df_method <- tolower(df_method)
  df_method <- match.arg(df_method, choices = c("wald", "ml1", "betwithin", "satterthwaite", "kenward"))

  # Processing
  if (bootstrap) {
    params <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    params <- .extract_parameters_mixed(model, ci = ci, df_method = df_method, robust = robust, standardize = standardize, p_adjust = p_adjust, wb_component = wb_component, ...)
  }


  if (exponentiate) params <- .exponentiate_parameters(params)
  params <- .add_model_parameters_attributes(params, model, ci, exponentiate, bootstrap, iterations, df_method, ...)

  if (isTRUE(details)) {
    attr(params, "details") <- .randomeffects_summary(model)
  }

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}

#' @export
model_parameters.lme <- model_parameters.merMod





# Mixed Models with zero inflation ------------------------------------

#' @importFrom stats coef
#' @inheritParams simulate_model
#' @rdname model_parameters.merMod
#' @export
model_parameters.glmmTMB <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), standardize = NULL, exponentiate = FALSE, df_method = NULL, details = FALSE, wb_component = TRUE, ...) {
  component <- match.arg(component)

  # p-values, CI and se might be based on different df-methods
  df_method <- .check_df_method(df_method)

  # fix argument, if model has only conditional component
  cs <- stats::coef(summary(model))
  has_zeroinf <- insight::model_info(model)$is_zero_inflated
  has_disp <- is.list(cs) && !is.null(cs$disp)

  if (!has_zeroinf && !has_disp && component != "conditional") {
    component <- "conditional"
  }

  # Processing
  if (bootstrap) {
    params <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    params <- .extract_parameters_generic(model, ci = ci, component = component, standardize = standardize, robust = FALSE, df_method = df_method, wb_component = wb_component, ...)
  }


  # add dispersion parameter
  if (inherits(model, "glmmTMB") && !is.null(params$Component) && !"dispersion" %in% params$Component) {
    dispersion_param <- insight::get_parameters(model, component = "dispersion")
    if (!is.null(dispersion_param)) {
      params[nrow(params) + 1, ] <- NA
      params[nrow(params), "Parameter"] <- dispersion_param$Parameter[1]
      params[nrow(params), "Coefficient"] <- dispersion_param$Estimate[1]
      params[nrow(params), "Component"] <- dispersion_param$Component[1]
    }
  }


  if (exponentiate) params <- .exponentiate_parameters(params)
  params <- .add_model_parameters_attributes(params, model, ci, exponentiate, ...)

  if (isTRUE(details)) {
    attr(params, "details") <- .randomeffects_summary(model)
  }

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
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


#' @export
model_parameters.glmm <- function(model, ci = .95, effects = c("all", "fixed", "random"), bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, ...) {
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
  out
}


#' @rdname model_parameters.merMod
#' @export
model_parameters.clmm <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, details = FALSE, df_method = NULL, ...) {
  # p-values, CI and se might be based on differen df-methods
  df_method <- .check_df_method(df_method)

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


#' @export
model_parameters.merModList <- function(model, ci = .95, exponentiate = FALSE, p_adjust = NULL, ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = FALSE,
    iterations = 10,
    merge_by = "Parameter",
    standardize = NULL,
    exponentiate = exponentiate,
    robust = FALSE,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}







# tools --------------------

.check_df_method <- function(df_method) {
  if (!is.null(df_method)) {
    df_method <- tolower(df_method)
    if (df_method %in% c("satterthwaite", "kenward", "kr")) {
      warning("Satterthwaite or Kenward-Rogers approximation of degrees of freedom is only available for linear mixed models.", call. = FALSE)
      df_method <- "wald"
    }
    df_method <- match.arg(df_method, choices = c("wald", "ml1", "betwithin"))
  }
  df_method
}
