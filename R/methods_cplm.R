# classes: .cpglm, .bcpglm, .zcpglm, .cpglmm


########## .zcpglm ---------------


#' @title Parameters from Zero-Inflated Models
#' @name model_parameters.zcpglm
#'
#' @description
#' Parameters from zero-inflated models (from packages like **pscl**,
#' **cplm** or **countreg**).
#'
#' @param model A model with zero-inflation component.
#' @inheritParams model_parameters.default
#' @inheritParams simulate_model
#'
#' @seealso [insight::standardize_names()] to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' if (require("pscl")) {
#'   data("bioChemists")
#'   model <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)
#'   model_parameters(model)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @inheritParams simulate_model
#' @export
model_parameters.zcpglm <- function(model,
                                    ci = 0.95,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    component = c("all", "conditional", "zi", "zero_inflated"),
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    summary = getOption("parameters_summary", FALSE),
                                    include_info = getOption("parameters_info", FALSE),
                                    keep = NULL,
                                    drop = NULL,
                                    verbose = TRUE,
                                    ...) {
  component <- match.arg(component)

  # fix argument, if model has no zi-part
  if (!insight::model_info(model, verbose = FALSE)$is_zero_inflated && component != "conditional") {
    component <- "conditional"
  }

  ## TODO remove deprecated later
  if (!missing(summary)) {
    .deprecated_warning("summary", "include_info", verbose)
    include_info <- summary
  }

  # Processing
  if (bootstrap) {
    params <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    params <- .extract_parameters_generic(
      model,
      ci = ci,
      component = component,
      standardize = standardize,
      p_adjust = p_adjust,
      keep_parameters = keep,
      drop_parameters = drop,
      verbose = verbose,
      ...
    )
  }

  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, model, exponentiate)

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    p_adjust = p_adjust,
    include_info = include_info,
    verbose = verbose,
    ...
  )
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
standard_error.zcpglm <- function(model,
                                  component = c("all", "conditional", "zi", "zero_inflated"),
                                  ...) {
  insight::check_if_installed("cplm")

  component <- match.arg(component)
  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients) # nolint
  params <- insight::get_parameters(model)

  tweedie <- .data_frame(
    Parameter = params$Parameter[params$Component == "conditional"],
    SE = as.vector(stats$tweedie[, "Std. Error"]),
    Component = "conditional"
  )

  zero <- .data_frame(
    Parameter = params$Parameter[params$Component == "zero_inflated"],
    SE = as.vector(stats$zero[, "Std. Error"]),
    Component = "zero_inflated"
  )

  out <- .filter_component(rbind(tweedie, zero), component)
  out
}


#' p-values for Models with Zero-Inflation
#'
#' This function attempts to return, or compute, p-values of hurdle and
#' zero-inflated models.
#'
#' @param model A statistical model.
#' @inheritParams p_value
#' @inheritParams simulate_model
#' @inheritParams standard_error
#'
#' @return
#' A data frame with at least two columns: the parameter names and the p-values.
#' Depending on the model, may also include columns for model components etc.
#'
#' @examples
#' if (require("pscl", quietly = TRUE)) {
#'   data("bioChemists")
#'   model <- zeroinfl(art ~ fem + mar + kid5 | kid5 + phd, data = bioChemists)
#'   p_value(model)
#'   p_value(model, component = "zi")
#' }
#' @export
p_value.zcpglm <- function(model,
                           component = c("all", "conditional", "zi", "zero_inflated"),
                           ...) {
  insight::check_if_installed("cplm")

  component <- match.arg(component)
  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients) # nolint
  params <- insight::get_parameters(model)

  tweedie <- .data_frame(
    Parameter = params$Parameter[params$Component == "conditional"],
    p = as.vector(stats$tweedie[, "Pr(>|z|)"]),
    Component = "conditional"
  )

  zero <- .data_frame(
    Parameter = params$Parameter[params$Component == "zero_inflated"],
    p = as.vector(stats$zero[, "Pr(>|z|)"]),
    Component = "zero_inflated"
  )

  out <- .filter_component(rbind(tweedie, zero), component)
  out
}




########## .bcpglm ---------------

#' @export
model_parameters.bcplm <- model_parameters.bayesQR


#' @export
p_value.bcplm <- p_value.brmsfit




########## .cpglm ---------------


#' @export
p_value.cpglm <- function(model, ...) {
  insight::check_if_installed("cplm")

  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients) # nolint
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, "Pr(>|t|)"])
  )
}


#' @export
standard_error.cpglm <- function(model, ...) {
  insight::check_if_installed("cplm")

  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients) # nolint
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. Error"])
  )
}





########## .cpglmm ---------------


#' @rdname model_parameters.merMod
#' @export
model_parameters.cpglmm <- function(model,
                                    ci = 0.95,
                                    ci_method = NULL,
                                    ci_random = NULL,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    standardize = NULL,
                                    effects = "all",
                                    group_level = FALSE,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    include_sigma = FALSE,
                                    keep = NULL,
                                    drop = NULL,
                                    verbose = TRUE,
                                    ...) {
  # p-values, CI and se might be based on different df-methods
  ci_method <- .check_df_method(ci_method)
  effects <- match.arg(effects, choices = c("fixed", "random", "all"))

  # standardize only works for fixed effects...
  if (!is.null(standardize) && standardize != "refit") {
    if (!missing(effects) && effects != "fixed" && verbose) {
      insight::format_alert("Standardizing coefficients only works for fixed effects of the mixed model.")
    }
    effects <- "fixed"
  }

  params <- .mixed_model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    effects = effects,
    p_adjust = p_adjust,
    group_level = group_level,
    ci_method = ci_method,
    include_sigma = include_sigma,
    ci_random = ci_random,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", "data.frame")

  params
}


#' @export
p_value.cpglmm <- function(model, method = "wald", ...) {
  p_value.default(model, method = method, ...)
}


#' @export
standard_error.cpglmm <- function(model, ...) {
  insight::check_if_installed("cplm")

  stats <- cplm::summary(model)$coefs
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. Error"])
  )
}




# tools --------------------

.check_df_method <- function(df_method) {
  if (!is.null(df_method)) {
    df_method <- tolower(df_method)
    if (df_method %in% c("satterthwaite", "kenward", "kr")) {
      insight::format_alert("Satterthwaite or Kenward-Rogers approximation of degrees of freedom is only available for linear mixed models.")
      df_method <- "wald"
    }
    df_method <- match.arg(df_method, choices = c("wald", "normal", "residual", "ml1", "betwithin", "profile", "boot", "uniroot"))
  }
  df_method
}
