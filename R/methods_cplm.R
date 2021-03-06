# classes: .cpglm, .bcpglm, .zcpglm, .cpglmm


########## .zcpglm ---------------


#' @title Parameters from Zero-Inflated Models
#' @name model_parameters.zcpglm
#'
#' @description Parameters from zero-inflated models (from packages like \pkg{pscl},
#'   \pkg{cplm} or \pkg{countreg}).
#'
#' @param model A model with zero-inflation component.
#' @inheritParams model_parameters.default
#' @inheritParams simulate_model
#'
#' @seealso \code{\link[insight:standardize_names]{standardize_names()}} to rename
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
                                    ci = .95,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    component = c("all", "conditional", "zi", "zero_inflated"),
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    robust = FALSE,
                                    p_adjust = NULL,
                                    verbose = TRUE,
                                    ...) {
  component <- match.arg(component)

  # fix argument, if model has no zi-part
  if (!insight::model_info(model)$is_zero_inflated && component != "conditional") {
    component <- "conditional"
  }


  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_generic(
      model,
      ci = ci,
      component = component,
      standardize = standardize,
      robust = robust,
      p_adjust = p_adjust,
      ...
    )
  }


  if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
    parameters <- .exponentiate_parameters(parameters, model, exponentiate)
  }

  parameters <- .add_model_parameters_attributes(
    parameters,
    model,
    ci,
    exponentiate,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )
  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}


#' @importFrom utils capture.output
#' @export
standard_error.zcpglm <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  component <- match.arg(component)
  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
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
#' This function attempts to return, or compute, p-values of hurdle and zero-inflated models.
#'
#' @param model A statistical model.
#' @inheritParams p_value
#' @inheritParams simulate_model
#' @inheritParams standard_error
#' @inheritParams ci.merMod
#'
#' @return A data frame with at least two columns: the parameter names and the p-values. Depending on the model, may also include columns for model components etc.
#'
#' @examples
#' if (require("pscl")) {
#'   data("bioChemists")
#'   model <- zeroinfl(art ~ fem + mar + kid5 | kid5 + phd, data = bioChemists)
#'   p_value(model)
#'   p_value(model, component = "zi")
#' }
#' @importFrom utils capture.output
#' @export
p_value.zcpglm <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  component <- match.arg(component)
  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
  params <- get_parameters(model)

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


#' @importFrom utils capture.output
#' @export
p_value.cpglm <- function(model, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, "Pr(>|t|)"])
  )
}


#' @importFrom utils capture.output
#' @export
standard_error.cpglm <- function(model, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. Error"])
  )
}


#' @export
ci.cpglm <- ci.tobit




########## .cpglmm ---------------


#' @rdname model_parameters.merMod
#' @export
model_parameters.cpglmm <- function(model,
                                    ci = .95,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    standardize = NULL,
                                    effects = "fixed",
                                    group_level = FALSE,
                                    exponentiate = FALSE,
                                    details = FALSE,
                                    df_method = NULL,
                                    p_adjust = NULL,
                                    verbose = TRUE,
                                    ...) {

  # p-values, CI and se might be based on different df-methods
  df_method <- .check_df_method(df_method)
  effects <- match.arg(effects, choices = c("fixed", "random", "all"))

  params <- .mixed_model_parameters_generic(
    model = model, ci = ci, bootstrap = bootstrap, iterations = iterations,
    merge_by = "Parameter", standardize = standardize,
    exponentiate = exponentiate, effects = effects, robust = FALSE,
    p_adjust = p_adjust, group_level = group_level, df_method = df_method, ...
  )

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", "data.frame")

  if (isTRUE(details)) {
    attr(params, "details") <- .randomeffects_summary(model)
    if (verbose) {
      message("Argument 'details' is deprecated. Please use 'group_level'.")
    }
  }

  params
}


#' @rdname p_value.lmerMod
#' @export
p_value.cpglmm <- function(model, method = "wald", ...) {
  method <- match.arg(tolower(method), c("wald", "betwithin", "ml1"))
  if (method == "wald") {
    dof <- Inf
  } else if (method == "ml1") {
    dof <- dof_ml1(model)
  } else {
    dof <- dof_betwithin(model)
  }
  p_value_wald(model, dof, ...)
}


#' @export
ci.cpglmm <- ci.tobit


#' @export
standard_error.cpglmm <- function(model, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

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
      warning("Satterthwaite or Kenward-Rogers approximation of degrees of freedom is only available for linear mixed models.", call. = FALSE)
      df_method <- "wald"
    }
    df_method <- match.arg(df_method, choices = c("wald", "ml1", "betwithin", "profile", "boot", "uniroot"))
  }
  df_method
}
