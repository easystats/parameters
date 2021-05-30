
#' @rdname model_parameters.merMod
#' @export
model_parameters.mixor <- function(model,
                                   ci = .95,
                                   effects = "all",
                                   bootstrap = FALSE,
                                   iterations = 1000,
                                   standardize = NULL,
                                   exponentiate = FALSE,
                                   verbose = TRUE,
                                   ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))

  # standardize only works for fixed effects...
  if (!is.null(standardize)) {
    if (!missing(effects) && effects != "fixed" && verbose) {
      warning(insight::format_message("Standardizing coefficients only works for fixed effects of the mixed model."), call. = FALSE)
    }
    effects <- "fixed"
  }

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


#' @rdname ci.merMod
#' @export
ci.mixor <- function(x, ci = .95, effects = "all", ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  ci_wald(model = x, ci = ci, dof = Inf, effects = effects, robust = FALSE, ...)
}


#' @rdname standard_error
#' @export
standard_error.mixor <- function(model, effects = "all", ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  stats <- model$Model[, "Std. Error"]
  parms <- insight::get_parameters(model, effects = effects)

  .data_frame(
    Parameter = parms$Parameter,
    SE = stats[parms$Parameter],
    Effects = parms$Effects
  )
}


#' @rdname p_value.lmerMod
#' @export
p_value.mixor <- function(model, effects = "all", ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  stats <- model$Model[, "P(>|z|)"]
  parms <- insight::get_parameters(model, effects = effects)

  .data_frame(
    Parameter = parms$Parameter,
    p = stats[parms$Parameter],
    Effects = parms$Effects
  )
}


#' @export
simulate_model.mixor <- function(model, iterations = 1000, effects = "all", ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  out <- .simulate_model(model, iterations, component = "conditional", effects = effects)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  out
}
